{-# language NamedFieldPuns #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Gueb (
        noJobs
    ,   makeHandlers
    ) where

import Data.Text
import Data.Monoid
import Data.Functor
import Data.Bifunctor
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Trans.Coiter
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Servant
import Servant.HTML.Lucid (HTML)

import Gueb.Types
import Gueb.Types.API

import System.Process.Streaming

noJobs :: Jobs ()
noJobs = Jobs mempty

makeHandlers :: Plan -> IO (Server JobsAPI)
makeHandlers plan = do
    let theJobs  = Jobs (fmap (Executions mempty) plan)
        idstream = Data.Text.pack . show <$> unfold (succ . runIdentity) (pure (0::Int))
    tvar <- newMVar (idstream,theJobs)
    pure (makeHandlersFromRef tvar)

maybeE :: Monad m => e -> Maybe a -> ExceptT e m a   
maybeE e = maybe (throwE e) pure 

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: MVar (Coiter Text,Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (query id)
    :<|> (\jobid        -> query (jobs . ix jobid))
    :<|> (\jobid execid -> query (jobs . ix jobid . executions . ix execid))
    :<|> (\jobid -> command (\(advance -> (counter,root)) -> do
             unless (not (has (jobs . folded . executions . folded . bloh . _Right) root)) 
                    (throwE err409) -- conflict, some job already running
             theJob <- maybeE err404 -- not found
                              (root ^? jobs . ix jobid . executable)
             let executionId = extract counter
             launched <- launch theJob
                                (notifyJobFinished (traversed . jobs . ix jobid . executions . ix executionId . bloh))
             let root' = root & jobs . ix jobid . executions . at executionId .~ Just launched
                 linkUri = show (safeLink (Proxy :: Proxy JobsAPI) (Proxy :: Proxy ExecutionEndpoint) jobid executionId)
             return ((counter,root'),addHeader linkUri (Page (Created linkUri)))))
    where
    readState_ = void . extract <$> liftIO (readMVar ref)
    putState   = \j -> liftIO (putMVar ref j)
    query somelens = do root <- readState_ 
                        Page <$> maybeE err404
                                        (root ^? somelens)
    command  = \f -> do s <- liftIO (takeMVar ref)
                        catchE (do (newState,result) <- f s 
                                   putState newState
                                   return result)
                               (\e -> do putState s
                                         throwE e)
    advance = first (runIdentity . unwrap)
    launch :: Job -> IO () -> ExceptT e IO (Execution (Async ())) 
    launch (Job {scriptPath}) after = do 
        a <- liftIO (async (do
                (do execute (piped (proc scriptPath [])) (pure ())
                    after
                    pure ()) 
                    `onException` after))
        pure (Execution {blah="started",_bloh=Right a})
    notifyJobFinished executionPointer = do
        _ <- withMVar ref (\s -> evaluate (s & executionPointer .~ Left "fff"))
        pure ()
