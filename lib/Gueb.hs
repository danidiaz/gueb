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
import Control.Monad
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Trans.Coiter
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
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
    let jobs     = Jobs (fmap (Executions mempty) plan)
        idstream = Data.Text.pack . show <$> unfold (succ . runIdentity) (pure 0)
    tvar <- atomically (newTMVar (idstream,jobs))
    pure (makeHandlersFromRef tvar)

maybeE :: Monad m => e -> Maybe a -> ExceptT e m a   
maybeE e = maybe (throwE e) pure 

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: TMVar (Coiter Text,Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (query id)
    :<|> (\jobid        -> query (jobs . ix jobid))
    :<|> (\jobid execid -> query (jobs . ix jobid . executions . ix execid))
    :<|> (\jobid -> command (\(advance -> (counter,root)) -> do
             unless (not (has (jobs . folded . executions . folded . bloh . _Right) root)) 
                    (throwE err409) -- conflict, some job already running
             ex <- maybeE err404 -- not found
                          (root ^. jobs . at jobid)
             let Executions {executable} = ex
                 Job {scriptPath} = executable
                 executionId = extract counter
             a <- liftIO (async (execute (piped (proc scriptPath [])) (pure ())))
             let newExecution = Execution {blah="started",_bloh=Right a}
                 ex' = ex & executions . at executionId .~ Just newExecution
                 root' = root & jobs . at jobid .~ Just ex'
                 linkUri = show (safeLink (Proxy :: Proxy JobsAPI) (Proxy :: Proxy ExecutionEndpoint) jobid executionId)
             return ((counter,root'),addHeader linkUri (Page (Created linkUri)))))
    where
    readState_ = void . extract <$> liftIO (atomically (readTMVar ref))
    putState   = \j -> liftIO (atomically (putTMVar ref j))
    query somelens = do root <- readState_ 
                        Page <$> maybeE err404
                                        (root ^? somelens)
    command  = \f -> do s <- liftIO (atomically (takeTMVar ref))
                        catchE (do (newState,result) <- f s 
                                   putState newState
                                   return result)
                               (\e -> do putState s
                                         throwE e)
    advance = first (runIdentity . unwrap)
