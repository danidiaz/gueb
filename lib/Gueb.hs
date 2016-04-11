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
import Control.Lens.Reified
import Control.Exception
import Control.Monad
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Cofree
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Servant
import Servant.HTML.Lucid (HTML)

import Gueb.Types
import Gueb.Types.API

import System.Process.Streaming

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

executionEndpoint :: Proxy ExecutionEndpoint
executionEndpoint = Proxy

noJobs :: Jobs ()
noJobs = Jobs mempty

makeHandlers :: Plan -> IO (Server JobsAPI)
makeHandlers plan = do
    let theJobs  = Jobs (fmap (Executions mempty) plan)
        idstream = Data.Text.pack . show <$> coiter (Identity . succ) (0::Int)
    tvar <- newMVar (idstream,theJobs)
    pure (makeHandlersFromRef tvar)

maybeE :: Monad m => e -> Maybe a -> ExceptT e m a   
maybeE e = maybe (throwE e) pure 

launch :: Job -> IO () -> ExceptT e IO (Execution (Async ())) 
launch (Job {scriptPath}) after = do 
    a <- liftIO (async (do
            (do execute (piped (proc scriptPath [])) (pure ())
                after
                pure ()) 
                `onException` after))
    pure (Execution {blah="started",_bloh=Right a})

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: MVar (Unending Text,Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (query id)
    :<|> (\jobid        -> query (jobs . ix jobid))
    :<|> (\jobid execid -> query (jobs . ix jobid . executions . ix execid))
    :<|> (\jobid -> command (\(advance -> (executionId,root)) -> do
             unless (not (has (_2 . jobs . folded . executions . folded . bloh . _Right) root)) 
                    (throwE err409) -- conflict, some job already running
             let Traversal ixJob = Traversal (_2 . jobs . ix jobid)
             script <- maybeE err404 -- job not found
                              (root ^? ixJob . executable)
             let Traversal atExecution = Traversal (ixJob . executions . at executionId) -- to add
             launched <- launch script
                                (notifyJobFinished (atExecution . _Just . bloh))
             let root' = root & atExecution .~ Just launched
                 linkUri = show (safeLink jobsAPI executionEndpoint jobid executionId)
             return (root',addHeader linkUri ((Created linkUri)))))
    where
    query somelens = do root <- void . extract <$> liftIO (readMVar ref)
                        Page <$> maybeE err404
                                        (root ^? somelens)
    command  = \f -> do oldState <- liftIO (takeMVar ref)
                        catchE (do (newState,result) <- f oldState
                                   liftIO (do putMVar ref newState
                                              return (Page <$> result)))
                               (\e -> do liftIO (putMVar ref oldState)
                                         throwE e)
    advance = first extract . duplicate . first (runIdentity . unwrap)
    notifyJobFinished executionPointer = do
        _ <- withMVar ref (\s -> evaluate (s & executionPointer .~ Left "fff"))
        pure ()
