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
import Data.Functor
import Data.Bifunctor
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Servant

import Gueb.Types
import Gueb.Types.API

import System.Process.Streaming


noJobs :: Jobs ()
noJobs = Jobs mempty

makeHandlers :: Plan -> IO (Server JobsAPI)
makeHandlers plan = do
    let theJobs  = Jobs (fmap (Executions mempty) plan)
        idstream = Data.Text.pack . show <$> coiter (Identity . succ) (0::Int)
    tvar <- newMVar (idstream,theJobs)
    pure (makeHandlersFromRef tvar)

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: MVar (Unending ExecutionId,Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (query id)
    :<|> (\jobid        -> query (jobs . ix jobid))
    :<|> (\jobid execid -> query (jobs . ix jobid . executions . ix execid))
    :<|> (\jobid        -> command (startExecution jobid))
    where
    query somelens = do root <- void . extract <$> liftIO (readMVar ref)
                        Page <$> maybeE err404
                                        (preview somelens root)
    command handler = do oldState <- liftIO (takeMVar ref)
                         catchE (do (newState,result) <- handler oldState deferrer
                                    liftIO (do putMVar ref newState
                                               return (Page <$> result)))
                                (\e -> do liftIO (putMVar ref oldState)
                                          throwE e)
    deferrer action post = 
        async (do (do action
                      notify post) 
                      `onException` notify post)
    notify change = do
        _ <- withMVar ref (\s -> evaluate (change s))
        pure ()

maybeE :: Monad m => e -> Maybe a -> ExceptT e m a   
maybeE e = maybe (throwE e) pure 

startExecution :: JobId
               -> GlobalState
               -> (IO () -> (GlobalState -> GlobalState) -> IO (Async ()))
               -> ExceptT ServantErr IO (GlobalState, Headers '[Header "Location" String] Created)
startExecution jobId (advance -> (executionId,root)) deferrer = do
    unless (not (has (_2 . jobs . folded . executions . folded . bloh . _Right) root)) 
           (throwE err409) -- conflict, some job already running
    let Traversal ixJob = Traversal (_2 . jobs . ix jobId)
    Job {scriptPath} <- maybeE err404 -- job not found
                               (preview (ixJob . executable) root)
    let Traversal atExecution = Traversal (ixJob . executions . at executionId) -- to add
    launched <- liftIO (launch scriptPath atExecution)
    let root' = set atExecution (Just launched) root
        linkUri = show (safeLink jobsAPI executionEndpoint jobId executionId)
    return (root',addHeader linkUri (Created linkUri))
    where
        launch scriptPath atExecution = do 
            pid <- deferrer (execute (piped (proc scriptPath [])) (pure ()))
                            (set (atExecution . _Just . bloh) (Left "foo"))
            pure (Execution {blah="started",_bloh=Right pid})
        
{-| World's most obscure i++		

-}
advance :: GlobalState -> (ExecutionId,GlobalState)
advance = first extract . duplicate . first (runIdentity . unwrap)


