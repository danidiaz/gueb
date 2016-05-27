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
import Data.Map as Map
import Data.Aeson
import Data.Functor
import Data.Bifunctor
import Data.Time
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


noJobs :: Jobs Identity ()
noJobs = Jobs mempty

makeHandlers :: Plan -> IO (Server JobsAPI)
makeHandlers plan = do
    let theJobs  = Jobs (fmap (Identity . Executions mempty) plan)
        idstream = Data.Text.pack . show <$> coiter (Identity . succ) (0::Int)
    tvar <- newMVar (idstream,theJobs)
    pure (makeHandlersFromRef tvar)

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: MVar (Unending ExecutionId,Jobs Identity (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (query id)
         --(query (to (addUri "/")))
    :<|> (\jobid        -> query (jobs . ix jobid))
    :<|> (\jobid execid -> query (jobs . ix jobid . executions . ix execid))
    :<|> (\jobid        -> command (startExecution jobid))
    where
    query somelens = do root <- void . extract <$> liftIO (readMVar ref)
                        Page <$> noteT err404
                                       (preview somelens root)
    command handler = do oldState <- liftIO (takeMVar ref)
                         catchE (do (newState,result) <- handler oldState deferrer
                                    liftIO (do putMVar ref newState
                                               return (Page <$> result)))
                                (\e -> do liftIO (putMVar ref oldState)
                                          throwE e)
    deferrer action post = 
        async (do (do _ <- action
                      change <- post
                      notify change) 
                      `onException` (do change <- post
                                        notify change))
    notify change = modifyMVar_ ref (\s -> evaluate (change s))

noteT :: Monad m => e -> Maybe a -> ExceptT e m a   
noteT e = maybe (throwE e) pure 

reason :: ServantErr -> Text -> ServantErr
reason err msg = err { errBody = jsonError msg } 
    where
        jsonError = encode . Map.singleton ("result"::Text)

startExecution :: JobId
               -> GlobalState
               -> (IO () -> IO (GlobalState -> GlobalState) -> IO (Async ()))
               -> ExceptT ServantErr IO (GlobalState, Headers '[Header "Location" String] Created)
startExecution jobId (advance -> (executionId,root)) deferrer = do
    let Traversal runningJobs = Traversal (_2 . jobs . traversed . executions . traversed . currentState . _Right)
        Traversal ixJob       = Traversal (_2 . jobs . ix jobId)
        Traversal atExecution = Traversal (ixJob . executions . at executionId)
    unless (not (has runningJobs root)) 
           (throwE (err409 `reason` "Job already running")) 
    Job {scriptPath} <- noteT (err404 `reason` "Job not found") 
                              (preview (ixJob . executable) root)
    launched <- liftIO (launch scriptPath atExecution)
    return (set atExecution (Just launched) root
           ,let linkUri = '/' : show (safeLink jobsAPI executionEndpoint jobId executionId)
            in addHeader linkUri (Created linkUri))
    where
        launch scriptPath atExecution = do 
            pid <- deferrer (execute (piped (proc scriptPath [])) (pure ()))
                            (do t <- getCurrentTime 
                                pure (set (atExecution . _Just . currentState) (Left t)))
            t <- getCurrentTime
            pure (Execution {startTime=t, _currentState=Right pid})
        
{-| World's most obscure i++		

-}
advance :: GlobalState -> (ExecutionId,GlobalState)
advance = first extract . duplicate . first (runIdentity . unwrap)

--addUri :: String -> Jobs Identity a -> Jobs Ref a
--addUri base = undefined
--
--addUriExecutions :: String -> Executions x Identity a -> Executions x Ref a
--addUriExecutions base  = undefined

