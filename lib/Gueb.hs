{-# language NamedFieldPuns #-}
{-# language DataKinds #-}
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
import Data.Bifunctor (first)
import Data.Time
import Control.Lens
import Control.Exception
import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree (coiter,unwrap)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Servant

import Gueb.Types
import Gueb.Types.API

import System.Process.Streaming

noJobs :: Jobs () ()
noJobs = Jobs mempty

makeHandlers :: Plan -> IO (Server JobsAPI)
makeHandlers plan = do
    let theJobs  = Jobs (fmap (Executions () mempty) plan)
        idstream = Data.Text.pack . show <$> coiter (Identity . succ) (0::Int)
    tvar <- newMVar (idstream,theJobs)
    pure (makeHandlersFromRef tvar)

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: MVar (Unending ExecutionId,Jobs () (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (query id)
    :<|> (\jobid        -> query (jobs . ix jobid))
    :<|> (\jobid        -> command (startExecution jobid))
    :<|> (\jobid execid -> query (jobs . ix jobid . executions . ix execid))
    :<|> (\jobid execid -> command (cancelExecution jobid execid))
    where
    query somelens = do root <- void . addUri . extract <$> liftIO (readMVar ref)
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
           ,let linkUri = mkExecutionUri jobId executionId
            in  addHeader linkUri (Created linkUri))
    where
        launch scriptPath atExecution = do 
            pid <- deferrer (execute (piped (proc scriptPath [])) (pure ()))
                            (do t <- getCurrentTime 
                                pure (set (atExecution . _Just . currentState) (Left t)))
            t <- getCurrentTime
            pure (Execution {executionView = (), startTime=t, _currentState=Right pid})
        
cancelExecution :: JobId
                -> ExecutionId
                -> GlobalState
                -> (IO () -> IO (GlobalState -> GlobalState) -> IO (Async ()))
                -> ExceptT ServantErr IO (GlobalState, Headers '[] Deleted)
cancelExecution jobId executionId (counter,root) _ = do
    return ((counter,root), Headers Deleted HNil) 

{-| World's most obscure i++		

-}
advance :: GlobalState -> (ExecutionId,GlobalState)
advance = first extract . duplicate . first (runIdentity . unwrap)

addUri :: Jobs () a -> Jobs Links a
addUri (Jobs j) = Jobs (iover itraversed addUriExecutions j)
    where
    addUriExecutions i xs = 
        let topURI = pack ('/' : show (safeLink jobsAPI (Proxy::Proxy JobsEndpoint)))
            jobURI = pack ('/' : show (safeLink jobsAPI (Proxy::Proxy JobEndpoint) i))
        in  xs { executionsView = Links topURI jobURI,
                 _executions = iover itraversed (addUriExecution jobURI i) (_executions xs)}
    addUriExecution upwards i i' x = 
        x { executionView = Links upwards (pack (mkExecutionUri i i')) }

mkExecutionUri :: Text -> Text -> String
mkExecutionUri i i' = '/': show (safeLink jobsAPI (Proxy::Proxy ExecutionEndpoint) i i')

