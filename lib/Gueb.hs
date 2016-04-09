{-# LANGUAGE NamedFieldPuns #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Gueb (
        noJobs
    ,   makeHandlers
    ) where

import Control.Lens
import Data.Text
import Data.Monoid
import Data.Functor
import Control.Monad
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
    let jobs = Jobs (fmap (Executions 0 mempty) plan)
    tvar <- atomically (newTMVar jobs)
    pure (makeHandlersFromRef tvar)
    
-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: TMVar (Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (do 
             root <- readState_ 
             pure (Page root))
    :<|> (\jobid -> do
             root <- takeState 
             unless (not (has (jobs . folded . executions . folded . bloh . _Right) root)) (do
                 putState root
                 throwE err409) -- conflict
             ex <- case root ^. jobs . at jobid of
                 Nothing -> do putState root
                               throwE err404 -- not found
                 Just ex  -> return ex
             let Executions {_nextExecutionId,executable} = ex
                 Job {scriptPath} = executable
                 nextExecutionIdInt = succ _nextExecutionId
                 nextExecutionIdText = Data.Text.pack (show nextExecutionIdInt) 
             a <- liftIO (async (execute (piped (proc scriptPath [])) (pure ())))
             let newExecution = Execution {blah="",_bloh=Right a}
                 ex' = ex & nextExecutionId .~ nextExecutionIdInt
                          & executions . at nextExecutionIdText .~ Just newExecution
                 root' = root & jobs . at jobid .~ Just ex'
             putState root'
             let linkProxy = Proxy :: (Proxy ("jobs" :> Capture "jobid" Text :> "executions" :> Capture "execid" Text :> Get '[JSON,HTML] (Page (Execution ()))))
                 linkUri = show (safeLink (Proxy :: Proxy JobsAPI) linkProxy jobid nextExecutionIdText)
             return (addHeader linkUri (Page (Created linkUri))))
    :<|> (\jobid -> do
             root <- readState_ 
             case root ^. jobs . at jobid of
                 Nothing -> throwE err404
                 Just jobex -> return (Page jobex))
    :<|> (\jobid execid -> do
             root <- readState_
             case root ^.. jobs . at jobid . folded . executions . at execid . folded of
                 []  -> throwE err404
                 exe : _ -> return (Page exe))
    where
    readState = liftIO (atomically (readTMVar ref))
    readState_ = void <$> readState
    takeState = liftIO (atomically (takeTMVar ref))
    putState j = liftIO (atomically (putTMVar ref j))
