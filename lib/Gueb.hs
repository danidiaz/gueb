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
    
-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: TMVar (Coiter Text,Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (do 
             root <- readState_ 
             pure (Page root))
    :<|> (\jobid -> do
             pristine@(runCoiter -> (currentId,nextId),root) <- takeState 
             unless (not (has (jobs . folded . executions . folded . bloh . _Right) root)) (do
                 putState pristine
                 throwE err409) -- conflict
             ex <- case root ^. jobs . at jobid of
                 Nothing -> do putState pristine
                               throwE err404 -- not found
                 Just ex  -> return ex
             let Executions {executable} = ex
                 Job {scriptPath} = executable
             a <- liftIO (async (execute (piped (proc scriptPath [])) (pure ())))
             let newExecution = Execution {blah="started",_bloh=Right a}
                 ex' = ex & executions . at currentId .~ Just newExecution
                 root' = root & jobs . at jobid .~ Just ex'
             putState (nextId,root')
             let linkProxy = Proxy :: (Proxy ("jobs" :> Capture "jobid" Text :> "executions" :> Capture "execid" Text :> Get '[JSON,HTML] (Page (Execution ()))))
                 linkUri = show (safeLink (Proxy :: Proxy JobsAPI) linkProxy jobid currentId)
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
    readState_ = void . extract <$> liftIO (atomically (readTMVar ref))
    takeState  = liftIO (atomically (takeTMVar ref))
    putState   = \j -> liftIO (atomically (putTMVar ref j))
