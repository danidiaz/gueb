module Gueb (
        noJobs
    ,   makeHandlers
    ) where

import Control.Lens
import Data.Monoid
import Data.Functor
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Servant

import Gueb.Types
import Gueb.Types.API

noJobs :: Jobs ()
noJobs = Jobs mempty

makeHandlers :: Plan -> IO (Server JobsAPI)
makeHandlers plan = do
    let jobs = Jobs (fmap (Executions 0 mempty) plan)
    tvar <- atomically (newTVar jobs)
    pure (makeHandlersFromRef tvar)
    
-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
makeHandlersFromRef :: TVar (Jobs (Async ())) -> Server JobsAPI 
makeHandlersFromRef ref =   
         (do 
             jobs <- readState_ 
             pure (Page jobs))
    :<|> (\jobid -> do
             jobs <- readState_ 
             undefined)
    :<|> (\jobid -> do
             jobs <- readState_ 
             case jobs ^. to getJobs . at jobid of
                 Nothing  -> throwE err404
                 Just jobex -> return (Page jobex))
    :<|> (\jobid execid -> do
             jobs <- readState_
             case jobs ^.. to getJobs . at jobid . folded . executions . at execid . folded of
                 []  -> throwE err404
                 exe : _ -> return (Page exe))
    where
    readState = liftIO (atomically (readTVar ref))
    readState_ = void <$> readState
