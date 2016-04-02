module Gueb (
        noJobs
    ,   makeServer
    ) where

import Control.Lens
import Data.Monoid
import Control.Monad.Trans.Except

import Servant

import Gueb.Types
import Gueb.Types.API

noJobs :: Jobs
noJobs = Jobs mempty

makeServer :: Plan -> Server JobsAPI
makeServer plan =
    -- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
         return (Page jobs)
    :<|> undefined
    :<|> (\jobid -> case jobs ^. to getJobs . at jobid of
               Nothing  -> throwE err404
               Just jobex -> return (Page jobex))
    :<|> (\jobid execid -> case jobs ^.. to getJobs . at jobid . folded . executions . at execid . folded of
               []  -> throwE err404
               exe : _ -> return (Page exe))
    where
    jobs = (Jobs . fmap (Executions 0 mempty)) plan

