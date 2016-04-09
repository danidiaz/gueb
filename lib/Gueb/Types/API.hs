{-# language FlexibleInstances #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

module Gueb.Types.API where

import Data.Text (Text)
import Data.Map.Strict
import Control.Lens
import qualified Data.Map.Strict as Map
import Data.Aeson

import GHC.Generics

import Lucid
import Servant.API
import Servant.HTML.Lucid


-- http://haskell-servant.readthedocs.org/en/stable/tutorial/ApiType.html
type JobsAPI = "jobs" :> Get '[JSON,HTML] (Page (Jobs ())) 
          :<|> "jobs" :> Capture "jobid" Text :> PostCreated '[JSON,HTML] (Headers '[Header "Location" String] (Page Created))
          :<|> "jobs" :> Capture "jobid" Text :> Get '[JSON,HTML] (Page (Executions Job ()))
          :<|> "jobs" :> Capture "jobid" Text :> "executions" :> Capture "execid" Text :> Get '[JSON,HTML] (Page (Execution ()))

newtype Page a = Page { getContent :: a } deriving (Show,Generic,ToJSON)

newtype Jobs async = Jobs { _jobs :: Map Text (Executions Job async) } deriving (Show,Generic,ToJSON,Functor)

jobs :: Lens' (Jobs async) (Map Text (Executions Job async)) 
jobs = lens _jobs (\r v -> r { _jobs = v }) 

instance ToHtml (Jobs ()) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

instance ToHtml (Page (Jobs ())) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

data Executions script async = Executions 
    {
        _nextExecutionId :: Int
    ,   _executions     :: Map Text (Execution async)
    ,   executable      :: script 
    } deriving (Show,Generic,ToJSON,Functor)

nextExecutionId :: Lens' (Executions script async) Int
nextExecutionId = lens _nextExecutionId (\r v -> r { _nextExecutionId = v }) 

executions :: Lens' (Executions script async) (Map Text (Execution async))
executions = lens _executions (\r v -> r { _executions = v }) 

instance ToHtml a => ToHtml (Executions a ()) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

instance ToHtml a => ToHtml (Page (Executions a ())) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

data Execution a = Execution
    {
        blah :: Text
    ,   _bloh :: Either Text a
    } deriving (Show,Generic,ToJSON,Functor)

bloh :: Lens' (Execution async) (Either Text async)
bloh = lens _bloh (\r v -> r { _bloh = v }) 

instance ToHtml (Execution ()) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

instance ToHtml (Page (Execution ())) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

data Job = Job 
    {
        scriptPath :: FilePath
    } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Job where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

newtype Created = Created { getLink :: String } deriving (Show,Generic,ToJSON)

instance ToHtml Created where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

instance ToHtml (Page Created) where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

