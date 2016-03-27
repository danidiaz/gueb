{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}

module Gueb.Types.API where

import Data.Text (Text)
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.Aeson

import GHC.Generics

import Lucid
import Servant.API
import Servant.HTML.Lucid

-- http://haskell-servant.readthedocs.org/en/stable/tutorial/ApiType.html
type JobsAPI = "jobs" :> Get '[JSON,HTML] Jobs
          :<|> "jobs" :> Capture "jobid" Text :> Get '[JSON,HTML] Job
          :<|> "jobs" :> Capture "jobid" Text :> PostCreated '[JSON,HTML] (Headers '[Header "Location" Text] Created)
          :<|> "jobs" :> Capture "jobid" Text :> "executions" :> Capture "execid" Text :> Get '[JSON,HTML] Execution

newtype Jobs = Jobs { getJobs :: Map Text Job } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Jobs where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

data Job = Job 
    {
        scriptPath :: FilePath
    } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Job where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

data Execution = Execution
    {
        blah :: Text
    } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Execution where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

newtype Created = Created { getLink :: String } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Created where
    toHtml _ = return ()
    toHtmlRaw _ = return ()

