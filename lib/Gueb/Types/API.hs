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

data Job = Job 
    {
        scriptPath :: FilePath
    } deriving (Show,Generic,FromJSON,ToJSON)

newtype Jobs = Jobs { getJobs :: Map Text Job } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Jobs where
    toHtml _ = return ()
    toHtmlRaw _ = return ()
