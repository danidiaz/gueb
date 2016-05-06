{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language OverloadedStrings #-}

module Gueb.Types.API where

import Data.Text (Text,pack)
import Data.Map.Strict
import Data.Proxy
import Control.Lens
import Data.Aeson

import GHC.Generics

import Lucid
import Servant.API
import Servant.HTML.Lucid

type JobId = Text

type ExecutionId = Text

-- http://haskell-servant.readthedocs.org/en/stable/tutorial/ApiType.html
type JobsAPI = "jobs" :> Get '[HTML,JSON] (Page (Jobs ())) 
          :<|> JobEndpoint
          :<|> ExecutionEndpoint
          :<|> "jobs" :> Capture "jobid" ExecutionId :> PostCreated '[HTML,JSON] (Headers '[Header "Location" String] (Page Created))

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

type JobEndpoint = 
           "jobs" :> Capture "jobid" JobId :> Get '[HTML,JSON] (Page (Executions Job ()))

jobEndpoint :: Proxy JobEndpoint
jobEndpoint = Proxy

type ExecutionEndpoint = 
           "jobs" :> Capture "jobid" JobId :> "executions" :> Capture "execid" ExecutionId :> Get '[HTML,JSON] (Page (Execution ()))

executionEndpoint :: Proxy ExecutionEndpoint
executionEndpoint = Proxy

newtype Jobs async = Jobs { _jobs :: Map JobId (Executions Job async) } deriving (Show,Generic,ToJSON,Functor)

jobs :: Lens' (Jobs async) (Map Text (Executions Job async)) 
jobs = lens _jobs (\r v -> r { _jobs = v }) 

instance ToHtml (Jobs ()) where
    toHtml js = div_ $ do
        div_ $ do _ <- itraverse tf (_jobs js)
                  pure ()
            where
            tf i v = div_ $ do p_ $ toHtml i
                               div_ $ do form_ [ action_ (pack ('/':show (safeLink jobsAPI jobEndpoint i)))
                                               , method_ "POST"
                                               ]
                                               $ do input_ [ type_ "submit", value_ "Start job"]
                               toHtml v
    toHtmlRaw = toHtml

instance ToHtml (Page (Jobs ())) where
    toHtml = pageWithTitle "Jobs"
    toHtmlRaw = toHtml

data Executions script async = Executions 
    {
        _executions     :: Map ExecutionId (Execution async)
    ,   _executable     :: script 
    } deriving (Show,Generic,ToJSON,Functor)

executions :: Lens' (Executions script async) (Map Text (Execution async))
executions = lens _executions (\r v -> r { _executions = v }) 

executable :: Lens' (Executions script async) script
executable = lens _executable (\r v -> r { _executable = v }) 

instance ToHtml a => ToHtml (Executions a ()) where
    toHtml x = div_ $ do
        div_ $ toHtml (_executable x)
        div_ $ do _ <- itraverse tf (_executions x)
                  pure ()
            where
            tf i v = div_ $ do p_ $ toHtml i
                               toHtml v
    toHtmlRaw = toHtml

instance ToHtml a => ToHtml (Page (Executions a ())) where
    toHtml    = pageWithTitle "Executions"
    toHtmlRaw = toHtml  

data Execution a = Execution
    {
        blah :: Text
    ,   _bloh :: Either Text a
    } deriving (Show,Generic,ToJSON,Functor)

bloh :: Lens' (Execution async) (Either Text async)
bloh = lens _bloh (\r v -> r { _bloh = v }) 

instance ToHtml (Execution ()) where
    toHtml c = div_ $ do
        p_ $ do "Execution started at: "
                toHtml (blah c)
        case _bloh c of
            Left start -> p_ $ do "Execution finished at: "
                                  toHtml start
            Right ()   -> p_ $ do "Execution still ongoing"
    toHtmlRaw = toHtml

instance ToHtml (Page (Execution ())) where
    toHtml    = pageWithTitle "Execution"
    toHtmlRaw = toHtml

data Job = Job 
    {
        scriptPath :: FilePath
    } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Job where
    toHtml c = div_ $ p_ $ do
        "Script path: "
        toHtml (scriptPath c)
    toHtmlRaw = toHtml

instance ToHtml (Page Job) where
    toHtml    = pageWithTitle "Job"
    toHtmlRaw = toHtml
    
newtype Created = Created { getLink :: String } deriving (Show,Generic,ToJSON)

instance ToHtml Created where
    toHtml (Created link) = div_ $ p_ $ do
        "Resource created at: "
        a_ [href_ (pack link)] (toHtml link)
    toHtmlRaw = toHtml

instance ToHtml (Page Created) where
    toHtml    = pageWithTitle "Resource created"
    toHtmlRaw = toHtml

newtype Page a = Page { getContent :: a } deriving (Show,Generic,ToJSON)

pageWithTitle :: (Monad m, ToHtml contents) => Text -> Page contents -> HtmlT m ()
pageWithTitle title (Page contents) = html_ $ do
    head_ (title_ (toHtml title))
    body_ (toHtml contents)


