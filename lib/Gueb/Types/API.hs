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
import Control.Monad (unless)
import Data.Aeson
import Data.Time

import GHC.Generics

import Lucid
import Lucid.Bootstrap

import Servant.API
import Servant.HTML.Lucid

type JobId = Text

type ExecutionId = Text

-- http://haskell-servant.readthedocs.org/en/stable/tutorial/ApiType.html
type JobsAPI = JobsEndpoint
          :<|> JobEndpoint
          :<|> Capture "jobid" JobId :> PostCreated '[HTML,JSON] (Headers '[Header "Location" String] (Page Created))
          :<|> ExecutionEndpoint
          :<|> Capture "jobid" JobId :> "executions" :> Capture "execid" ExecutionId :> Delete '[JSON] (Headers '[] (Page Deleted))
          
type JobsEndpoint = 
           Get '[HTML,JSON] (Page (Jobs Links ())) 

type JobEndpoint = 
           Capture "jobid" JobId :> Get '[HTML,JSON] (Page (Executions Job Links ()))

type ExecutionEndpoint = 
           Capture "jobid" JobId :> "executions" :> Capture "execid" ExecutionId :> Get '[HTML,JSON] (Page (Execution Links ()))

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

-------------------------------------------------------------------------------

newtype Jobs link async = Jobs { _jobs :: Map JobId (Executions Job link async) } deriving (Show,Functor,Generic,ToJSON)

jobs :: Lens' (Jobs link async) (Map Text (Executions Job link async)) 
jobs = lens _jobs (\r v -> r { _jobs = v }) 

instance ToHtml (Jobs Links ()) where
    toHtml js = div_ $ do
        let runningJobs = executions . traversed . currentState . _Right
            tf :: Monad m => Text -> Executions script Links async -> HtmlT m ()
            tf i v = li_ [class_ "list-group-item"] $ do a_ [href_ (this (executionsView v))] (toHtml i)
                                                         unless (not (has runningJobs v)) 
                                                                (span_ [class_ "badge"] "running")
        ul_ [class_ "list-group"] $ do _ <- itraverse tf (_jobs js)
                                       pure ()
    toHtmlRaw = toHtml

instance ToHtml (Page (Jobs Links ())) where
    toHtml = pageWithTitle "Jobs" Nothing
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

data Executions script link async = Executions 
    {
        executionsView :: link
    ,   _executions     :: Map ExecutionId (Execution link async)
    ,   _executable     :: script 
    } deriving (Show,Functor,Generic,ToJSON)

executions :: Lens' (Executions script link async) (Map Text (Execution link async))
executions = lens _executions (\r v -> r { _executions = v }) 

executable :: Lens' (Executions script link async) script
executable = lens _executable (\r v -> r { _executable = v }) 

instance ToHtml a => ToHtml (Executions a Links ()) where
    toHtml x = div_ $ do
        div_ $ toHtml (_executable x)
        div_ $ do form_ [ action_ (this (executionsView x))
                        , method_ "POST"
                        , role_ "form"
                        ]
                        $ div_ [class_ "form-group"] $ input_ [ class_ "form-control", type_ "submit", value_ "Start job"]
        let tf :: Monad m => Text -> Execution Links () -> HtmlT m ()
            tf i v = li_ [class_ "list-group-item"] $ do p_ $ a_ [href_ (this (executionView v))] (toHtml i)
                                                         toHtml v
        div_ $ ul_ [class_ "list-group"] $ do _ <- itraverse tf (_executions x)
                                              pure ()
    toHtmlRaw = toHtml

instance ToHtml a => ToHtml (Page (Executions a Links ())) where
    toHtml p@(Page x) = pageWithTitle "Executions" (Just (upwards (executionsView x))) p
    toHtmlRaw = toHtml  

-------------------------------------------------------------------------------

data Execution link a = Execution
    {
        executionView :: link
    ,   startTime :: UTCTime
    ,   _currentState :: Either UTCTime a
    } deriving (Show,Functor,Generic,ToJSON)

currentState :: Lens' (Execution link async) (Either UTCTime async)
currentState = lens _currentState (\r v -> r { _currentState = v }) 

instance ToHtml (Execution Links ()) where
    toHtml c = div_ $ do
        p_ $ do "Execution started at: "
                toHtml (formatTime defaultTimeLocale "%T" (startTime c))
        case _currentState c of
            Left endTime -> p_ $ do "Execution finished at: "
                                    toHtml (formatTime defaultTimeLocale "%T" endTime)
            Right () -> p_ $ do "Execution still ongoing"
    toHtmlRaw = toHtml

instance ToHtml (Page (Execution Links ())) where
    toHtml p@(Page x) = pageWithTitle "Execution" (Just (upwards (executionView x))) p
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

data Job = Job 
    {
        scriptPath :: FilePath
    } deriving (Show,Generic,FromJSON,ToJSON)

instance ToHtml Job where
    toHtml c = div_ $ p_ $ do
        "Script: "
        toHtml (scriptPath c)
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

newtype Created = Created { getLink :: String } deriving (Show,Generic,ToJSON)

instance ToHtml Created where
    toHtml (Created link) = div_ $ p_ $ do
        "Resource created "
        a_ [href_ (pack link)] "here"
        "."
    toHtmlRaw = toHtml

instance ToHtml (Page Created) where
    toHtml    = pageWithTitle "Resource created" Nothing
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

data Deleted = Deleted deriving (Show,Generic,ToJSON)

-------------------------------------------------------------------------------

newtype Page a = Page { getContent :: a } deriving (Show,Generic,ToJSON)

-- http://www.w3schools.com/bootstrap/bootstrap_get_started.asp
-- http://www.tutorialspoint.com/bootstrap/bootstrap_forms.htm
pageWithTitle :: (Monad m, ToHtml contents) => Text -> Maybe Text -> Page contents -> HtmlT m ()
pageWithTitle title murl (Page contents) = html_ $ do
    head_ (do title_ (toHtml title)
              meta_ [charset_ "utf-8"]
              meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
              link_ [rel_ "stylesheet", href_ "http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"]
              script_ [src_ ("https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js")] (""::Text)
              script_ [src_ ("http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js")] (""::Text))
    body_ (do container_ $ do div_ $ toHtml contents
                              foldMap (\url -> div_ $ a_ [href_ url] (toHtml ("Up"::Text))) murl)

-------------------------------------------------------------------------------

data Links = Links
           {
             upwards :: Text
           , this :: Text
           } deriving (Show,Generic,ToJSON)

