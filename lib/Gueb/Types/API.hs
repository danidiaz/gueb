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
import Data.Time

import GHC.Generics

import Lucid
import Servant.API
import Servant.HTML.Lucid

type JobId = Text

type ExecutionId = Text

type L = Text
--type Ref = ((,) Text)

-- http://haskell-servant.readthedocs.org/en/stable/tutorial/ApiType.html
type JobsAPI = Get '[HTML,JSON] (Page (Jobs L ())) 
          :<|> JobEndpoint
          :<|> ExecutionEndpoint
          :<|> Capture "jobid" ExecutionId :> PostCreated '[HTML,JSON] (Headers '[Header "Location" String] (Page Created))

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

type JobEndpoint = 
           Capture "jobid" JobId :> Get '[HTML,JSON] (Page (Executions Job L ()))

jobEndpoint :: Proxy JobEndpoint
jobEndpoint = Proxy

type ExecutionEndpoint = 
           Capture "jobid" JobId :> "executions" :> Capture "execid" ExecutionId :> Get '[HTML,JSON] (Page (Execution L ()))

executionEndpoint :: Proxy ExecutionEndpoint
executionEndpoint = Proxy

-------------------------------------------------------------------------------

newtype Jobs link async = Jobs { _jobs :: Map JobId (Executions Job link async) } deriving (Show,ToJSON,Generic,Functor)

jobs :: Lens' (Jobs link async) (Map Text (Executions Job link async)) 
jobs = lens _jobs (\r v -> r { _jobs = v }) 

instance ToHtml (Jobs L ()) where
    toHtml js = div_ $ do
        div_ $ do _ <- itraverse tf (_jobs js)
                  pure ()
        where
        tf i v = div_ $ do p_ $ toHtml i
                           div_ $ do form_ [ action_ (executionsView v)
                                           , method_ "POST"
                                               ]
                                               $ do input_ [ type_ "submit", value_ "Start job"]
                           toHtml v
    toHtmlRaw = toHtml

instance ToHtml (Page (Jobs L ())) where
    toHtml = pageWithTitle "Jobs"
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

data Executions script link async = Executions 
    {
        executionsView :: link
    ,   _executions     :: Map ExecutionId (Execution link async)
    ,   _executable     :: script 
    } deriving (Show,ToJSON,Generic,Functor)

executions :: Lens' (Executions script link async) (Map Text (Execution link async))
executions = lens _executions (\r v -> r { _executions = v }) 

executable :: Lens' (Executions script link async) script
executable = lens _executable (\r v -> r { _executable = v }) 

instance ToHtml a => ToHtml (Executions a L ()) where
    toHtml x = div_ $ do
        div_ $ toHtml (_executable x)
        div_ $ do _ <- itraverse tf (_executions x)
                  pure ()
            where
            tf i v = div_ $ do p_ $ a_ [href_ (executionView v)] (toHtml i)
                               toHtml v
    toHtmlRaw = toHtml

instance ToHtml a => ToHtml (Page (Executions a L ())) where
    toHtml    = pageWithTitle "Executions"
    toHtmlRaw = toHtml  

-------------------------------------------------------------------------------

data Execution link a = Execution
    {
        executionView :: link
    ,   startTime :: UTCTime
    ,   _currentState :: Either UTCTime a
    } deriving (Show,Generic,ToJSON,Functor)

currentState :: Lens' (Execution link async) (Either UTCTime async)
currentState = lens _currentState (\r v -> r { _currentState = v }) 

instance ToHtml (Execution L ()) where
    toHtml c = div_ $ do
        p_ $ do "Execution started at: "
                toHtml (formatTime defaultTimeLocale "%T" (startTime c))
        case _currentState c of
            Left endTime -> p_ $ do "Execution finished at: "
                                    toHtml (formatTime defaultTimeLocale "%T" endTime)
            Right () -> p_ $ do "Execution still ongoing"
    toHtmlRaw = toHtml

instance ToHtml (Page (Execution L ())) where
    toHtml    = pageWithTitle "Execution"
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

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
    
-------------------------------------------------------------------------------

newtype Created = Created { getLink :: String } deriving (Show,Generic,ToJSON)

instance ToHtml Created where
    toHtml (Created link) = div_ $ p_ $ do
        "Resource created at: "
        a_ [href_ (pack link)] (toHtml link)
    toHtmlRaw = toHtml

instance ToHtml (Page Created) where
    toHtml    = pageWithTitle "Resource created"
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------

newtype Page a = Page { getContent :: a } deriving (Show,Generic,ToJSON)

pageWithTitle :: (Monad m, ToHtml contents) => Text -> Page contents -> HtmlT m ()
pageWithTitle title (Page contents) = html_ $ do
    head_ (title_ (toHtml title))
    body_ (toHtml contents)


