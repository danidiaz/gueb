{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}

module Gueb.Main (
        makeMain
    ) where

import Control.Applicative
import qualified Options.Applicative as O

import Servant

import Network.Wai
import Network.Wai.Handler.Warp

import Gueb.Types.API

data Plan = Plan
    {
        port :: Int
    } deriving (Show,Eq)

-- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
server1 :: Server JobsAPI
server1 = return (Jobs [])

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

app1 :: Application
app1 = serve jobsAPI server1

makeMain :: IO ()
makeMain = run 8000 app1

