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

import Data.ByteString as Bytes
import Data.Aeson
import Control.Exception
import Control.Applicative
import Options.Applicative

import Servant

import Network.Wai
import Network.Wai.Handler.Warp

import Gueb (noJobs)
import Gueb.Types.API

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

data Args = Args
    {
        port :: Int
    ,   planPath :: FilePath
    } deriving (Show,Eq)

readJSON :: FromJSON a => FilePath -> IO a
readJSON path = do
    bytes <- Bytes.readFile path
    case eitherDecodeStrict' bytes of 
        Right v -> pure v
        Left  err  -> throwIO (userError err)

parserInfo :: ParserInfo Args
parserInfo = 
    info (helper <*> parser) infoMod
  where
    parser = 
        Args <$> (option auto (help "port" <> long "port" <> metavar "PORT" <> value 8000))
             <*> (strArgument (help "json plan file" <> metavar "DICT"))
    infoMod = 
        fullDesc <> header "program description" 

makeMain :: IO ()
makeMain = do
    Args port_ planPath_ <- execParser parserInfo
    jobs :: Jobs <- readJSON planPath_
    -- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
    let server1 :: Server JobsAPI
        server1 = return jobs
        app1 :: Application
        app1 = serve jobsAPI server1
    run port_ app1

