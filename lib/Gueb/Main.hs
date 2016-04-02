{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language NamedFieldPuns #-}
--{-# language RecordWildCards #-}

module Gueb.Main (
        makeMain
    ) where

import Data.ByteString as Bytes
import Data.Aeson
import Control.Lens
import Control.Exception
import Control.Applicative
import Options.Applicative

import Servant

import Network.Wai
import Network.Wai.Handler.Warp

import Gueb (noJobs,makeServer)
import Gueb.Types
import Gueb.Types.API

jobsAPI :: Proxy JobsAPI
jobsAPI = Proxy

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

data Args = Args
    {
        port :: Int
    ,   planPath :: FilePath
    } deriving (Show,Eq)

makeMain :: IO ()
makeMain = do
    Args {port,planPath} <- execParser parserInfo
    plan <- readJSON planPath
    -- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
    let app1 :: Application
        app1 = serve jobsAPI (makeServer plan)
    run port app1

