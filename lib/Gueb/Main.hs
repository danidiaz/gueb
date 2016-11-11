{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language NamedFieldPuns #-}

module Gueb.Main (
        makeMain
    ) where

import Data.Monoid
import Data.ByteString as Bytes
import Data.Aeson
import Control.Exception
import Options.Applicative

import Servant

import Network.Wai.Handler.Warp

import Gueb (makeHandlers)
import Gueb.Types.API

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
    handlers <- makeHandlers plan
    -- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
    let app1 :: Application
        app1 = serve jobsAPI handlers
    run port app1

