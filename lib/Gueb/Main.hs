{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language RecordWildCards #-}

module Gueb.Main (
        makeMain
    ) where

import Data.ByteString as Bytes
import Data.Aeson
import Control.Lens
import Control.Exception
import Control.Applicative
import Control.Monad.Trans.Except
import Options.Applicative

import Servant

import Network.Wai
import Network.Wai.Handler.Warp

import Gueb (noJobs)
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

data Args = Args
    {
        port :: Int
    ,   planPath :: FilePath
    } deriving (Show,Eq)

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
    Args {..} <- execParser parserInfo
    jobs@(Jobs jobMap) <- Jobs . fmap (Executions 0 mempty) <$> readJSON planPath
    -- http://haskell-servant.readthedocs.org/en/tutorial/tutorial/Server.html
    let server1 :: Server JobsAPI
        server1 = return (Page jobs)
             :<|> undefined
             :<|> (\jobid -> case jobMap ^. at jobid of
                        Nothing  -> throwE err404
                        Just jobex -> return (Page jobex))
             :<|> (\jobid execid -> case jobMap ^.. at jobid . folded . executions . at execid . folded of
                        []  -> throwE err404
                        exe : _ -> return (Page exe))
        app1 :: Application
        app1 = serve jobsAPI server1
    run port app1

