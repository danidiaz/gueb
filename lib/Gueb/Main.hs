module Gueb.Main (
        defaultMain
    ) where

import Control.Applicative
import qualified Options.Applicative as O

data Plan = Plan
    {
        port :: Int
    ,   max_stdout_lines :: Int
    ,   max_stderr_lines :: Int
    ,   command_string :: String
    } deriving (Show,Eq)

defaultMain :: IO ()
defaultMain = print ""

