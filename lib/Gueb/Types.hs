module Gueb.Types where

import Gueb.Types.API

import Data.Text (Text)
import Data.Map.Strict
import qualified Data.Map.Strict as Map

type Plan = Map Text Job 

