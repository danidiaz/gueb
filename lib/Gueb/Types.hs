module Gueb.Types where

import Gueb.Types.API

import Data.Text (Text)
import Data.Functor.Identity
import Data.Map.Strict
import Control.Comonad.Cofree

type Plan = Map Text Job 

type Unending a = Cofree Identity a

type ExecutionId = Text
