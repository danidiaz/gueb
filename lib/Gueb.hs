module Gueb (
        noJobs
    ) where

import Data.Monoid

import Gueb.Types.API

noJobs :: Jobs
noJobs = Jobs mempty
