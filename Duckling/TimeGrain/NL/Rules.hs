-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.NL.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types

grains :: [(Text, String, TG.Grain)]
grains = [ ("seconde (grain)"  , "s(ec)?(ond)?e?n?",   TG.Second)
         , ("minuut (grain)"   , "min(uu?t(en)?)?",    TG.Minute)
         , ("uur (grain)"      , "uu?(r(en)?)?",       TG.Hour)
         , ("dag (grain)"      , "dag(en)?",           TG.Day)
         , ("week (grain)"     , "wee?k(en)?",         TG.Week)
         , ("maand (grain)"    , "maand(en)?",         TG.Month)
         , ("trimester (grain)", "(trimester|trim)s?", TG.Quarter)
         , ("jaar (grain)"     , "jaa?r(en)?",         TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }
