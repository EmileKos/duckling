-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.NL.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Duration.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, examples)
  where
    examples =
      [ "voor maanden"
      , "in dagen"
      , "secretaris"
      , "minuten"
      , "ik beaam dat"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (DurationData 1 Second)
             [ "een sec"
             , "één seconde"
             , "1 seconde"
             , "1s"
             , "1 s"
             , "1\""
             ]
  , examples (DurationData 2 Minute)
             [ "2 min"
             , "2min"
             , "twee minuten"
             , "2'"
             ]
  , examples (DurationData 30 Day)
             [ "30 dagen"
             ]
  , examples (DurationData 7 Week)
             [ "seven weken"
             ]
  , examples (DurationData 1 Month)
             [ "1 maand"
             , "een maand"
             , "één maand"
             ]
  , examples (DurationData 3 Quarter)
             [ "3 trimesters"
             ]
  , examples (DurationData 2 Year)
             [ "2 jaren"
             ]
  ]
