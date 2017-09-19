-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = NL}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "1ste"
             , "eerste"
             , "1e"
             ]
  , examples (OrdinalData 3)
             [ "3de"
             , "derde"
             , "3e"
             ]
  , examples (OrdinalData 4)
             [ "4de"
             , "vierde"
             , "4e"
             ]
  , examples (OrdinalData 8)
             [ "8ste"
             , "achtste"
             , "8e"
             ]
  , examples (OrdinalData 14)
             [ "14de"
             , "veertiende"
             , "14e"
             ]
  , examples (OrdinalData 18)
             [ "18de"
             , "achttiende"
             , "18e"
             ]     
  , examples (OrdinalData 17)
             [ "zeventiende"
             , "17de"
             , "17e"
             ]
  , examples (OrdinalData 100)
             [ "100ste"
             , "honderdste"
             , "100e"
             ]  
  ]
