-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Lang
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {lang = NL}, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 0)
             [ "0"
             , "nul"
             , "geen"
             , "niks"
             ]
  , examples (NumeralValue 1)
             [ "1"
             , "een"
             , "één"
             ]
  , examples (NumeralValue 2)
             [ "2"
             , "twee"
             , "een paar"
             , "paar"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "3 en 30"
             , "drieendertig"
             , "drieëndertig"
             , "0033"
             ]
  , examples (NumeralValue 10)
             [ "10"
             , "tien"
             ]             
  , examples (NumeralValue 11)
             [ "11"
             , "elf"
             ]
  , examples (NumeralValue 12)
             [ "12"
             , "twaalf"
             , "een dozijn"
             ]
  , examples (NumeralValue 13)
             [ "13"
             , "dertien"
             ]                         
  , examples (NumeralValue 14)
             [ "14"
             , "veertien"
             ]
  , examples (NumeralValue 16)
             [ "16"
             , "zestien"
             ]
  , examples (NumeralValue 17)
             [ "17"
             , "zeventien"
             ]
  , examples (NumeralValue 18)
             [ "18"
             , "achttien"
             ]
  , examples (NumeralValue 24)
             [ "24"
             , "vierentwintig"
             , "twee dozijn"
             ]             
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100.000,0"
             , "100000"
             , "100K"
             , "100k"
             , "honderdduizend"
             ]
  , examples (NumeralValue 0.2)
             [ "1/5"
             , "2/10"
             , "3/15"
             , "20/100"
             , "0,2"
             ]
  , examples (NumeralValue 3e6)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "3 miljoen"
             , "drie miljoen"
             ]
  , examples (NumeralValue 1.2e6)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200k"
             , "1,2 miljoen"
             , "een miljoen tweehonderdduizend"
             , "één miljoen tweehonderdduizend"
             ]
  , examples (NumeralValue 5000)
             [ "5 duizend"
             , "vijfduizend"
             ]
  , examples (NumeralValue (-1.2e6))
             [ "- 1.200.000"
             , "-1200000"
             , "min 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "min een miljoen tweehonderdduizend"
             , "min één miljoen tweehonderdduizend"
             ]
  , examples (NumeralValue 122)
             [ "honderdtweeëntwintig"
             , "honderdtweentwintig"
             ]
  , examples (NumeralValue 2e5)
             [ "tweehonderdduizend"
             , "200.000"
             , "200000"
             ]
  , examples (NumeralValue 21011)
             [ "eenentwintigduizend elf"
             , "eenentwintigduizend en elf"
             , "eenentwintigduizendelf"
             ]
  , examples (NumeralValue 721012)
             [ "zevenhonderdeenentwintigduizend twaalf"
             , "zevenhonderdeenentwintigduizendtwaalf"
             , "zevenhonderdeenentwintigduizend en twaalf"
             ]
  , examples (NumeralValue 31256721)
             [ "eenendertig miljoen tweehonderdzessenvijftigduizend zevenhonderdeenentwintig"
             ]
  , examples (NumeralValue 2400)
             [ "tweehonderd dozijn"
             , "200 dozijn"
             ]
  , examples (NumeralValue 2200000)
             [ "twee miljoen tweehonderdduizend"
             , "2,2 miljoen"
             ]
  ]