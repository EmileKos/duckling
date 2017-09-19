-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NL.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.AmountOfMoney.Types
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Dollar 1)
             [ "$1"
             , "één dollar"
             , "een dollar"
             ]
  , examples (simple Dollar 10)
             [ "$10"
             , "$ 10"
             , "10$"
             , "10 dollar"
             , "tien dollar"
             ]
  , examples (simple Cent 10)
             [ "10 cent"
             , "tien centen"
             , "tien cent"
             ]
  , examples (simple Dollar 1e4)
             [ "$10K"
             , "10k$"
             , "$10.000"
             , "$10000"
             , "tienduizend dollar"
             ]
  , examples (simple USD 3.14)
             [ "USD3,14"
             , "3,14US$"
             , "US$ 3,14"
             ]
  , examples (simple EUR 20)
             [ "20\x20ac"
             , "20 euros"
             , "20 eruo"
             , "20 Euro"
             , "20 Euro's"
             , "EUR 20"
             , "EUR 20,0"
             , "20€"
             , "20 €ur"
             , "€20"
             , "€ 20"
             ]
  , examples (simple Pound 10)
             [ "\x00a3\&10"
             , "tien pond"
             ]
  , examples (simple INR 20)
             [ "Rs. 20"
             , "Rs 20"
             , "20 roepies"
             , "20Rs"
             , "Rs20"
             ]
  , examples (simple INR 20.43)
             [ "20 roepies 43"
             , "twintig roepies 43"
             ]
  , examples (simple Dollar 20.43)
             [ "$20 en 43c"
             , "$20,43"
             , "20 dollar 43c"
             , "20 dollar 43 cent"
             , "twintig dollar 43 cent"
             , "20 dollar 43"
             , "twintig dollar en 43"
             ]
  , examples (simple GBP 3.01)
             [ "GBP3,01"
             , "GBP 3,01"
             , "3 pond 1 pence"
             ]
  , examples (simple Unnamed 42)
             [ "42 knotsen"
             , "ongeveer 42 knotsen"
             , "exact 42 knotsen"
             ]
  , examples (simple KWD 42)
             [ "42 KWD"
             , "42 koeweitse dinar"
             ]
  , examples (simple LBP 42)
             [ "42 LBP"
             , "42 libanese ponden"
             ]
  , examples (simple EGP 42)
             [ "42 EGP"
             , "42 egyptische ponden"
             ]
  , examples (simple QAR 42)
             [ "42 QAR"
             , "42 qatarese riyals"
             ]
  , examples (simple BGN 42)
             [ "42 BGN"
             , "42 bulgaarse nieuwe levs"
             ]
  , examples (simple MYR 42)
             [ "42 MYR"
             , "MYR 42"
             , "42MYR"
             , "MYR42"
             , "42 ringgits"
             , "42 maleisische ringgits"
             ]
  , examples (between Dollar (10, 20))
             [ "tussen 10 en 20 dollar"
             , "10 dollar tot 20 dollar"
             , "rond 10 tot 20 dollar"
             , "tussen 10 dollar en 20 dollar"
             , "van 10 tot 20 dollar"
             , "ongeveer $10-$20"
             , "10-20 dollar"
             ]
  , examples (under EUR 7)
             [ "onder zeven euro"
             , "minder dan 7 EUR"
             , "lager dan €7"
             ]
  , examples (above Dollar 1.42)
             [ "meer dan 1 dollar en vierenveertig cent"
             , "minstens $1,42"
             , "meer dan 1,42 dollar"
             , "boven een dollar and 42 cent"
             ]
  ]
