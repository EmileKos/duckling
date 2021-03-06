-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.NL.Rules
  ( rules ) where

import Data.Maybe
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData (..))
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import Duckling.Dimensions.Types
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pond(en)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleOtherPounds :: Rule
ruleOtherPounds = Rule
  { name = "other pounds"
  , pattern =
    [ regex "(egyptische|libanese )?pond(en)?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "egyptische" -> Just . Token AmountOfMoney $ currencyOnly EGP
        "libanese" -> Just . Token AmountOfMoney $ currencyOnly LBP
        _          -> Nothing
      _ -> Nothing
  }

ruleRiyals :: Rule
ruleRiyals = Rule
  { name = "riyals"
  , pattern =
    [ regex "(qatarese|soedische )?riyals?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "qatarese" -> Just . Token AmountOfMoney $ currencyOnly QAR
        "saoedische"  -> Just . Token AmountOfMoney $ currencyOnly SAR
        _        -> Nothing
      _ -> Nothing
  }

ruleDinars :: Rule
ruleDinars = Rule
  { name = "dinars"
  , pattern =
    [ regex "(koeweitse )?dinars?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "koeweitse" -> Just . Token AmountOfMoney $ currencyOnly KWD
        _        -> Nothing
      _ -> Nothing
  }

ruleDirham :: Rule
ruleDirham = Rule
  { name = "dirham"
  , pattern =
    [ regex "dirhams?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly AED
  }

ruleRinggit :: Rule
ruleRinggit = Rule
  { name = "ringgit"
  , pattern =
    [ regex "(maleisische )?ringgits?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly MYR
  }

ruleCent :: Rule
ruleCent = Rule
  { name = "cent"
  , pattern =
    [ regex "cent(en)?|penn(y|ies)|pence|sens?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Cent
  }

ruleBucks :: Rule
ruleBucks = Rule
  { name = "bucks"
  , pattern =
    [ regex "knots(en)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Unnamed
  }

ruleACurrency :: Rule
ruleACurrency = Rule
  { name = "a <currency>"
  , pattern =
    [ regex "een"
    , financeWith TAmountOfMoney.value isNothing
    ]
  , prod = \tokens -> case tokens of
      (_:Token AmountOfMoney fd:_) -> Just . Token AmountOfMoney $ fd {TAmountOfMoney.value = Just 1}
      _ -> Nothing
  }

ruleIntersectAndXCents :: Rule
ruleIntersectAndXCents = Rule
  { name = "intersect (and X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "en"
    , financeWith TAmountOfMoney.currency (== Cent)
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ dimension Numeral 
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = c}):
       Token AmountOfMoney fd:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectAndNumeral :: Rule
ruleIntersectAndNumeral = Rule
  { name = "intersect (and number)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "en"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       _:
       Token Numeral (NumeralData {TNumeral.value = c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleIntersectXCents :: Rule
ruleIntersectXCents = Rule
  { name = "intersect (X cents)"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , financeWith id $ \x -> case TAmountOfMoney.value x of
        Just v | v > 0 -> TAmountOfMoney.currency x == Cent
        _              -> False
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just c}):
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "exact|precies|ongeveer|circa|dicht bij|rond|bijna"
    , dimension AmountOfMoney
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "tussen|van"
    , dimension Numeral
    , regex "en|tot"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Numeral (NumeralData {TNumeral.value = from}):
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}):
       _) ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "tussen|van"
    , financeWith TAmountOfMoney.value isJust
    , regex "en|tot"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just from, TAmountOfMoney.currency = c1}):
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c2}):
       _) | c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <amount-of-money>"
  , pattern =
    [ dimension Numeral
    , regex "-"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = from}):
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}):
       _) ->
         Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<amount-of-money> - <amount-of-money>"
  , pattern =
    [ financeWith TAmountOfMoney.value isJust
    , regex "-"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just from, TAmountOfMoney.currency = c1}):
       _:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c2}):
       _) | c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "(onder|minder|lager|niet meer) dan"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}):
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
    [ regex "(over|boven|minstens|meer) dan"
    , financeWith TAmountOfMoney.value isJust
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token AmountOfMoney (AmountOfMoneyData {TAmountOfMoney.value = Just to, TAmountOfMoney.currency = c}):
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleACurrency
  , ruleBucks
  , ruleCent
  , ruleDinars
  , ruleDirham
  , ruleIntersect
  , ruleIntersectAndNumeral
  , ruleIntersectAndXCents
  , ruleIntersectXCents
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  , ruleOtherPounds
  , rulePounds
  , rulePrecision
  , ruleRinggit
  , ruleRiyals
  ]
