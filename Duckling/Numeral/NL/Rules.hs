-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.NL.Rules
  ( rules ) where

import Control.Monad (join)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types


zeroNineteenMap :: HashMap Text Integer
zeroNineteenMap = HashMap.fromList
  [ ("niks", 0)
  , ("nul", 0)
  , ("geen", 0)
  , ("één", 1)
  , ("een", 1)
  , ("twee", 2)
  , ("drie", 3)
  , ("vier", 4)
  , ("vijf", 5)
  , ("zes", 6)
  , ("zeven", 7)
  , ("acht", 8)
  , ("negen", 9)
  , ("tien", 10)
  , ("elf", 11)
  , ("twaalf", 12)
  , ("dertien", 13)
  , ("veertien", 14)
  , ("vijftien", 15)
  , ("zestien", 16)
  , ("zeventien", 17)
  , ("achttien", 18)
  , ("negentien", 19)
  ]

informalMap :: HashMap Text Integer
informalMap = HashMap.fromList
  [ ( "enkele?", 1 )
  , ( "een paar", 2 )
  , ( "paar", 2 )
  , ( "koppel", 2 )
  , ( "koppel van", 2 )
  , ( "paar of", 2 )
  , ( "een koppel van", 2 )
  , ( "een aantal", 3 )
  ]

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|min(us)?|negatief"
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral (NumeralData {TNumeral.value = v}):_) ->
        double $ v * (- 1)
      _ -> Nothing
  }

ruleIntegerNumeric :: Rule
ruleIntegerNumeric = Rule
  { name = "integer (numeric)"
  , pattern =
    [ regex "(\\d{1,18})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        v <- parseInt match
        integer $ toInteger v
      _ -> Nothing
  }

ruleFew :: Rule
ruleFew = Rule
  { name = "few"
  , pattern =
    [ regex "meerdere"
    ]
  , prod = \_ -> integer 3
  }

ruleTen :: Rule
ruleTen = Rule
  { name = "ten"
  , pattern =
    [ regex "tien"
    ]
  , prod = \_ -> integer 10 >>= withGrain 1
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let dot = Text.singleton '.'
            comma = Text.singleton ','
            fmt = Text.replace comma dot $ Text.replace dot Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal False match
      _ -> Nothing
  }

ruleInteger3 :: Rule
ruleInteger3 = Rule
  { name = "integer ([2-9][1-9])"
  , pattern =
    [ regex "(een|twee|drie|vier|vijf|zes|zeven|acht|negen)(s?en|ën)(twintig|dertig|veertig|vijftig|zestig|zeventig|tachtig|negentig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        v1 <- case Text.toLower m1 of
          "een" -> Just 1
          "twee" -> Just 2
          "drie" -> Just 3
          "vier" -> Just 4
          "vijf" -> Just 5
          "zes" -> Just 6
          "zeven" -> Just 7
          "acht" -> Just 8
          "negen" -> Just 9
          _ -> Nothing
        v3 <- case Text.toLower m3 of
          "twintig" -> Just 20
          "dertig" -> Just 30
          "veertig" -> Just 40
          "vijftig" -> Just 50
          "zestig" -> Just 60
          "zeventig" -> Just 70
          "tachtig" -> Just 80
          "negentig" -> Just 90
          _ -> Nothing
        integer $ v1 + v3
      _ -> Nothing
  }

ruleInteger4 :: Rule --werkt niet
ruleInteger4 = Rule
  { name = "integer ([2-9][2-9][2-9][1-9])"--tousands or hundreds, no tousand hundreds because they use a space after tausand. No "tien", "elf" "twaalf" "dertien" "veertien"
  , pattern =
    [ regex "(twee|drie|vier|vijf|zes|zeven|acht|negen)?(honderd|duizend)(en| )?(een|twee|drie|vier|vijf|zes|zeven|acht|negen)?(s?en|ën)?(twintig|dertig|veertig|vijftig|zestig|zeventig|tachtig|negentig)?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:m4:m5:m6:_)):_) -> do
        v1 <- case Text.toLower m1 of
          "twee" -> Just 2
          "drie" -> Just 3
          "vier" -> Just 4
          "vijf" -> Just 5
          "zes" -> Just 6
          "zeven" -> Just 7
          "acht" -> Just 8
          "negen" -> Just 9
          _ -> Nothing
        v2 <- case Text.toLower m2 of
          "honderd" -> Just 100
          "duizend" -> Just 1000    
          _ -> Nothing
        v4 <- case Text.toLower m4 of
          "een" -> Just 1
          "twee" -> Just 2
          "drie" -> Just 3
          "vier" -> Just 4
          "vijf" -> Just 5
          "zes" -> Just 6
          "zeven" -> Just 7
          "acht" -> Just 8
          "negen" -> Just 9
          _ -> Nothing
        v6 <- case Text.toLower m6 of
          "twintig" -> Just 20
          "dertig" -> Just 30
          "veertig" -> Just 40
          "vijftig" -> Just 50
          "zestig" -> Just 60
          "zeventig" -> Just 70
          "tachtig" -> Just 80
          "negentig" -> Just 90
          _ -> Nothing
        integer $ v1 * v2 + v4 + v6
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , numberWith TNumeral.multipliable id
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ numberWith (fromMaybe 0 . TNumeral.grain) (>1)
    , dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = val1, TNumeral.grain = Just g}):
       Token Numeral (NumeralData {TNumeral.value = val2}):
       _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v}):
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "k" -> double $ v * 1e3
         "m" -> double $ v * 1e6
         "g" -> double $ v * 1e9
         _ -> Nothing
      _ -> Nothing
  }

ruleNumeralsEn :: Rule
ruleNumeralsEn = Rule
  { name = "numbers en"
  , pattern =
    [ numberBetween 1 10
    , regex "en"
    , oneOf [20, 30 .. 90]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern =
    [ regex "(honderd|duizend|miljoen)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "honderd" -> double 1e2 >>= withGrain 2 >>= withMultipliable
        "duizend" -> double 1e3 >>= withGrain 3 >>= withMultipliable
        "miljoen" -> double 1e6 >>= withGrain 6 >>= withMultipliable
        _         -> Nothing
      _ -> Nothing
  }

ruleCouple :: Rule
ruleCouple = Rule
  { name = "couple"
  , pattern =
    [ regex "(een )?paar"
    ]
  , prod = \_ -> integer 2
  }

ruleInteger :: Rule
ruleInteger = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(geen|nul|niks|één|een|twee|drie|vier|vijftien|vijf|zestien|zes|zeventien|zeven|achttien|acht|negentien|negen|tien|elf|twaalf|dertien|veertien)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroNineteenMap >>= integer
      _ -> Nothing
  }

ruleInteger2 :: Rule
ruleInteger2 = Rule
  { name = "integer (20..90)"
  , pattern =
    [ regex "(twintig|dertig|veertig|vijftig|zestig|zeventig|tachtig|negentig)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "twintig" -> integer 20
        "dertig" -> integer 30
        "veertig" -> integer 40
        "vijftig" -> integer 50
        "zestig" -> integer 60
        "zeventig" -> integer 70
        "tachtig" -> integer 80
        "negentig" -> integer 90
        _ -> Nothing
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "komma"
    , numberWith TNumeral.grain isNothing
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace (Text.singleton '.') Text.empty match) >>= double
      _ -> Nothing
  }
ruleHundreds :: Rule
ruleHundreds = Rule
  { name = "hundreds"
  , pattern =
    [ regex "(twee|drie|vier|vijf|zes|zeven|acht|negen)honderd"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "twee" -> integer 200
        "drie" -> integer 300
        "vier" -> integer 400
        "vijf" -> integer 500
        "zes" -> integer 600
        "zeven" -> integer 700
        "acht" -> integer 800
        "negen" -> integer 900
        _ -> Nothing
      _ -> Nothing
  }

ruleHundredsAnd ::Rule --werkt niet
ruleHundredsAnd = Rule
  { name = "hundreds en"
  , pattern =
    [ oneOf [100, 200 .. 900]
    , regex "(en)?"
    , numberBetween 1 99
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral (NumeralData {TNumeral.value = v1}):
       _:
       Token Numeral (NumeralData {TNumeral.value = v2}):
       _) -> double $ v1 + v2
      _ -> Nothing
  }

ruleThousands :: Rule
ruleThousands = Rule
  { name = "thousands"
  , pattern =
    [ regex "(twee|drie|vier|vijf|zes|zeven|acht|negen)duizend"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "twee" -> integer 2000
        "drie" -> integer 3000
        "vier" -> integer 4000
        "vijf" -> integer 5000
        "zes" -> integer 6000
        "zeven" -> integer 7000
        "acht" -> integer 8000
        "negen" -> integer 9000
        _ -> Nothing
      _ -> Nothing
  }
ruleHThousand :: Rule
ruleHThousand = Rule
  { name = "hundredthousand"
  , pattern =
    [regex "honderdduizend"
    ]
  , prod = \_ -> integer 100000
  }
ruleHThousands :: Rule
ruleHThousands = Rule
  { name = "hundredthousands"
  , pattern =
    [ regex "(twee|drie|vier|vijf|zes|zeven|acht|negen)honderdduizend"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> case Text.toLower match of
        "twee" -> integer 200000
        "drie" -> integer 300000
        "vier" -> integer 400000
        "vijf" -> integer 500000
        "zes" -> integer 600000
        "zeven" -> integer 700000
        "acht" -> integer 800000
        "negen" -> integer 900000
        _ -> Nothing
      _ -> Nothing
  }

ruleFractions :: Rule
ruleFractions = Rule
  { name = "fractional number"
  , pattern = [regex "(\\d+)/(\\d+)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (numerator:denominator:_)):_) -> do
        n <- parseDecimal False numerator
        d <- parseDecimal False denominator
        divide n d
      _ -> Nothing
  }

ruleIntegerTh :: Rule
ruleIntegerTh = Rule
  { name = "integer (0..19)"
  , pattern =
    [ regex "(twee|drie|vier|vijftien|vijf|zestien|zes|zeventien|zeven|achttien|acht|negentien|negen|tien|elf|twaalf|dertien|veertien)(honderd|duizend)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        v1 <- case HashMap.lookup (Text.toLower m1) zeroNineteenMap >>= integer
        _ -> Nothing
        v2 <- case Text.toLower m2 of
          "honderd" -> double 1e2
          "duizend" -> double 1e3
        _ -> Nothing
          -> double $ v1 * v2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleCouple
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleFew
  , ruleFractions
  , ruleHundreds
  , ruleHundredsAnd
  , ruleInteger
  , ruleInteger2
  , ruleInteger3
  , ruleInteger4
  , ruleIntegerTh
  , ruleIntegerWithThousandsSeparator
  , ruleIntegerNumeric
  , ruleIntersect
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsEn
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleTen
  , ruleThousands
  , ruleHThousand
  , ruleHThousands
  ]
