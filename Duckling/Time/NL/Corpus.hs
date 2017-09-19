-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


--Source for spelling rules: https://www.taaltelefoon.be

{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.NL.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)

corpus :: Corpus
corpus = (testContext, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, examples)
  where
    examples =
      [ "laughing out loud"
      , "1 volwassene"
      , "we zijn gescheiden"
      , "25"
      , "het is deze"
      , "binnen 61"
      , "deze"
      , "de vorige"
      , "per enkele"
      , "op een paar van"
      , "per paar"
      , "per een beetje"
      , "per dozijn"
      , "enkel uur"
      , "dozijn uur"
      , "Rom 6"
      , "rom 6"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "nu"
             , "dit moment"
             , "juist nu"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "vandaag"
             , "op deze dag"
             ]
  , examples (datetime (2013, 2, 1, 0, 0, 0) Day)
             [ "2/2013"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             ["gisteren"]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "morgen"
             , "morge"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "maandag"
             , "ma"
             , "deze maandag"
             , "Maandag, 18 Feb"
             , "Ma, 18 Februari"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "dinsdag"
             , "di"
             , "Dinsdag de 19de"
             , "Dinsdag 19de"
             ]
  , examples (datetime (2013, 8, 15, 0, 0, 0) Day)
             [ "Di 15de"
             , "Di 15e"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "donderdag"
             , "do"
             , "do."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "vrijdag"
             , "vr"
             , "vr."
             , "vrij"
             , "vrij."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "zaterdag"
             , "zat"
             , "zat."
             , "za"
             , "za."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "zondag"
             , "zo"
             , "zo."
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "de eerste van maart"
             , "eerste maart"
             , "de eerste dag van maart"
             ]
  , examples (datetime (2013, 3, 3, 0, 0, 0) Day)
             ["3 maart"]
  , examples (datetime (2013, 3, 15, 0, 0, 0) Day)
             ["de iden van maart"]
  , examples (datetime (2015, 3, 3, 0, 0, 0) Day)
             [ "3 maart 2015"
             , "3e maart 2015"
             , "derde maart 2015"
             , "3/3/2015"
             , "3/3/15"
             , "3/3/'15"
             , "2015-3-3"
             , "2015-03-03"
             , "2015-03-03"

             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "op de 15de"
             , "de 15e van februari"
             , "15 februari"
             , "de vijftiende februari"
             , "februari 15"
             , "15e februari"
             , "15/2"
             , "15/02"
             , "op 15/2"
             , "op 15/02"
             , "15 / 2"
             , "15 / 02"
             , "15.2"
             , "15.02"
             , "2-15"
             , "02-15"
             , "2 - 15"s
             , "02 - 15"
             ]
  , examples (datetime (2013, 8, 8, 0, 0, 0) Day)
             ["8 Aug"]
  , examples (datetime (2014, 7, 18, 0, 0, 0) Day)
             [ "vr, 18 jul"
             , "vrij, 18 jul"
             , "18 jul, vrij"
             , "18 jul, vr"
             ]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Month)
             ["Oktober 2014"]
  , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
             [ "31/10/1974"
             , "31/10/74"
             , "31/10/'74"
             , "10-31-1974"
             , "10-31-74"
             , "10-31-'74"
             ]
  , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
             [ "14april 2015"
             , "15 april, 2015"
             , "14e April 15"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "volgende dinsdag"
             , "volgende dinsdag ergens"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             [ "vrijdag en acht dagen"
             , "vrijdag en 8 dagen"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             ["komende maart"]
  , examples (datetime (2014, 3, 1, 0, 0, 0) Month)
             ["maart volgend jaar"]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             ["zondag, 10 feb"]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "Woe, 13feb"
             , "Wo, 13feb"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Week)
             [ "deze week"
             , "huidige week"
             , "komende week"
             ]
  , examples (datetime (2013, 2, 4, 0, 0, 0) Week)
             [ "vorige week"
             , "voorbije week"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Week)
             [ "volgende week"
             , "de volgende week"
             , "volgende week ergens"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Month)
             [ "verleden maand"
             , "vorige maand"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             ["volgende maand"]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Quarter)
             [ "dit trimester"
             , "dit trim"
             ]
  , examples (datetime (2013, 4, 1, 0, 0, 0) Quarter)
             [ "volgend trimester"
             , "next trim"
             ]
  , examples (datetime (2013, 7, 1, 0, 0, 0) Quarter)
             [ "derde trimester"
             , "3rd trimester"
             , "derde trim"
             , "3e trim"
             , "het 3e trim"
             ]
  , examples (datetime (2018, 10, 1, 0, 0, 0) Quarter)
             [ "4e trimester 2018"
             , "4e trim 2018"
             , "het 4e trim van 2018"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Year)
             [ "vorig jaar"
             , "verleden jaar"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Year)
             [ "dit jaar"
             , "huidig jaar"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Year)
             [ "volgend jaar"
             , "komend jaar"
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "vorige zondag"
             , "zondag van vorige week"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             ["vorige dinsdag"]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             ["volgende dinsdag"]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             ["volgende woensdag"]
  , examples (datetime (2013, 2, 20, 0, 0, 0) Day)
             [ "volgende week woensdag"
             , "woensdag en acht dagen"
             ]
  , examples (datetime (2013, 2, 22, 0, 0, 0) Day)
             ["vrijdag en acht dagen"]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             ["deze week maandag"]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             ["deze week dinsdag"]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             ["deze week woensdag"]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             ["overmorgen"]
  , examples (datetime (2013, 2, 14, 17, 0, 0) Hour)
             [ "overmorgen 17u"
             , "overmorgen 17:00"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             ["eergisteren"]
  , examples (datetime (2013, 2, 10, 8, 0, 0) Hour)
             [ "eergisteren 8u"
             , "eergisteren 08:00"
             ]
  , examples (datetime (2013, 3, 25, 0, 0, 0) Day)
             ["laatste maandag van maart"]
  , examples (datetime (2014, 3, 30, 0, 0, 0) Day)
             ["laatste zondag van maart 2014"]
  , examples (datetime (2013, 10, 3, 0, 0, 0) Day)
             ["derde dag van oktober"]
  , examples (datetime (2014, 10, 6, 0, 0, 0) Week)
             ["eerste week van oktober 2014"]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             ["de week van 6 oktober"]
  , examples (datetime (2013, 10, 7, 0, 0, 0) Week)
             ["de week van 7 oktober"]
  , examples (datetime (2015, 10, 31, 0, 0, 0) Day)
             ["laatste dag van oktober 2015"]  
  , examples (datetime (2014, 9, 22, 0, 0, 0) Week)
             ["laatste week van september 2014"]
  , examples (datetime (2013, 10, 1, 0, 0, 0) Day)
             ["eerste dinsdag van oktober"]
  , examples (datetime (2014, 9, 16, 0, 0, 0) Day)
             ["derde dinsdag van september 2014"]
  , examples (datetime (2014, 10, 1, 0, 0, 0) Day)
             ["eerste woensdag van oktober 2014"]
  , examples (datetime (2014, 10, 8, 0, 0, 0) Day)
             ["tweede woensdag van oktober 2014"]
  , examples (datetime (2015, 1, 13, 0, 0, 0) Day)
             ["derde woensdag van oktober 2014"]
  , examples (datetime (2013, 2, 13, 3, 0, 0) Hour)
             [ "om 3u"
             , "3u s'ochtends"
             , "3u sochtends"
             , "om 3AM"
             , "om 03:00"
             , "3u in de ochtend"
             , "om drie uur"
             ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
             [ "3:18"
             , "03:18"
             , "3u18"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "om 15u"
             , "@15u"
             , "3PM"
             , "3 uur 's middags"
             , "3 uur smiddags"
             , "3u in de namiddag"
             , "3u in de middag ongeveer"
             , "om 15u ongeveer"
             , "rond 15u"
             ]
  , examples (datetime (2013, 2, 12, 15, 15, 0) Minute)
             [ "om 15u15"
             , "om kwart na 3 's middags"
             , "om kwart na 3 smiddags"
             , "3:15 in de namiddag"
             , "15:15"
             , "3:15pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 20, 0) Minute)
             [ "om 20 na 3 in de middag"
             , "3:20 in de namiddag"
             , "3:20 's namiddas'"
             , "3:20 snamiddags"
             , "twintig na 3 in de middag"
             , "3:20pm"
             ]
  , examples (datetime (2013, 2, 12, 15, 30, 0) Minute)
             [ "om half vier 's middags"
             , "om half vier smiddags"
             , "15:30"
             , "15u30"
             , "3:30pm"
             , "3:30"
             , "half vier"
             ]
  , examples (datetime (2013, 2, 12, 15, 23, 24) Second)
             [ "15:23:24"
             ]
  , examples (datetime (2013, 2, 12, 11, 45, 0) Minute)
             [ "kwart voor twaalf in de middag"
             , "11:45am"
             , "11:45"
             , "11u45"
             ]
  , examples (datetime (2013, 2, 12, 20, 0, 0) Hour)
             [ "8u 's avonds"
             , "8u vanavond"
             , "20u"
             , "20u00"
             , "20:00"
             , "om acht uur in de avond"
             , "vanavond om acht uur"
             ]
  , examples (datetime (2013, 9, 20, 19, 30, 0) Minute)
             [ "om 19:30 op vr, 20 sep"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "om 9u op zaterdag"
             ]
  , examples (datetime (2013, 2, 16, 9, 0, 0) Hour)
             [ "op zaterdag om 9u"
             ]
  , examples (datetime (2014, 7, 18, 19, 0, 0) Minute)
             [ "19:00 vr, jul 18, 2014"
             ]
  , examples (datetime (2013, 2, 12, 4, 30, 1) Second)
             [ "over een seconde"
             , "over 1s"
             , "in 1\""
             ]
  , examples (datetime (2013, 2, 12, 4, 31, 0) Second)
             [ "over een minuut"
             , "in 1'"
             ]
  , examples (datetime (2013, 2, 12, 4, 32, 0) Second)
             [ "over 2 minuten"
             , "over een paar minuten"
             ]
  , examples (datetime (2013, 2, 12, 4, 33, 0) Second)
             [ "over enkele minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Second)
             [ "over 60 minuten"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "over een kwartier"
             , "over 1/4u"
             , "over 1/4 u"
             , "over 1/4 uur"
             , "over 15 min"
             , "over 15 min."
             , "over 15 minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "over een halfuur"
             , "over een half uur"
             , "over 1/2u"
             , "over 1/2 u"
             , "over 1/2 uur"
             , "over 30 min"
             , "over 30 min."
             , "over 30 minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 15, 0) Second)
             [ "binnen drie kwartier"
             , "over 3/4u"
             , "over 3/4 u"
             , "over 3/4 uur"
             , "over 45 min"
             , "over 45 min."
             , "over 45 minuten"
             ]
  , examples (datetime (2013, 2, 12, 7, 0, 0) Second)
             [ "over 2,5 uur"
             , "over twee en een half uur"
             , "over tweeënhalf uur"
             , "over twee uur en dertig minuten"
             ]
  , examples (datetime (2013, 2, 12, 5, 30, 0) Minute)
             [ "over een uur"
             , "over 1u"
             ]
  , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
             [ "over een paar uur"
             , "over een paar uren"
             ]
  , examples (datetime (2013, 2, 12, 7, 30, 0) Minute)
             [ "over een aantal uur"
             , "over een aantal uren"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Minute)
             [ "over 24 uur"
             , "over 24u"
             ]
  , examples (datetime (2013, 2, 13, 4, 0, 0) Hour)
             [ "over een dag"
             ]
  , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
             [ "een dag van nu af"
             ]
  , examples (datetime (2016, 2, 12, 0, 0, 0) Day)
             [ "3 jaar vanaf vandaag"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "binnen 7 dagen"
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "binnen 1 week"
             , "binnen een week"
             ]
  , examples (datetime (2013, 2, 12, 5, 0, 0) Second)
             [ "binnen een half uur ongeveer"
             ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
             [ "7 dagen geleden"
             ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
             [ "14 dagen geleden"
             , "twee weken geleden"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "een week geleden"
             , "één week geleden"
             , "1 week geleden"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "drie weken geleden"
             ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
             [ "drie maanden geleden"
             ]
  , examples (datetime (2011, 2, 1, 0, 0, 0) Month)
             [ "twee jaar geleden"
             ]
  , examples (datetime (1954, 1, 1, 0, 0, 0) Year)
             [ "1954"
             ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
             [ "over een week"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             ["een jaar na Kerstmis"]
  , examples (datetimeInterval ((2013, 12, 18, 0, 0, 0), (2013, 12, 29, 0, 0, 0)) Day)
             ["voor 10 dagen vanaf 18 dec"]
  , examples (datetimeInterval ((2013, 6, 21, 0, 0, 0), (2013, 9, 24, 0, 0, 0)) Day)
             [ "deze zomer"
             , "huidige zomer"
             ]
  , examples (datetimeInterval ((2012, 12, 21, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "deze winter"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "xmas"
             , "Kerstmis"
             , "Kerstdag"
             ]
  , examples (datetime (2013, 12, 31, 0, 0, 0) Day)
             [ "oudejaarsavond"
             , "oudejaar"
             , "oudjaar"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "Nieuwjaar"
             , "nieuwjaarsdag"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "valentijn"
             , "Valentijnsdag"
             ]
  , examples (datetime (2013, 5, 1, 0, 0, 0) Day)
             [ "Dag van de Arbeid"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "halloween"
             , "volgende halloween"
             , "Halloween 2013"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "deze avond"
             , "vanavond"
             ]
  , examples (datetimeInterval ((2013, 2, 8, 18, 0, 0), (2013, 2, 11, 0, 0, 0)) Hour)
             [ "vorig weekend"
             , "vorig weekeinde"
             , "voorbije weekeind"
             , "vorbije weekend"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 18, 0, 0), (2013, 2, 14, 0, 0, 0)) Hour)
             [ "morgenavond"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 14, 0, 0)) Hour)
             [ "morgen lunchtijd"
             , "morgen tijdens lunch"
             ]
  , examples (datetimeInterval ((2013, 2, 11, 18, 0, 0), (2013, 2, 12, 0, 0, 0)) Hour)
             [ "gisteravond"
             , "gisterenavond"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 18, 0, 0), (2013, 2, 18, 0, 0, 0)) Hour)
             [ "dit weekend"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 12, 0, 0)) Hour)
             [ "maandag ochtend"
             , "maandag morgen"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 4, 0, 0), (2013, 2, 18, 9, 0, 0)) Hour)
             [ "maandag vroeg in de ochtend"
             , "maandag vroege ochtend"
             , "maandag in de vroege uurtjes van de ochtend"
             , "maandag vroeg in de morgen"
             , "maandag vroege morgen"
             , "maandag in de vroege uurtjes van de morgen"
             ]
  , examples (datetimeInterval ((2013, 2, 15, 4, 0, 0), (2013, 2, 15, 12, 0, 0)) Hour)
             [ "15 fabruari in de ochtend"
             , "15e februari in de ochtend"
             , "15 februari 's ochtends"
             , "15 fabruari in de morgen"
             , "15e februari in de morgen"
             , "15 februari 's morgens"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 29, 58), (2013, 2, 12, 4, 30, 0)) Second)
             [ "voorbije 2 seconden"
             , "voorbije twee seconden"
             , "afgelopen 2 seconden"
             , "afgelopen twee seconden"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 1), (2013, 2, 12, 4, 30, 4)) Second)
             [ "volgende 3 seconden"
             , "volgende drie seconden"
             , "komende 3 seconden"
             , "komende drie seconden"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 28, 0), (2013, 2, 12, 4, 30, 0)) Minute)
             [ "voorbije 2 minuten"
             , "voorbije twee minuten"
             , "afgelopen 2 minuten"
             , "afgelopen twee minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 31, 0), (2013, 2, 12, 4, 34, 0)) Minute)
             [ "volgende 3 minuten"
             , "komende 3 minuten"
             , "volgende drie minuten"
             , "komende drie minuten"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 3, 0, 0), (2013, 2, 12, 4, 0, 0)) Hour)
             [ "voorbije uur"
             , "afgelopen uur"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 5, 0, 0), (2013, 2, 12, 8, 0, 0)) Hour)
             [ "komende 3 uur"
             , "volgende 3 uur"
             , "komende drie uur"
             , "volgende drie uur"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "vorige 2 dagen"
             , "vorige twee dagen"
             , "afelopen 2 dagen"
             , "afelopen twee dagen"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "komende 3 dagen"
             , "volgende 3 dagen"
             , "komende drie dagen"
             , "volgende drie dagen"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 0, 0, 0), (2013, 2, 16, 0, 0, 0)) Day)
             [ "volgende aantal dagen"
             , "komende aantal dagen"
             ]
  , examples (datetimeInterval ((2013, 1, 28, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Week)
             [ "vorige 2 weken"
             , "vorige twee weken"
             , "afelopen 2 weken"
             , "afelopen twee weken"
             ]
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Week)
             [ "komende 3 weken"
             , "volgende 3 weken"
             , "komende drie weken"
             , "volgende drie weken"
             ]
  , examples (datetimeInterval ((2012, 12, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Month)
             [ "vorige 2 maanden"
             , "vorige twee maanden"
             , "afelopen 2 maanden"
             , "afelopen twee maanden"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 6, 1, 0, 0, 0)) Month)
             [ "komende 3 maanden"
             , "volgende 3 maanden"
             , "komende drie maanden"
             , "volgende drie maanden"
             ]
  , examples (datetimeInterval ((2011, 1, 1, 0, 0, 0), (2013, 1, 1, 0, 0, 0)) Year)
             [ "vorige 2 jaren"
             , "vorige twee jaren"
             , "afelopen 2 jaren"
             , "afelopen twee jaren"
             ]
  , examples (datetimeInterval ((2014, 1, 1, 0, 0, 0), (2017, 1, 1, 0, 0, 0)) Year)
             [ "komende 3 jaren"
             , "volgende 3 jaren"
             , "komende drie jaren"
             , "volgende drie jaren"
             ]
  , examples (datetimeInterval ((2013, 7, 13, 0, 0, 0), (2013, 7, 16, 0, 0, 0)) Day)
             [ "13-15 juli"
             , "13 tot 15 juli"
             , "van 13 tot 15 juli"
             , "van 13 tot en met 15 juli"
             , "van 13 tem 15 juli"
             , "13 juli - 15 juli"
             , "van 13-15 juli"
             , "13 tot 15 juli"
             , "13 tot en met 15 juli"
             , "13 tem 15 juli"
             ]
  , examples (datetimeInterval ((2013, 8, 8, 0, 0, 0), (2013, 8, 13, 0, 0, 0)) Day)
             [ "8 aug - 12 aug"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 9, 30, 0), (2013, 2, 12, 11, 1, 0)) Minute)
             [ "9:30 - 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "van 9:30 - 11:00 op donderdag"
             , "tussen 9:30 en 11:00 op donderdag"
             , "9:30 - 11:00 op donderdag"
             , "later dan 9:30 maar voor 11:00 op donderdag"
             , "donderdag van 9:30 tot 11:00"
             , "van 9:30 tot 11:00 op donderdag"
             , "9:30 tot 11:00 op donderdag"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 1, 0, 0), (2013, 2, 13, 2, 31, 0)) Minute)
             [ "morgen tussen 1-2:30 ongeveer"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "3-5pm"
             , "van 3 tot 5 in de namidag"
             , "rond 15u - 17u"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 30, 0), (2013, 2, 12, 19, 0, 0)) Minute)
             [ "15:30 tot 19:00"
             , "3:30-19:00."
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 14, 0, 0)) Hour)
             [ "8am - 1pm"
             , "08:00 - 14:00"
             , "8u - 14u"
             ]
  , examples (datetimeInterval ((2013, 2, 14, 9, 0, 0), (2013, 2, 14, 12, 0, 0)) Hour)
             [ "donderdag van 9 tot 11"
             , "deze donderdag 9-11"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 11, 30, 0), (2013, 2, 12, 13, 31, 0)) Minute)
             [ "11:30-1:30"
             ]
  , examples (datetime (2013, 9, 21, 13, 30, 0) Minute)
             [ "11:30 op zat, 21 sep"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 26, 0, 0, 0)) Second)
             [ "binnen 2 weken"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 2, 12, 14, 0, 0)) Second)
             [ "voor 14:00"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 30, 0), (2013, 4, 1, 0, 0, 0)) Second)
             [ "voor het einde van volgende maand"
             ]
  , examples (datetime (2013, 2, 14, 6, 0, 0) Minute)
             [ "donderdag 8:00 GMT"
             , "don om 8 GMT"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "vandaag om 14u"
             , "om 14u"
             , "om 14:00"
             ]
  , examples (datetime (2013, 4, 25, 16, 0, 0) Minute)
             [ "25/04 om 16:00"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "15:00 morgen"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 14, 0, 0) Minute)
             [ "tot 14:00"
             , "voor 14:00"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 12, 14, 0, 0) Hour)
             [ "na 14:00"
             , "vanaf 14:00"
             ]
  , examples (datetimeOpenInterval After (2013, 2, 17, 4, 0, 0) Hour)
             [ "na 5 dagen"
             ]
  , examples (datetimeOpenInterval Before (2013, 2, 12, 11, 0, 0) Hour)
             [ "voor 11:00"
             , "tot 11:00"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "in de middag"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 8, 0, 0), (2013, 2, 12, 19, 0, 0)) Hour)
             [ "8:00 tot 19u"
             ]
  , examples (datetime (2013, 2, 12, 13, 30, 0) Minute)
             [ "om 13:30"
             , "13:30"
             ]
  , examples (datetime (2013, 2, 12, 4, 45, 0) Second)
             [ "over 15 minuten"
             , "over 15'"
             , "over een kwartier"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 13, 0, 0), (2013, 2, 12, 17, 0, 0)) Hour)
             [ "na lunch"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 21, 0, 0)) Hour)
             [ "na school"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "10:30"
             , "ongeveer half elf"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 4, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "deze morgen"
             , "deze ochtend"
             ]
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "volgende maandag"
             ]
  , examples (datetime (2013, 2, 12, 12, 0, 0) Hour)
             [ "om 12:00"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Hour)
             [ "om 24:00"
             , "om 00:00"
             , "om 0:00"
             , "om middernacht"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Month)
             [ "maart"
             , "in maart"
             ]
  , examples (datetime (2013, 2, 13, 17, 0, 0) Hour)
             [ "morgenmiddag om 5u"
             , "om 5u morgenmiddag"
             , "om 17u morgen"
             , "morgen om 17u"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 12, 0, 0), (2013, 2, 13, 19, 0, 0)) Hour)
             [ "morgenmiddag"
             ]
  , examples (datetimeInterval ((2013, 2, 13, 13, 0, 0), (2013, 2, 13, 15, 0, 0)) Hour)
             [ "13u-15u morgen"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "op de eerste"
             , "de eerste"
             , "de 1ste"
             ]
  , examples (datetime (2013, 2, 12, 10, 30, 0) Minute)
             [ "om 10:30"
             , "rond 10:30"
             ]
  , examples (datetime (2013, 2, 12, 19, 30, 0) Minute)
             [ "om 7u30 in de avond"
             , "om 7u30 's avonds"
             , "om 19u30"
             , "om 19:30"
             ]
  , examples (datetime (2013, 2, 13, 1, 50, 0) Minute)
             [ "morgen om 1u50"
             ]
  , examples (datetime (2013, 2, 12, 23, 0, 0) Hour)
             [ "vanavond om 11u"
             ]
  , examples (datetime (2013, 2, 12, 4, 23, 0) Minute)
    -- yes, the result is in the past, we may need to revisit
             [ "om 4:23"
             , "4:23"
             , "4u23"
             ]
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 3, 11, 0, 0, 0)) Day)
             [ "begin maart"
             ]
  , examples (datetimeInterval ((2013, 3, 11, 0, 0, 0), (2013, 3, 21, 0, 0, 0)) Day)
             [ "midden maart"
             ]
  , examples (datetimeInterval ((2013, 3, 21, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ "einde maart"
             ]
  , examples (datetimeInterval ((2013, 10, 25, 18, 0, 0), (2013, 10, 28, 0, 0, 0)) Hour)
             [ "laatste weekend van oktober"
             , "laatste weekeind in oktober"
             , "laatste weekeinde in oktober"
             ]
  , examples (datetimeInterval ((2013, 7, 26, 18, 0, 0), (2013, 7, 29, 0, 0, 0)) Hour)
             [ "laatste weekend van juli"
             , "laatste weekeinde van juli"
              "laatste weekeind van juli"
             ]
  , examples (datetimeInterval ((2017, 10, 27, 18, 0, 0), (2017, 10, 30, 0, 0, 0)) Hour)
             [ "laatste weekend van oktober 2017"
             , "laatste weekeinde van oktober 2017"
             , "laatste weekeind van oktober 2017"
             ]
  , examples (datetimeInterval ((2013, 8, 27, 0, 0, 0), (2013, 8, 30, 0, 0, 0)) Day)
             [ "27 - 29 augustus"
             , "from 27 - 29 augustus"
             ]
  , examples (datetimeInterval ((2013, 10, 23, 0, 0, 0), (2013, 10, 27, 0, 0, 0)) Day)
             [ "23ste tot 26ste oktober"
             ]
  , examples (datetimeInterval ((2013, 9, 1, 0, 0, 0), (2013, 9, 9, 0, 0, 0)) Day)
             [ "1-8 september"
             ]
  , examples (datetimeInterval ((2013, 9, 12, 0, 0, 0), (2013, 9, 17, 0, 0, 0)) Day)
             [ "12 tot 16 september"
             ]
  , examples (datetimeInterval ((2013, 8, 19, 0, 0, 0), (2013, 8, 22, 0, 0, 0)) Day)
             [ "19 tot 21 augustus"
             ]

  ]