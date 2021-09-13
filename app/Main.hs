{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String
import Data.Text (Text)
import qualified Data.Text as Text

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Duckling.Core
import Duckling.Locale (Region(..))
import Duckling.Dimensions.Types
import Duckling.Resolve (DucklingTime)
import Duckling.Time.Types hiding (TimeOfDay)
import Duckling.Numeral.Types hiding (value)

import Data.Time hiding (parseTime)
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries, timeZoneFromSeries)
import Data.Time.LocalTime.TimeZone.Olson

import Data.Functor
import Control.Monad (forever)
import System.IO


-- Note: duckling is not in stackage
-- Note: Enabled language extensions are not needed 
-- but they made my life easier when writing this file so there

main :: IO ()
main = do
  let tz = "Europe/London"
  tzs  <- getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" <> tz
  forever $ do
      putStr "Give me a time in natural language: "
      hFlush stdout
      str <- Text.pack <$> getLine
      time <- parseTime str tzs
      putStrLn $ "\n" ++ case time of
        Nothing -> "I couldn't parse a time out of that!"
        Just t -> "Found a time:\n  " ++ show t
      putStrLn ""

-- | Parse some data into an expected representation.
parseDuckling :: Text -> [Seal Dimension] -> TimeZoneSeries -> IO [Entity]
parseDuckling rawString dims tzs = do

  let 
    timezone :: IsString s => s
    timezone = "Europe/London"

  now <- currentReftime 
        (HashMap.singleton "Europe/London" tzs) 
        timezone

  let
    context = Context { referenceTime = now, locale = makeLocale EN (Just GB) }
    options = Options { withLatent = True }

    parsedResult = Duckling.Core.parse rawString context options dims

  return parsedResult

-- | Parse a fixed point in time.
parseTime :: Text -> TimeZoneSeries -> IO (Maybe ZonedTime)
parseTime rawString tzs = 
  parseDuckling rawString [Seal Time] tzs
  <&> \case
      [] -> Nothing
      (h:_) -> case value h of 
        RVal Time (TimeValue (SimpleValue (InstantValue x _)) _ _) -> Just x
        _ -> Nothing

-- | Parse a number.
parseNumber :: Text -> TimeZoneSeries  -> IO (Maybe Integer)
parseNumber rawString tzs = parseDuckling rawString [Seal Numeral] tzs
  <&> \case
      [] -> Nothing
      (h:_) -> case value h of
        RVal Numeral (NumeralValue n) -> Just $ floor n

