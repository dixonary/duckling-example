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
  -- Timezone info changes somtimes, so duckling uses the operating system's
  -- own timezone information rather than baking it into the library.
  
  -- We grab it from /usr/share/zoneinfo. If your OS stores it elsewhere,
  -- you'll need to figure out the process for retrieving it yourself.
  -- The timezone libraries for Haskell (included as deps to this package) 
  -- should be a good starting point.
  let tz = "Europe/London"
  tzs  <- getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" <> tz
  
  -- A simple interaction loop.
  forever $ do
      putStr "Give me a time in natural language: "
      -- Flush to force rendering of the above string without a newline
      hFlush stdout

      str <- Text.pack <$> getLine
      time <- parseTime str tzs
      
      putStrLn $ "\n" ++ case time of
        Nothing -> "I couldn't parse a time out of that!"
        Just t -> "Found a time:\n  " ++ show t
      
      putStrLn ""


-- | Parse some data into an expected representation.
-- The [Seal Dimension] parameter is a list of possible dimensions that 
-- duckling should attempt to extract semantic data for from the raw string.
-- There are lots of different dimensions - this package only uses 
-- Time and Numeral.
parseDuckling :: Text -> [Seal Dimension] -> TimeZoneSeries -> IO [Entity]
parseDuckling rawString dims tzs = do

  -- We assume we are in the UK timezone.
  let 
    timezone :: IsString s => s
    timezone = "Europe/London"

  -- Get the current time, adjusted for local time zone.
  now <- currentReftime 
        (HashMap.singleton "Europe/London" tzs) 
        timezone

  let
    -- Duckling requires us to provide this context.
    context = Context { referenceTime = now, locale = makeLocale EN (Just GB) }
    options = Options { withLatent = True }

    -- Notice: Duckling's parsing is actually free of IO, so it would be
    -- possible to do all of this without IO if the "get current time" part
    -- were abstracted away!
    parsedResult = Duckling.Core.parse rawString context options dims

  return parsedResult

-- | Parse a fixed point in time.
parseTime :: Text -> TimeZoneSeries -> IO (Maybe ZonedTime)
parseTime rawString tzs = 
  -- Run the generic parseDuckling function above, extracting the first found
  -- time instance. Note that Duckling may find *zero or more* semantic elements, so we need to handle each.
  -- We expect times to be only single instants in time. In principle Duckling also supports time ranges, series, etc, but here we only care about instants.
  parseDuckling rawString [Seal Time] tzs
  <&> \case
      [] -> Nothing
      (h:_) -> case value h of 
        RVal Time (TimeValue (SimpleValue (InstantValue x _)) _ _) -> Just x
        _ -> Nothing

-- | Parse a number.
parseNumber :: Text -> TimeZoneSeries  -> IO (Maybe Integer)
parseNumber rawString tzs = parseDuckling rawString [Seal Numeral] tzs
  -- Similar to the Time example, but only using singular Numerals. See the duckling docs to find all the possible RVal types (there are a lot)!
  <&> \case
      [] -> Nothing
      (h:_) -> case value h of
        RVal Numeral (NumeralValue n) -> Just $ floor n

