module EclipseLogic
  ( advanceSecond
  , countdownLine
  , currentSky
  , localClock
  , moonPathOffset
  , moonPathProgress
  , placeCover
  , placeRows
  , skyCaption
  , skyLine
  , tickPeriod
  , utcClock
  , viewPlace
  ) where

import Prelude ((+), (-), (*), (/), (<), (<=), (>=), (==), (&&), (<>), ($), bind, map, max, min, negate, otherwise, pure, show)

import Data.Array (find)
import Data.Int (floor, quot, rem, round, toNumber)
import Data.JSDate (getTime, getTimezoneOffset, now)
import Data.Maybe (Maybe(..))
import Data.Number (remainder)
import Data.Ord (abs)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

currentSky :: {} -> Aff { nowMs :: Number, tzMs :: Number, place :: String }
currentSky _ = liftEffect do
  d <- now
  offMin <- getTimezoneOffset d
  pure { nowMs: getTime d, tzMs: negate offMin * 60000.0, place: observingPlace }

viewPlace :: String -> { place :: String }
viewPlace name = { place: name }

tickPeriod :: { ms :: Number }
tickPeriod = { ms: 1000.0 }

advanceSecond :: { nowMs :: Number } -> Maybe { nowMs :: Number }
advanceSecond { nowMs } = Just { nowMs: nowMs + tickPeriod.ms }

eclipseDayMs :: Number
eclipseDayMs = 1786492800000.0

atUTC :: Int -> Int -> Number
atUTC h m = eclipseDayMs + toNumber ((h * 60 + m) * 60) * 1000.0

firstContactMs :: Number
firstContactMs = atUTC 15 34

umbraBeginsMs :: Number
umbraBeginsMs = atUTC 16 58

greatestEclipseMs :: Number
greatestEclipseMs = atUTC 17 46

umbraEndsMs :: Number
umbraEndsMs = atUTC 18 34

lastContactMs :: Number
lastContactMs = atUTC 19 57

globalHalfMs :: Number
globalHalfMs = greatestEclipseMs - firstContactMs

placeCover :: { nowMs :: Number, place :: String } -> Number
placeCover { nowMs, place } =
  let c = circumstancesOf place
  in coverBell { nowMs, maxAtMs: c.maxAtMs, halfMs: c.halfMs, maxObs: c.maxObs }

moonPathProgress :: { nowMs :: Number, place :: String } -> Number
moonPathProgress { nowMs, place } =
  let c = circumstancesOf place
  in 2.0 * max (-1.6) (min 1.6 ((nowMs - c.maxAtMs) / c.halfMs))

moonPathOffset :: { place :: String } -> Number
moonPathOffset { place } = 2.0 * (1.0 - (circumstancesOf place).maxObs)

skyCaption :: { place :: String } -> String
skyCaption { place } = "The Sun and Moon over " <> place <> " — pick any place below · circumstances approximate"

circumstancesOf :: String -> { maxAtMs :: Number, halfMs :: Number, maxObs :: Number }
circumstancesOf name = case find (\c -> c.place == name) eclipsePlaces of
  Just c -> { maxAtMs: atUTC c.peakH c.peakM, halfMs: c.halfMin * 60000.0, maxObs: c.maxObs }
  Nothing -> { maxAtMs: greatestEclipseMs, halfMs: globalHalfMs, maxObs: 1.0 }

skyLine :: { nowMs :: Number, tzMs :: Number } -> String
skyLine { nowMs, tzMs }
  | nowMs < firstContactMs = "The Moon has not touched the Sun yet — first contact at " <> hhmm (firstContactMs + tzMs) <> " (15:34 UTC)"
  | nowMs < umbraBeginsMs = "Partial eclipse underway — the Moon's shadow deepens over Europe"
  | nowMs <= umbraEndsMs = "TOTALITY — the umbra sweeps Greenland, Iceland and northern Spain"
  | nowMs <= lastContactMs = "Partial eclipse waning — the Moon slides off the Sun"
  | otherwise = "The eclipse has ended — Europe's next total eclipse comes 2 August 2027"

countdownLine :: { nowMs :: Number, tzMs :: Number } -> String
countdownLine { nowMs, tzMs }
  | nowMs < greatestEclipseMs = "Greatest eclipse (" <> hhmm (greatestEclipseMs + tzMs) <> ", 17:46 UTC) in " <> duration (greatestEclipseMs - nowMs)
  | otherwise = "Greatest eclipse (" <> hhmm (greatestEclipseMs + tzMs) <> ", 17:46 UTC) was " <> duration (nowMs - greatestEclipseMs) <> " ago"

utcClock :: Number -> String
utcClock ms = clockOfDay ms

localClock :: { nowMs :: Number, tzMs :: Number } -> String
localClock { nowMs, tzMs } = clockOfDay (nowMs + tzMs)

placeRows
  :: { nowMs :: Number, tzMs :: Number, place :: String }
  -> Array { place :: String, span :: String, phase :: String, cover :: String, frac :: Number, observing :: Boolean, viewing :: Boolean }
placeRows { nowMs, tzMs, place } = map row eclipsePlaces
  where
  row c =
    let maxAtMs = atUTC c.peakH c.peakM
        halfMs = c.halfMin * 60000.0
        c1 = maxAtMs - halfMs
        c4 = maxAtMs + halfMs
        frac = coverBell { nowMs, maxAtMs, halfMs, maxObs: c.maxObs }
        phase =
          if nowMs < c1 then "begins " <> hhmm (c1 + tzMs)
          else if c.maxObs >= 1.0 && abs (nowMs - maxAtMs) <= c.totalSec * 500.0 then "TOTAL now"
          else if nowMs < maxAtMs then "partial · deepening"
          else if nowMs <= c4 then "partial · waning"
          else "ended " <> hhmm (c4 + tzMs)
    in
      { place: c.place
      , span: hhmm (c1 + tzMs) <> "–" <> hhmm (c4 + tzMs) <> " (" <> hhmm c1 <> "–" <> hhmm c4 <> " UTC)"
      , phase
      , cover: show (round (frac * 100.0)) <> "/" <> show (round (c.maxObs * 100.0)) <> "%"
      , frac
      , observing: c.place == observingPlace
      , viewing: c.place == place
      }

observingPlace :: String
observingPlace = "Olszówka (Mszana Dolna)"

eclipsePlaces :: Array { place :: String, peakH :: Int, peakM :: Int, halfMin :: Number, maxObs :: Number, totalSec :: Number }
eclipsePlaces =
  [ { place: "Reykjavík", peakH: 17, peakM: 48, halfMin: 58.0, maxObs: 1.0, totalSec: 110.0 }
  , { place: "Oslo", peakH: 17, peakM: 58, halfMin: 59.0, maxObs: 0.91, totalSec: 0.0 }
  , { place: "Warsaw", peakH: 18, peakM: 3, halfMin: 58.0, maxObs: 0.75, totalSec: 0.0 }
  , { place: "Bydgoszcz", peakH: 18, peakM: 4, halfMin: 59.0, maxObs: 0.81, totalSec: 0.0 }
  , { place: "Inowrocław", peakH: 18, peakM: 4, halfMin: 59.0, maxObs: 0.8, totalSec: 0.0 }
  , { place: "Kraków", peakH: 18, peakM: 6, halfMin: 58.0, maxObs: 0.72, totalSec: 0.0 }
  , { place: "Zabrze", peakH: 18, peakM: 6, halfMin: 58.0, maxObs: 0.75, totalSec: 0.0 }
  , { place: "Żory", peakH: 18, peakM: 6, halfMin: 58.0, maxObs: 0.74, totalSec: 0.0 }
  , { place: "Olszówka (Mszana Dolna)", peakH: 18, peakM: 7, halfMin: 58.0, maxObs: 0.71, totalSec: 0.0 }
  , { place: "Wrocław", peakH: 18, peakM: 7, halfMin: 59.0, maxObs: 0.79, totalSec: 0.0 }
  , { place: "Berlin", peakH: 18, peakM: 10, halfMin: 60.0, maxObs: 0.84, totalSec: 0.0 }
  , { place: "London", peakH: 18, peakM: 13, halfMin: 61.0, maxObs: 0.91, totalSec: 0.0 }
  , { place: "Paris", peakH: 18, peakM: 17, halfMin: 61.0, maxObs: 0.92, totalSec: 0.0 }
  , { place: "Rome", peakH: 18, peakM: 23, halfMin: 55.0, maxObs: 0.67, totalSec: 0.0 }
  , { place: "Bilbao", peakH: 18, peakM: 27, halfMin: 60.0, maxObs: 1.0, totalSec: 100.0 }
  , { place: "Zaragoza", peakH: 18, peakM: 29, halfMin: 59.0, maxObs: 1.0, totalSec: 85.0 }
  , { place: "Palma de Mallorca", peakH: 18, peakM: 31, halfMin: 55.0, maxObs: 1.0, totalSec: 90.0 }
  , { place: "Lisbon", peakH: 18, peakM: 32, halfMin: 55.0, maxObs: 0.93, totalSec: 0.0 }
  ]

coverBell :: { nowMs :: Number, maxAtMs :: Number, halfMs :: Number, maxObs :: Number } -> Number
coverBell { nowMs, maxAtMs, halfMs, maxObs } =
  let d = (nowMs - maxAtMs) / halfMs
  in min 1.0 (max 0.0 (maxObs * (1.0 - d * d)))

clockOfDay :: Number -> String
clockOfDay ms =
  let sod = floor (remainder ms 86400000.0 / 1000.0)
  in pad2 (sod `quot` 3600) <> ":" <> pad2 ((sod `rem` 3600) `quot` 60) <> ":" <> pad2 (sod `rem` 60)

hhmm :: Number -> String
hhmm ms =
  let sod = floor (remainder ms 86400000.0 / 1000.0)
  in pad2 (sod `quot` 3600) <> ":" <> pad2 ((sod `rem` 3600) `quot` 60)

duration :: Number -> String
duration ms =
  let total = floor (ms / 1000.0)
  in show (total `quot` 3600) <> "h " <> pad2 ((total `rem` 3600) `quot` 60) <> "m " <> pad2 (total `rem` 60) <> "s"

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n
