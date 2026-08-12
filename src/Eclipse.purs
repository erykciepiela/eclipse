module Eclipse (eclipse) where

import Prelude ((#), ($), (*), (-), (<>), (<<<), (>>>), Unit, const, identity, show)

import Data.Int (round)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import EclipseLogic (advanceSecond, countdownLine, currentSky, localClock, moonPathOffset, moonPathProgress, placeCover, placeRows, skyCaption, skyLine, tickPeriod, utcClock, viewPlace)
import Effect (Effect)
import PUI (action, completed, constantly, every, forField, foreach, looped, pempty, projected, silence, toCase, updated, with)
import PUI.Web.HTML (attrWith, body, cl, clWhen, clicked, div, h1, h4, p, progress, staticText, table, tbody, td, text, th, thead, tr, (:=))
import PUI.Web.SVG as SVG
import QualifiedDo.Semigroupoid as Semigroupoid

eclipse :: Effect Unit
eclipse =
  body $ div $ ( Semigroupoid.do
      silence # action currentSky
      ( Semigroupoid.do
          every tickPeriod advanceSecond
          ( RecordToRecord.do
              h1 (staticText "Solar Eclipse over Europe")
              h4 (staticText "12 August 2026 — total across Iceland and northern Spain, partial everywhere else")
              p >>> cl "readout" $ RecordToRecord.do
                text # projected @"value" localClock
                staticText " ("
                text # forField @"value" @"nowMs" utcClock
                staticText " UTC)"
              div >>> cl "phase" $ text # projected @"value" skyLine
              p >>> cl "readout" $ text # projected @"value" countdownLine
              SVG.svg >>> cl "sky" >>> "viewBox" := "0 0 400 240" $ RecordToRecord.do
                SVG.circle >>> cl "sun" >>> "cx" := "200" >>> "cy" := "120" >>> "r" := "60" $ pempty
                SVG.circle >>> cl "moon" >>> "r" := "62" >>> attrWith "cx" moonCx >>> attrWith "cy" (\sky -> moonCy { place: sky.place }) $ pempty # constantly {}
                SVG.path >>> cl "veil" >>> "d" := "M0 0H400V240H0Z" >>> attrWith "opacity" veilOpacity $ pempty # constantly {}
              p >>> cl "caption" $ text # projected @"value" skyCaption ) # completed
          ( table $ RecordToRecord.do
              thead $ tr $ RecordToRecord.do
                th (staticText "Place")
                th (staticText "Partial phase")
                th (staticText "State")
                th (staticText "Cover (now/max)")
                th (staticText "Sunset")
                th (staticText "Obscuration")
              tbody
                ( ( clicked ( ( tr $ RecordToRecord.do
                      td $ text # forField @"value" @"place" identity
                      td $ text # forField @"value" @"span" identity
                      td $ text # forField @"value" @"phase" identity
                      td $ text # forField @"value" @"cover" identity
                      td $ text # forField @"value" @"sunset" identity
                      td $ progress # forField @"value" @"frac" identity ) # completed ) # clWhen _.observing "observing" # clWhen _.viewing "viewing" ) # foreach @"place" placeRows ) ) # toCase @"placePicked" _.place # updated (match { placePicked: const <<< viewPlace })
      ) # looped
  ) # with {}

moonCx :: { nowMs :: Number, place :: String } -> String
moonCx sky = show (200.0 - 60.0 * moonPathProgress sky)

moonCy :: { place :: String } -> String
moonCy sky = show (120.0 - 60.0 * moonPathOffset sky)

veilOpacity :: { nowMs :: Number, place :: String } -> String
veilOpacity sky = show (round (78.0 * placeCover sky)) <> "%"
