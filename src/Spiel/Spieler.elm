module Spiel.Spieler (..) where

import Spiel.Kortn exposing (..)


type alias Spieler =
  { id : Int
  , name : String
  , hond : Hond
  }


{-| Team af tirolerisch? |
-}
type alias Team =
  { id : Int
  , name : String
  , punktestond : Int
  , spieler : ( Spieler, Spieler )
  }


type alias Teams =
  ( Team, Team )
