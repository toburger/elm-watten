module Main (..) where

import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple
import Spiel


spieler : Int -> String -> Spiel.Spieler
spieler id nome =
  Spiel.Spieler id nome []


spieler1 : Spiel.Spieler
spieler1 =
  spieler 1 "sepp"


spieler2 : Spiel.Spieler
spieler2 =
  spieler 1 "franz"


spieler3 : Spiel.Spieler
spieler3 =
  spieler 3 "valerio"


spieler4 : Spiel.Spieler
spieler4 =
  spieler 4 "alessio"


team1 : Spiel.Team
team1 =
  { id = 1
  , name = "trouler"
  , punktestond = 0
  , spieler = ( spieler1, spieler3 )
  }


team2 : Spiel.Team
team2 =
  { id = 2
  , name = "walsche"
  , punktestond = 0
  , spieler = ( spieler2, spieler4 )
  }


teams : Spiel.Teams
teams =
  ( team1, team2 )


spiel : ( Spiel.Teams, Spiel.PacktlRest )
spiel =
  Spiel.gebm Spiel.packtl teams


main : Signal Html
main =
  StartApp.Simple.start
    { model = spiel
    , update = \action model -> model
    , view = \address model -> div [ onClick address () ] [ text (toString model) ]
    }
