module Main (..) where

import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple
import Spiel
import Spiel.Kortn


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
  spieler 3 "toni"


spieler4 : Spiel.Spieler
spieler4 =
  spieler 4 "luis"


team1 : Spiel.Team
team1 =
  { id = 1
  , name = "team uans"
  , punktestond = 0
  , spieler = ( spieler1, spieler3 )
  }


team2 : Spiel.Team
team2 =
  { id = 2
  , name = "team zwoa"
  , punktestond = 0
  , spieler = ( spieler2, spieler4 )
  }


teams : Spiel.Teams
teams =
  ( team1, team2 )


spiel : ( Spiel.Teams, Spiel.PacktlRest )
spiel =
  Spiel.gebm Spiel.packtl teams


type alias Model =
  Spiel.Teams


initialModel : Model
initialModel =
  fst spiel


type alias Action =
  ()


update : Action -> Model -> Model
update action model =
  model


kortnNome : Spiel.Kortn.Kort -> String
kortnNome kort =
  case kort of
    Spiel.Kortn.Kort forb schlog ->
      toString forb ++ " " ++ toString schlog

    Spiel.Kortn.Wheli _ ->
      "Wheli"


viewKort : Spiel.Kortn.Kort -> Html
viewKort kort =
  li [] [ text (kortnNome kort) ]


viewSpieler : Spiel.Spieler -> Html
viewSpieler spieler =
  div
    []
    [ h2 [] [ text spieler.name ]
    , ul
        []
        (List.map viewKort spieler.hond)
    ]


viewTeam : Spiel.Team -> Html
viewTeam team =
  div
    []
    [ h1 [] [ text team.name ]
    , div
        []
        [ viewSpieler (fst team.spieler)
        , viewSpieler (snd team.spieler)
        ]
    ]


view : Signal.Address a -> Model -> Html
view address model =
  div
    []
    [ viewTeam (fst model)
    , viewTeam (snd model)
    ]


main : Signal Html
main =
  StartApp.Simple.start
    { model = initialModel
    , update = update
    , view = view
    }
