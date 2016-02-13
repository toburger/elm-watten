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


type alias Model =
  ( Spiel.Teams, Spiel.Packtl )


initialModel : Model
initialModel =
  ( teams, Spiel.packtl )


type Action
  = Mischgln


update : Action -> Model -> Model
update action model =
  case action of
    Mischgln ->
      Spiel.gebm Spiel.packtl (fst model)


kortnNome : Spiel.Kortn.Kort -> String
kortnNome kort =
  case kort of
    Spiel.Kortn.Kort forb schlog ->
      toString forb ++ " " ++ toString schlog

    Spiel.Kortn.Wheli ->
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


view : Signal.Address Action -> Model -> Html
view address ( ( team1, team2 ), packtl ) =
  div
    []
    [ button
        [ onClick address Mischgln ]
        [ text "mischgln" ]
    , viewTeam team1
    , viewTeam team2
    ]


main : Signal Html
main =
  StartApp.Simple.start
    { model = initialModel
    , update = update
    , view = view
    }
