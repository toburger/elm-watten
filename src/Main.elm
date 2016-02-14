module Main (..) where

import String
import Effects
import Task
import Html exposing (..)
import Html.Attributes exposing (src, width, height, style, title, class)
import Html.Events exposing (onClick)
import StartApp
import Spiel.Spieler exposing (..)
import Spiel.Kortn exposing (..)
import Spiel.Packtl exposing (..)


spieler : Int -> String -> Spieler
spieler id nome =
  Spieler id nome []


spieler1 : Spieler
spieler1 =
  spieler 1 "sepp"


spieler2 : Spieler
spieler2 =
  spieler 1 "franz"


spieler3 : Spieler
spieler3 =
  spieler 3 "toni"


spieler4 : Spieler
spieler4 =
  spieler 4 "luis"


team1 : Team
team1 =
  { id = 1
  , name = "team uans"
  , punktestond = 0
  , spieler = ( spieler1, spieler3 )
  }


team2 : Team
team2 =
  { id = 2
  , name = "team zwoa"
  , punktestond = 0
  , spieler = ( spieler2, spieler4 )
  }


teams : Teams
teams =
  ( team1, team2 )


type alias Model =
  { teams : Teams
  , packtl : Packtl
  }


initialModel : Model
initialModel =
  { teams = teams
  , packtl = packtl
  }


type Action
  = Nuistart
  | Mischgln
  | Mischgler Int
  | Gebm


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case Debug.log "action" action of
    Nuistart ->
      ( initialModel, Effects.none )

    Mischgln ->
      ( model
      , Effects.tick
          ((*) 1000.0
            >> round
            >> Mischgler
          )
      )

    Mischgler seed ->
      ( { model
          | packtl =
              model.packtl
                |> mischgln seed
        }
      , Effects.none
      )

    Gebm ->
      let
        ( teams, packtl ) =
          model.packtl
            |> gebm model.teams
      in
        ( { teams = teams, packtl = packtl }
        , Effects.none
        )


kortnNome : Spiel.Kortn.Kort -> String
kortnNome kort =
  let
    path =
      "/resources/images"
  in
    case kort of
      Kort forb schlog ->
        let
          forb' =
            toString forb |> String.toLower

          schlog' =
            toString schlog |> String.toLower
        in
          path ++ "/" ++ forb' ++ "/" ++ schlog' ++ ".png"

      Wheli ->
        path ++ "/wheli.png"


viewKort : Kort -> Html
viewKort kort =
  li
    [ class "inline-block mr1" ]
    [ img
        [ src (kortnNome kort)
        , height 100
        , title (toString kort)
        ]
        []
    ]


viewSpieler : Spieler -> Html
viewSpieler spieler =
  div
    []
    [ h2
        [ class "h2" ]
        [ text spieler.name ]
    , ul
        [ class "list-reset" ]
        (List.map viewKort spieler.hond)
    ]


viewTeam : Team -> Html
viewTeam team =
  div
    [ class "col-4" ]
    [ h1
        [ class "h1" ]
        [ text team.name ]
    , div
        []
        [ viewSpieler (fst team.spieler)
        , viewSpieler (snd team.spieler)
        ]
    ]


viewPacktl : Packtl -> Html
viewPacktl packtl =
  div
    []
    [ h1
        [ class "h1" ]
        [ text "Packtl" ]
    , ul
        [ class "list-reset" ]
        (List.map viewKort packtl)
    ]


view : Signal.Address Action -> Model -> Html
view address { teams, packtl } =
  div
    []
    [ button
        [ onClick address Nuistart ]
        [ text "nuistart" ]
    , button
        [ onClick address Mischgln ]
        [ text "mischgln" ]
    , button
        [ onClick address Gebm ]
        [ text "gebm" ]
    , div
        [ class "flex justify-start" ]
        [ viewTeam (fst teams)
        , viewTeam (snd teams)
        ]
    , viewPacktl packtl
    ]


app : StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
