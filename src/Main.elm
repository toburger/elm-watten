module Main (..) where

import String
import Effects
import Task
import Html exposing (..)
import Html.Attributes exposing (src, width, height, style, title, class)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
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
  , tisch : Spiel
  }


initialModel : Model
initialModel =
  { teams = teams
  , packtl = packtl
  , tisch = []
  }


type Action
  = Nuistart
  | Mischgln
  | Mischgler Int
  | Gebm
  | KortSpieln Spieler Kort


kortEntfernen : Kort -> Teams -> Teams
kortEntfernen kort ( team1, team2 ) =
  let
    filterSpieler spieler =
      { spieler
        | hond =
            spieler.hond
              |> List.filter ((/=) kort)
      }

    filterTeam team =
      { team
        | spieler =
            ( filterSpieler (fst team.spieler)
            , filterSpieler (snd team.spieler)
            )
      }
  in
    ( filterTeam team1, filterTeam team2 )


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
        ( { model
            | teams = teams
            , packtl = packtl
          }
        , Effects.none
        )

    KortSpieln player kort ->
      ( { model
          | teams = kortEntfernen kort model.teams
          , tisch = kort :: model.tisch
        }
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


viewKort : Signal.Address Action -> Maybe (Spieler) -> Kort -> Html
viewKort address spieler kort =
  let
    attribs =
      case spieler of
        Just spieler ->
          [ onClick address (KortSpieln spieler kort) ]

        Nothing ->
          []
  in
    li
      [ class "inline-block mr1" ]
      [ img
          ([ src (kortnNome kort)
           , height 100
           , title (toString kort)
           ]
            ++ attribs
          )
          []
      ]


viewSpieler : Signal.Address Action -> Spieler -> Html
viewSpieler address spieler =
  div
    [ class "sm-col sm-col-6" ]
    [ h2
        [ class "h2" ]
        [ text spieler.name ]
    , ul
        [ class "list-reset" ]
        (List.map (lazy2 (viewKort address) (Just spieler)) spieler.hond)
    ]


viewTeam : Signal.Address Action -> Team -> Html
viewTeam address team =
  div
    [ class "sm-col sm-col-4" ]
    [ h1
        [ class "h1" ]
        [ text team.name ]
    , div
        [ class "clearfix" ]
        [ lazy (viewSpieler address) (fst team.spieler)
        , lazy (viewSpieler address) (snd team.spieler)
        ]
    ]


viewTisch : Signal.Address Action -> Spiel -> Html
viewTisch address tisch =
  div
    [ class "sm-col sm-col-4" ]
    [ h1
        [ class "h1" ]
        [ text "tisch"
        , ul
            []
            (List.map (lazy (viewKort address Nothing)) tisch)
        ]
    ]


viewPacktl : Signal.Address Action -> Packtl -> Html
viewPacktl address packtl =
  div
    []
    [ h1
        [ class "h1" ]
        [ text "packtl" ]
    , ul
        [ class "list-reset" ]
        (List.map (lazy (viewKort address Nothing)) packtl)
    ]


view : Signal.Address Action -> Model -> Html
view address { teams, packtl, tisch } =
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
        [ class "clearfix" ]
        [ lazy (viewTeam address) (fst teams)
        , lazy (viewTeam address) (snd teams)
        , lazy (viewTisch address) tisch
        ]
    , lazy (viewPacktl address) packtl
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
