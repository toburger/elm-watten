module Spiel (..) where

import Spiel.Kortn exposing (..)
import Array
import Random
import Random.Array


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


type alias Kortn =
  List Kort


type alias Packtl =
  Kortn


packtl : Packtl
packtl =
  let
    forbm =
      [ Herz, Schell, Lab, Oachl ]

    schleg =
      [ Siebmer, Ochter, Neiner, Zehner, Unter, Ober, Kinig, Ass ]
  in
    Wheli
      :: -- Konn mit List.apply <*> und List.map <$> realisiert werdn
         -- theoretisch: Kort <$> forbm <*> schleg
         (forbm
            |> List.concatMap
                (\forb ->
                  schleg
                    |> List.map (\schlog -> Kort forb schlog)
                )
         )


mischgln : Packtl -> Packtl
mischgln packtl =
  let
    seed =
      Random.initialSeed 1
  in
    packtl
      |> Array.fromList
      |> Random.Array.shuffle
      |> (flip Random.generate seed)
      |> fst
      |> Array.toList


type alias PacktlRest =
  Kortn


gebm : Packtl -> Teams -> ( Teams, PacktlRest )
gebm packtl teams =
  let
    ( team1, team2 ) =
      teams

    ( spieler1, spieler3 ) =
      team1.spieler

    ( spieler2, spieler4 ) =
      team2.spieler

    gschmigltesPaarl =
      mischgln packtl

    spieler1' =
      { spieler1
        | hond =
            gschmigltesPaarl
              |> List.take 5
      }

    spieler2' =
      { spieler2
        | hond =
            gschmigltesPaarl
              |> List.drop 5
              |> List.take 5
      }

    spieler3' =
      { spieler3
        | hond =
            gschmigltesPaarl
              |> List.drop 10
              |> List.take 5
      }

    spieler4' =
      { spieler4
        | hond =
            gschmigltesPaarl
              |> List.drop 15
              |> List.take 5
      }

    packtlRest =
      gschmigltesPaarl
        |> List.drop 20

    team1' =
      { team1 | spieler = ( spieler1', spieler3' ) }

    team2' =
      { team2 | spieler = ( spieler2', spieler4' ) }
  in
    ( ( team1', team2' ), packtlRest )
