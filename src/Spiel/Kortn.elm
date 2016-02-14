module Spiel.Kortn (..) where


type Forb
  = Herz
  | Schell
  | Lab
  | Oachl


type Schlog
  = Siebmer
  | Ochter
  | Neiner
  | Zehner
  | Unter
  | Ober
  | Kinig
  | Ass


type Kort
  = Kort Forb Schlog
  | Wheli


type alias Kortn =
  List Kort


type alias KortnPaarl =
  ( Kort, Kort )


type alias Spiel =
  ( Kort, Kort, Kort, Kort )


type alias Hond =
  List Kort


schlogAlsNummer : Schlog -> Int
schlogAlsNummer schlog =
  case schlog of
    Siebmer ->
      7

    Ochter ->
      8

    Neiner ->
      9

    Zehner ->
      10

    Unter ->
      11

    Ober ->
      12

    Kinig ->
      13

    Ass ->
      14


compareSchlog : Schlog -> Schlog -> Order
compareSchlog schlog1 schlog2 =
  compare
    (schlogAlsNummer schlog1)
    (schlogAlsNummer schlog2)
