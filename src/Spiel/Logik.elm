module Spiel.Logik (..) where

import Spiel.Kortn exposing (..)


forb : Kort -> Forb
forb kort =
  case kort of
    Kort forb _ ->
      forb

    -- Wheli isch is niedrigste Schell
    Wheli ->
      Schell


checkKortn : KortnPaarl -> Result String KortnPaarl
checkKortn ( kort1, kort2 ) =
  if kort1 == kort2 then
    Result.Err "Die zwoa Kortn sein gleich"
  else
    Result.Ok ( kort1, kort2 )


heachererSchlog : KortnPaarl -> Result String KortnPaarl
heachererSchlog ( kort1, kort2 ) =
  let
    forb1 =
      forb kort1

    forb2 =
      forb kort2
  in
    if forb1 == forb2 then
      case ( kort1, kort2 ) of
        ( Wheli, kort2 ) ->
          Result.Ok ( kort2, Wheli )

        ( kort1, Wheli ) ->
          Result.Ok ( kort1, Wheli )

        ( Kort _ schlog1, Kort _ schlog2 ) ->
          case compareSchlog schlog1 schlog2 of
            EQ ->
              Result.Err "Der Schlog terf net gleich sein"

            GT ->
              Result.Ok ( kort1, kort2 )

            LT ->
              Result.Ok ( kort2, kort1 )
    else
      Result.Ok ( kort1, kort2 )


ischBlinder : Forb -> Kort -> Bool
ischBlinder forb' kort =
  forb kort == forb'


heachererBlinder : Forb -> KortnPaarl -> KortnPaarl
heachererBlinder forb ( kort1, kort2 ) =
  case ( ischBlinder forb kort1, ischBlinder forb kort2 ) of
    ( True, False ) ->
      ( kort1, kort2 )

    ( False, True ) ->
      ( kort2, kort1 )

    _ ->
      ( kort1, kort2 )


ischRechter : Kort -> Kort -> Bool
ischRechter rechter kort =
  rechter == kort



-- ischRechter : Kort -> Kort -> Bool
-- ischRechter rechter kort =
--   case ( rechter, kort ) of
--     ( Wheli _, Wheli _ ) ->
--       True
--
--     ( Kort rechterForb rechterSchlog, Kort kortnForb kortnSchlog ) ->
--       kortnForb == rechterForb && kortnSchlog == rechterSchlog
--
--     _ ->
--       False


ischSchlogUansHeacher : Schlog -> Schlog -> Bool
ischSchlogUansHeacher rechterSchlog kortnSchlog =
  if rechterSchlog == Ass then
    kortnSchlog == Siebmer
  else
    let
      schlogNummer =
        schlogAlsNummer rechterSchlog

      kortnNummer =
        schlogAlsNummer kortnSchlog
    in
      kortnNummer - schlogNummer == 1


ischGuater : Kort -> Kort -> Bool
ischGuater rechter kort =
  case ( rechter, kort ) of
    ( _, Wheli ) ->
      -- Wheli konn nia der Guate sein
      False

    ( Wheli, _ ) ->
      -- Wenn der Wheli der Rechte isch, konns kuan Guatn gebm
      False

    ( Kort rechterForb rechterSchlog, Kort kortnForb kortnSchlog ) ->
      ischSchlogUansHeacher rechterSchlog kortnSchlog && kortnForb == rechterForb


ischSchlog : Kort -> Kort -> Bool
ischSchlog rechter kort =
  case ( rechter, kort ) of
    ( Kort rechterForb rechterSchlog, Kort forb schlog ) ->
      if rechterForb == forb then
        False
      else
        rechterSchlog == schlog

    ( Wheli, Wheli ) ->
      True

    ( _, Wheli ) ->
      False

    ( Wheli, _ ) ->
      False


check : (Kort -> Kort -> Bool) -> Kort -> KortnPaarl -> KortnPaarl
check f kort ( kort1, kort2 ) =
  if f kort kort1 then
    ( kort1, kort2 )
  else if f kort kort2 then
    ( kort2, kort2 )
  else
    ( kort1, kort2 )


checkSchlog : Kort -> KortnPaarl -> KortnPaarl
checkSchlog =
  check ischSchlog


checkGuatn : Kort -> KortnPaarl -> KortnPaarl
checkGuatn =
  check ischGuater


checkRechtn : Kort -> KortnPaarl -> KortnPaarl
checkRechtn =
  check ischRechter


stechn : Kort -> KortnPaarl -> Result String KortnPaarl
stechn rechter =
  checkKortn
    >> Result.map (heachererBlinder <| forb rechter)
    >> (flip Result.andThen heachererSchlog)
    >> Result.map (checkSchlog rechter >> checkRechtn rechter >> checkGuatn rechter)
