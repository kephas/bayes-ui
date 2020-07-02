module Probability exposing (KeyedProba(..), Msg, ProbaConversion(..), ProbaKey, ProbaViewState, Probability(..), convert, emptyViewState, update, viewKeyedProba, viewProba)

import Dict exposing (Dict)
import Element exposing (Element, el, text)
import Element.Events as Ev


type Probability
    = Proba Float
    | Odds Float Float
    | Evidence Int


type ProbaConversion
    = AsProba
    | AsOdds
    | AsEvidence


type alias ProbaKey =
    String


type KeyedProba
    = KeyedProba ProbaKey Float
    | KeyedOdds ProbaKey Float Float
    | KeyedEvidence ProbaKey Int


type alias ProbaViewState =
    Dict ProbaKey ProbaConversion


emptyViewState : ProbaViewState
emptyViewState =
    Dict.empty


getConversion : ProbaViewState -> ProbaKey -> Maybe ProbaConversion
getConversion state key =
    Dict.get key state



-- UPDATE


type Msg
    = ChangeViewState ProbaKey ProbaConversion


update : Msg -> ProbaViewState -> ProbaViewState
update msg state =
    case msg of
        ChangeViewState key conv ->
            Dict.insert key conv state


convert : Maybe ProbaConversion -> Probability -> Probability
convert to from =
    case ( from, to ) of
        ( _, Nothing ) ->
            from

        ( Proba _, Just AsProba ) ->
            from

        ( Odds _ _, Just AsOdds ) ->
            from

        ( Evidence _, Just AsEvidence ) ->
            from

        ( Proba p, Just AsOdds ) ->
            Odds (p / (1 - p)) 1

        ( Proba _, Just AsEvidence ) ->
            from
                |> convert (Just AsOdds)
                |> convert (Just AsEvidence)

        ( Odds for against, Just AsProba ) ->
            Proba <| for / (for + against)

        ( Odds for against, Just AsEvidence ) ->
            Evidence <| round <| (*) 10 <| logBase 10 <| for / against

        ( Evidence _, Just AsProba ) ->
            from
                |> convert (Just AsOdds)
                |> convert (Just AsProba)

        ( Evidence e, Just AsOdds ) ->
            Odds (10 ^ (toFloat e / 10)) 1


convertWith : ProbaViewState -> ProbaKey -> Probability -> Probability
convertWith state key from =
    convert (getConversion state key) from


probaKey : KeyedProba -> ProbaKey
probaKey kproba =
    case kproba of
        KeyedProba key _ ->
            key

        KeyedOdds key _ _ ->
            key

        KeyedEvidence key _ ->
            key


toProba : KeyedProba -> Probability
toProba kproba =
    case kproba of
        KeyedProba _ p ->
            Proba p

        KeyedOdds _ for against ->
            Odds for against

        KeyedEvidence _ e ->
            Evidence e


probaAsConversion : Probability -> ProbaConversion
probaAsConversion proba =
    case proba of
        Proba _ ->
            AsProba

        Odds _ _ ->
            AsOdds

        Evidence _ ->
            AsEvidence


nextConversion : ProbaConversion -> ProbaConversion
nextConversion conv =
    case conv of
        AsProba ->
            AsOdds

        AsOdds ->
            AsEvidence

        AsEvidence ->
            AsProba


nextConversionWith : ProbaViewState -> KeyedProba -> ProbaConversion
nextConversionWith state kproba =
    getConversion state (probaKey kproba)
        |> Maybe.withDefault (kproba |> toProba |> probaAsConversion)
        |> nextConversion


viewKeyedProba : (Msg -> msg) -> ProbaViewState -> KeyedProba -> Element msg
viewKeyedProba msgmap state kproba =
    el [ Ev.onClick <| ChangeViewState (probaKey kproba) <| nextConversionWith state kproba ]
        (toProba kproba
            |> convertWith state (probaKey kproba)
            |> viewProba
        )
        |> Element.map msgmap


viewProba : Probability -> Element msg
viewProba proba =
    text <|
        case proba of
            Proba p ->
                String.fromFloat (p * 100) ++ "%"

            Odds for against ->
                String.fromFloat for ++ ":" ++ String.fromFloat against

            Evidence e ->
                String.fromInt e ++ "dB"
