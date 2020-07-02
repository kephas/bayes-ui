module Main exposing (..)

import Browser
import Cmd.Extra exposing (withNoCmd)
import Dict
import Element exposing (Element, column, el, layout, padding, spacing, text)
import Html exposing (Html)
import Probability as P



---- MODEL ----


type alias Model =
    { probas : List P.KeyedProba
    , probaViewState : P.ProbaViewState
    }


init : ( Model, Cmd Msg )
init =
    ( { probas = [ P.KeyedProba "a" 0.9999, P.KeyedOdds "b" 10 1, P.KeyedEvidence "c" 33 ], probaViewState = P.emptyViewState }, Cmd.none )



---- UPDATE ----


type Msg
    = ProbaMsg P.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProbaMsg submsg ->
            withNoCmd <|
                { model | probaViewState = P.update submsg model.probaViewState }



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        column [ spacing 10 ] <|
            List.map (P.viewKeyedProba ProbaMsg model.probaViewState) model.probas



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
