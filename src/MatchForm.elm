module MatchForm exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Types exposing (..)
import Date
import Result
import Maybe
import String


type alias Model =
    { match : Match
    , homeGoals : String
    , awayGoals : String
    }


init : Match -> Model
init match =
    case match.predictedResult of
        Nothing ->
            Model match "" ""

        Just { homeGoals, awayGoals } ->
            Model match (toString homeGoals) (toString awayGoals)


type Msg
    = SetHomeGoals String
    | SetAwayGoals String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetHomeGoals v ->
            ( { model | homeGoals = v } |> maybeUpdatePrediction, Cmd.none )

        SetAwayGoals v ->
            ( { model | awayGoals = v } |> maybeUpdatePrediction, Cmd.none )


maybeUpdatePrediction : Model -> Model
maybeUpdatePrediction model =
    let
        toInt v =
            String.toInt v |> Result.toMaybe

        res =
            Maybe.map2 (\h a -> ( h, a )) (toInt model.homeGoals) (toInt model.awayGoals)

        updMatchPrediction ( h, a ) m =
            { m | predictedResult = Just (MatchResult h a) }
    in
        case res of
            Nothing ->
                model

            Just v ->
                { model | match = updMatchPrediction v model.match }


view : Model -> Html Msg
view { match, homeGoals, awayGoals } =
    tr []
        [ td [] [ text (formatDate match.date) ]
        , td [ style [ ( "text-align", "right" ) ] ] [ text (toString match.homeTeam) ]
        , td []
            [ input
                [ class "form-control input-sm"
                , value homeGoals
                , onInput SetHomeGoals
                ]
                []
            ]
        , td []
            [ input
                [ class "form-control input-sm"
                , value awayGoals
                , onInput SetAwayGoals
                ]
                []
            ]
        , td [] [ text (toString match.awayTeam) ]
        ]


formatDate : Date.Date -> String
formatDate date =
    (toString <| Date.day date) ++ "." ++ (toString <| Date.month date)
