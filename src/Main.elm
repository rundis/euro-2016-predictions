port module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Time
import Bootstrap
import Types exposing (..)
import Date

main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { groups : List Group
    }


init : ( Model, Cmd msg )
init =
    ( Model Bootstrap.initialGroupSeed, Cmd.none )



-- UPDATE


type Msg
    = None
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Tick ->
            ( model, Cmd.none )


port alert : String -> Cmd msg


port log : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second <| always Tick



-- VIEW


view : Model -> Html.Html Msg
view model =
    div [ class "container" ]
        (List.map groupView model.groups)


groupView : Group -> Html.Html Msg
groupView group =
    div [ class "row" ]
        [ h3 [] [ text ("Group " ++ group.name) ]
        , div [ class "col-sm-6 well" ]
            [ matchesTable group.matches ]
        , div [ class "col-sm-6"] [tablePredictions group]
        ]


matchesTable : List Match -> Html.Html Msg
matchesTable matches =
    table [ class "table table-condensed" ]
        [ tbody [] (List.map matchRow matches) ]


matchRow : Match -> Html.Html Msg
matchRow match =
    tr []
        [ td [] [text (formatDate match.date)]
        , td [style [("text-align", "right")]] [text (toString match.homeTeam)]
        , td [] [input [class "form-control input-sm"] []]
        , td [] [input [class "form-control input-sm"] []]
        , td [] [text (toString match.awayTeam)]
        ]


tablePredictions : Group -> Html.Html Msg
tablePredictions group =
    table
        [class "table table-condensed"]
        [ thead
            []
            [ tr
                []
                [ th [] [text "Country"]
                , th [] [text "W"]
                , th [] [text "D"]
                , th [] [text "L"]
                , th [] [text "Gd"]
                , th [] [text "Pts"]
                ]
            ]
        , tbody
            []
            (List.map predictionRow (Bootstrap.calcPredictedGroupResult group))
        ]


predictionRow :  GroupCountryResult -> Html.Html Msg
predictionRow res =
    tr
        []
        [ td [] [text (toString res.country)]
        , td [] [text (toString res.wins)]
        , td [] [text (toString res.draws)]
        , td [] [text (toString res.losses)]
        , td [] [text (toString res.goalDifference)]
        , td [] [text (toString res.points)]
        ]


formatDate : Date.Date -> String
formatDate date =
    (toString <| Date.day date) ++ "." ++ (toString <| Date.month date)
