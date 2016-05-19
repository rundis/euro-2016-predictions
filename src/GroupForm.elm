module GroupForm exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App as App
import Types exposing (..)
import MatchForm
import Dict exposing (Dict)
import Maybe


type alias Model =
    { group : Group
    , matchPredictions : Dict MatchId MatchForm.Model
    }


init : Group -> Model
init group =
    let
        forms =
            List.map (\m -> ( m.matchId, MatchForm.init m )) group.matches
                |> Dict.fromList
    in
        Model group forms


type Msg
    = None
    | MatchFormMsg MatchId MatchForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        MatchFormMsg mId subMsg ->
            let
                res =
                    Dict.get mId model.matchPredictions
                        |> Maybe.map (MatchForm.update subMsg)
            in
                case res of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( subMdl, subCmd ) ->
                        ( { model
                            | matchPredictions = Dict.insert mId subMdl model.matchPredictions
                            , group = model.group
                          }
                        , Cmd.map (MatchFormMsg mId) subCmd
                        )


view : Model -> Html Msg
view { group, matchPredictions } =
    div [ class "row" ]
        [ h3 [] [ text ("Group " ++ group.name) ]
        , div [ class "col-sm-6 well" ]
            [ matchesTable matchPredictions ]
        , div [ class "col-sm-6" ] [ tablePredictions group matchPredictions ]
        ]


matchesTable : Dict MatchId MatchForm.Model -> Html Msg
matchesTable matchPredictions =
    table [ class "table table-condensed" ]
        [ tbody [] (List.map matchView (Dict.toList matchPredictions)) ]


matchView : ( MatchId, MatchForm.Model ) -> Html Msg
matchView ( matchId, matchModel ) =
    App.map (MatchFormMsg matchId) (MatchForm.view matchModel)


tablePredictions : Group -> Dict MatchId MatchForm.Model -> Html.Html Msg
tablePredictions group predictions =
    table [ class "table table-condensed" ]
        [ thead []
            [ tr []
                [ th [] [ text "Country" ]
                , th [] [ text "W" ]
                , th [] [ text "D" ]
                , th [] [ text "L" ]
                , th [] [ text "Gd" ]
                , th [] [ text "Pts" ]
                ]
            ]
        , tbody []
            (List.map snd (Dict.toList predictions)
                |> List.map .match
                |> (calcPredictedGroupResult group)
                |> List.map predictionRow
            )
          --(List.map predictionRow (calcPredictedGroupResult group (List.map   )))
        ]


predictionRow : GroupCountryResult -> Html.Html Msg
predictionRow res =
    tr []
        [ td [] [ text (toString res.country) ]
        , td [] [ text (toString res.wins) ]
        , td [] [ text (toString res.draws) ]
        , td [] [ text (toString res.losses) ]
        , td [] [ text (toString res.goalDifference) ]
        , td [] [ text (toString res.points) ]
        ]


calcPredictedGroupResult : Group -> List Match -> List GroupCountryResult
calcPredictedGroupResult group matches =
    let
        result match =
            Maybe.map (\res -> ( toString match.homeTeam, toString match.awayTeam, res )) match.predictedResult

        loss gd mv =
            mv |> Maybe.map (\v -> { v | losses = v.losses + 1, goalDifference = v.goalDifference - gd })

        win gd mv =
            mv |> Maybe.map (\v -> { v | wins = v.wins + 1, goalDifference = v.goalDifference + gd, points = v.points + 3 })

        draw mv =
            mv |> Maybe.map (\v -> { v | draws = v.draws + 1, points = v.points + 1 })

        gameResult ( home, away, res ) acc =
            let
                goalDifference =
                    res.homeGoals - res.awayGoals
            in
                case (compare res.homeGoals res.awayGoals) of
                    LT ->
                        Dict.update home (loss -goalDifference) acc
                            |> Dict.update away (win -goalDifference)

                    EQ ->
                        Dict.update home draw acc
                            |> Dict.update away draw

                    GT ->
                        Dict.update home (win goalDifference)  acc
                            |> Dict.update away (loss goalDifference)
    in
        List.filterMap result matches
            |> List.foldl gameResult (initGroupResults group.countries)
            |> Dict.values
            |> List.sortWith resultSort


resultSort : GroupCountryResult -> GroupCountryResult -> Order
resultSort a b =
    case compare a.points b.points of
      LT -> GT
      EQ ->
          case compare a.goalDifference b.goalDifference of
              LT -> GT
              EQ -> EQ
              GT -> LT
      GT -> LT



initGroupResults : List Country -> Dict.Dict String GroupCountryResult
initGroupResults countries =
    List.map (\country -> ( toString country, Types.GroupCountryResult country 0 0 0 0 0 )) countries
        |> Dict.fromList
