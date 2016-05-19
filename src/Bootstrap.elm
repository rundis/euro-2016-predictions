module Bootstrap exposing (initialGroupSeed, calcPredictedGroupResult)

import Types exposing (..)
import Date exposing (Date, fromString)
import Result
import List
import Dict
import Maybe


date : String -> Date
date str =
    case Date.fromString str of
        Result.Ok date ->
            date

        Result.Err _ ->
            Debug.crash "Why you can't type proper date"


initialGroupSeed : List Group
initialGroupSeed =
    [ Group "A"
        [ France, Switzerland, Romania, Albania ]
        [ Match 1 France Romania (date "2016-06-10") SaintDenis Nothing Nothing
        , Match 2 Albania Switzerland (date "2016-06-11") Lens Nothing Nothing
        , Match 14 Romania Switzerland (date "2016-06-15") Paris Nothing Nothing
        , Match 15 France Albania (date "2016-06-15") Marseille Nothing Nothing
        , Match 25 Romania Albania (date "2016-06-19") Lyon Nothing Nothing
        , Match 26 Switzerland France (date "2016-06-19") Lille Nothing Nothing
        ]
    , Group "B"
        [ England, Russia, Slovakia, Wales ]
        [ Match 3 Wales Slovakia (date "2016-06-11") Bordeaux Nothing Nothing
        , Match 4 England Russia (date "2016-06-11") Marseille Nothing Nothing
        , Match 13 Russia Slovakia (date "2016-06-15") Lille Nothing Nothing
        , Match 16 England Wales (date "2016-06-15") Lens Nothing Nothing
        , Match 27 Russia Wales (date "2016-06-20") Toulouse Nothing Nothing
        , Match 28 Slovakia England (date "2016-06-20") SaintEtienne Nothing Nothing
        ]
    ]


calcPredictedGroupResult : Group -> List GroupCountryResult
calcPredictedGroupResult group =
    let
        result match =
            case match.predictedResult of
                Nothing ->
                    Nothing

                Just res ->
                    Just ( match.homeTeam, match.awayTeam, res )

        predictions =
            List.filterMap result group.matches

        gameResult ( home, away, res ) acc =
            case (compare res.homeGoals res.awayGoals) of
                LT ->
                    Dict.update (toString home) (\mv -> Maybe.map (\v -> { v | losses = v.losses + 1 }) mv) acc
                        |> Dict.update (toString away) (\mv -> Maybe.map (\v -> { v | wins = v.wins + 1 }) mv)

                EQ ->
                    Dict.update (toString home) (\mv -> Maybe.map (\v -> { v | draws = v.draws + 1 }) mv) acc
                        |> Dict.update (toString away) (\mv -> Maybe.map (\v -> { v | draws = v.draws + 1 }) mv)

                GT ->
                    Dict.update (toString home) (\mv -> Maybe.map (\v -> { v | wins = v.wins + 1 }) mv) acc
                        |> Dict.update (toString away) (\mv -> Maybe.map (\v -> { v | losses = v.losses + 1 }) mv)
    in
        List.foldl gameResult (initGroupResults group.countries) predictions
            |> Dict.values


initGroupResults : List Country -> Dict.Dict String GroupCountryResult
initGroupResults countries =
    List.map (\country -> ( toString country, Types.GroupCountryResult country 0 0 0 0 0 )) countries
        |> Dict.fromList
