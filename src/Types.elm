module Types exposing (..)

import Date exposing (Date)



type alias Match =
    { matchId : Int
    , homeTeam : Country
    , awayTeam : Country
    , date : Date
    , venue : Venue
    , predictedResult : Maybe MatchResult
    , actualResult : Maybe MatchResult
    }


type alias MatchResult =
    { homeGoals : Int
    , awayGoals : Int
    }


type alias Group =
    { name : String
    , countries : List Country
    , matches : List Match
    }

type alias GroupCountryResult =
    { country : Country
    , wins : Int
    , losses : Int
    , draws : Int
    , goalDifference : Int
    , points : Int
    }


type Country
    = France
    | Switzerland
    | Romania
    | Albania
    | England
    | Russia
    | Slovakia
    | Wales
    | Germany
    | Ukraine
    | Poland
    | NorthernIreland
    | Spain
    | Croatia
    | CzechRepublic
    | Turkey
    | Belgium
    | Italy
    | Sweden
    | Ireland
    | Portugal
    | Austria
    | Hungary
    | Iceland

type Venue
    = SaintDenis
    | Lens
    | Bordeaux
    | Marseille
    | Nice
    | Lille
    | Paris
    | Toulouse
    | Lyon
    | SaintEtienne
