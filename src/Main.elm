port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Time
import Bootstrap
import Types exposing (..)
import GroupForm
import Dict exposing (Dict)
import Maybe

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
    , groupForms : Dict GroupName GroupForm.Model
    }


init : ( Model, Cmd msg )
init =
    let
        groups = Bootstrap.initialGroupSeed
        groupForms =
            List.map (\g -> (g.name, GroupForm.init g)) groups
                |> Dict.fromList
    in
        ( Model groups groupForms, Cmd.none )



-- UPDATE


type Msg
    = None
    | Tick
    | GroupFormMsg GroupName GroupForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Tick ->
            ( model, Cmd.none )

        GroupFormMsg mId subMsg ->
            let
                res =
                    Dict.get mId model.groupForms
                        |> Maybe.map (GroupForm.update subMsg)
            in
                case res of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( subMdl, subCmd ) ->
                        ( { model | groupForms = Dict.insert mId subMdl model.groupForms }
                        , Cmd.map (GroupFormMsg mId) subCmd
                        )


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
        (List.map groupViewItem (Dict.toList model.groupForms))


groupViewItem : (GroupName, GroupForm.Model) -> Html.Html Msg
groupViewItem (groupName, groupModel) =
    App.map (GroupFormMsg groupName) (GroupForm.view groupModel)

