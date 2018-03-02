module Main exposing (..)

-- comonent import example

import Decoders.FeatureSwitches exposing (decodeFeatureSwitch, decodeFeatureSwitches, encodeFeatureSwitch)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.FeatureSwitches exposing (FeatureSwitch)


-- APP


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


init : ( Model, Cmd Msg )
init =
    ( initialModel, featureSwitchRequest )



-- MODEL


type alias Model =
    { featureSwitches : List FeatureSwitch
    , error : Maybe String
    , isFetching : Bool
    , filter : Filter
    , unsavedFeatureSwitch : FeatureSwitch
    }


initialUnsavedFeatureSwitch : FeatureSwitch
initialUnsavedFeatureSwitch =
    { name = "", description = "", isOn = False }


initialModel : Model
initialModel =
    { featureSwitches = []
    , error = Nothing
    , isFetching = False
    , filter = All
    , unsavedFeatureSwitch = initialUnsavedFeatureSwitch
    }


type Filter
    = All
    | Enabled



-- UPDATE


type Msg
    = FetchFeatureSwitches
    | FetchedFeatureSwitches (Result Http.Error (List FeatureSwitch))
    | SetFilter String
    | UpdateName String
    | UpdateDescription String
    | UpdateCheckbox
    | CreateNewFeatureSwitch
    | CreatedNewFeatureSwitch (Result Http.Error FeatureSwitch)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFeatureSwitches ->
            ( { model | isFetching = True }, featureSwitchRequest )

        FetchedFeatureSwitches (Ok newFeatureSwitches) ->
            ( { model | featureSwitches = newFeatureSwitches, isFetching = False }, Cmd.none )

        FetchedFeatureSwitches (Err _) ->
            ( { model | error = Just "something happened!" }, Cmd.none )

        SetFilter filter ->
            let
                filterValue =
                    if filter == "Enabled" then
                        Enabled
                    else
                        All
            in
            ( { model | filter = filterValue }, Cmd.none )

        UpdateName name ->
            let
                oldFeatureSwitch =
                    model.unsavedFeatureSwitch

                newFeatureSwitch =
                    { oldFeatureSwitch | name = name }
            in
            ( { model | unsavedFeatureSwitch = newFeatureSwitch }, Cmd.none )

        UpdateDescription description ->
            let
                oldFeatureSwitch =
                    model.unsavedFeatureSwitch

                newFeatureSwitch =
                    { oldFeatureSwitch | description = description }
            in
            ( { model | unsavedFeatureSwitch = newFeatureSwitch }, Cmd.none )

        UpdateCheckbox ->
            let
                oldFeatureSwitch =
                    model.unsavedFeatureSwitch

                newFeatureSwitch =
                    { oldFeatureSwitch | isOn = not oldFeatureSwitch.isOn }
            in
            ( { model | unsavedFeatureSwitch = newFeatureSwitch }, Cmd.none )

        CreateNewFeatureSwitch ->
            ( { model | isFetching = True }, createFeatureSwitchRequest model )

        CreatedNewFeatureSwitch (Ok newFeatureSwitch) ->
            ( { model | isFetching = False, featureSwitches = newFeatureSwitch :: model.featureSwitches, unsavedFeatureSwitch = initialUnsavedFeatureSwitch }, Cmd.none )

        CreatedNewFeatureSwitch (Err _) ->
            ( { model | isFetching = False, error = Just "Something went wrong with creating the new fs", unsavedFeatureSwitch = initialUnsavedFeatureSwitch }, Cmd.none )



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    let
        containerBody =
            case model.error of
                Just error ->
                    errorView error

                Nothing ->
                    featureSwitchesView model
    in
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ] [ div [ class "controls" ] [ refreshButton, filterSelector ], containerBody ]


featureSwitchView : FeatureSwitch -> Html Msg
featureSwitchView featureSwitch =
    li [ class (featureSwitchClass featureSwitch), style styles.featureSwitch ]
        [ span [ class "name" ] [ text featureSwitch.name ]
        , span [ class "description" ] [ text featureSwitch.description ]
        ]


featureSwitchForm : Html Msg
featureSwitchForm =
    li [ class "feature-switch form" ]
        [ input [ class "name", type_ "text", placeholder "name", onInput UpdateName ] []
        , input [ class "description", type_ "text", placeholder "description", onInput UpdateDescription ] []
        , input [ class "isOn", type_ "checkbox", onClick UpdateCheckbox ] []
        , button [ class "button", onClick CreateNewFeatureSwitch ] [ text "Create!" ]
        ]


featureSwitchesView : Model -> Html Msg
featureSwitchesView model =
    if model.isFetching then
        div [ class "loading-message" ] [ text "... Hold Please" ]
    else
        ul []
            (featureSwitchForm
                :: List.map
                    featureSwitchView
                    (List.filter
                        (getFilter
                            model.filter
                        )
                        model.featureSwitches
                    )
            )


errorView : String -> Html Msg
errorView errorMessage =
    div [ class "error" ] [ text errorMessage ]


refreshButton : Html Msg
refreshButton =
    button [ onClick FetchFeatureSwitches ] [ text "Refresh Feature Switches" ]


getFilter : Filter -> FeatureSwitch -> Bool
getFilter filter =
    case filter of
        All ->
            \_ -> True

        Enabled ->
            \fs -> fs.isOn


filterSelector : Html Msg
filterSelector =
    select [ onInput SetFilter ]
        [ option [ value "All" ] [ text "All" ], option [ value "Enabled" ] [ text "Enabled" ] ]



-- CSS STYLES


featureSwitchClass : FeatureSwitch -> String
featureSwitchClass featureSwitch =
    if featureSwitch.isOn then
        "feature-switch on"
    else
        "feature-switch"


styles : { featureSwitch : List ( String, String ) }
styles =
    { featureSwitch =
        [ ( "display", "flex" )
        , ( "flex-direction", "column" )
        , ( "padding", "10px" )
        , ( "border", "solid 1px black" )
        , ( "border-radius", "20px" )
        , ( "margin", "10px" )
        ]
    }



-- HTTP


featureSwitchRequest : Cmd Msg
featureSwitchRequest =
    let
        url =
            "http://localhost:8888/features"
    in
    Http.send FetchedFeatureSwitches (Http.get url decodeFeatureSwitches)


createFeatureSwitchRequest : Model -> Cmd Msg
createFeatureSwitchRequest model =
    let
        url =
            "http://localhost:8888/features"

        body =
            Http.jsonBody (encodeFeatureSwitch model.unsavedFeatureSwitch)
    in
    Http.send CreatedNewFeatureSwitch (Http.post url body decodeFeatureSwitch)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
