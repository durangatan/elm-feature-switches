module Main exposing (..)

-- comonent import example

import Decoders.FeatureSwitches exposing (decodeFeatureSwitches)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    }


initialModel : Model
initialModel =
    { featureSwitches = []
    , error = Nothing
    }



-- UPDATE


type Msg
    = FetchFeatureSwitches
    | FetchedFeatureSwitches (Result Http.Error (List FeatureSwitch))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFeatureSwitches ->
            ( initialModel, featureSwitchRequest )

        FetchedFeatureSwitches (Ok newFeatureSwitches) ->
            ( Model newFeatureSwitches Nothing, Cmd.none )

        FetchedFeatureSwitches (Err _) ->
            ( { model | error = Just "something happened!" }, Cmd.none )



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
                    featureSwitchesView model.featureSwitches
    in
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ] [ containerBody ]


featureSwitchView : FeatureSwitch -> Html Msg
featureSwitchView featureSwitch =
    li [ class (featureSwitchClass featureSwitch), style styles.featureSwitch ]
        [ span [ class "name" ] [ text featureSwitch.name ]
        , span [ class "description" ] [ text featureSwitch.description ]
        ]


featureSwitchesView : List FeatureSwitch -> Html Msg
featureSwitchesView featureSwitches =
    ul []
        (List.map
            featureSwitchView
            featureSwitches
        )


errorView : String -> Html Msg
errorView errorMessage =
    div [ class "error" ] [ text errorMessage ]



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
        [ ( "display", "flex" ), ( "flex-direction", "column" ) ]
    }



-- HTTP


featureSwitchRequest : Cmd Msg
featureSwitchRequest =
    let
        url =
            "http://localhost:8888/features"
    in
    Http.send FetchedFeatureSwitches (Http.get url decodeFeatureSwitches)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
