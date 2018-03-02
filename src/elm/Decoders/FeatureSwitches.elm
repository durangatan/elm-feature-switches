module Decoders.FeatureSwitches exposing (..)

import Json.Decode
import Json.Encode
import Json.Decode.Pipeline
import Model.FeatureSwitches exposing (FeatureSwitch)


decodeFeatureSwitch : Json.Decode.Decoder FeatureSwitch
decodeFeatureSwitch =
    Json.Decode.Pipeline.decode FeatureSwitch
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "isOn" Json.Decode.bool


decodeFeatureSwitches : Json.Decode.Decoder (List FeatureSwitch)
decodeFeatureSwitches =
    Json.Decode.list decodeFeatureSwitch

encodeFeatureSwitch : FeatureSwitch -> Json.Encode.Value
encodeFeatureSwitch record =
    Json.Encode.object
        [ ("name",  Json.Encode.string <| record.name)
        , ("description",  Json.Encode.string <| record.description)
        , ("isOn",  Json.Encode.bool <| record.isOn)
        ]