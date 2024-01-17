module Elm.Json.Util exposing (encodeTyped)

import Json.Encode as JE exposing (Value)


encodeTyped : String -> Value -> Value
encodeTyped x v =
    JE.object
        [ ( "type", JE.string x )
        , ( x, v )
        ]
