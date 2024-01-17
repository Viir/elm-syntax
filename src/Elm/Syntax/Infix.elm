module Elm.Syntax.Infix exposing
    ( Infix, InfixDirection(..)
    , encode, encodeDirection
    )

{-|


## Types

@docs Infix, InfixDirection


## Serialization

@docs encode, encodeDirection, decoder, decodeDirection

-}

import Elm.Syntax.Node as Node exposing (Node)
import Json.Encode as JE exposing (Value)


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
    | Non


{-| Encode an infix
-}
encode : Infix -> Value
encode inf =
    JE.object
        [ ( "direction", Node.encode encodeDirection inf.direction )
        , ( "precedence", Node.encode JE.int inf.precedence )
        , ( "operator", Node.encode JE.string inf.operator )
        , ( "function", Node.encode JE.string inf.function )
        ]


{-| Encode the infix direction
-}
encodeDirection : InfixDirection -> Value
encodeDirection d =
    case d of
        Left ->
            JE.string "left"

        Right ->
            JE.string "right"

        Non ->
            JE.string "non"
