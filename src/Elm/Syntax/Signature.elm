module Elm.Syntax.Signature exposing
    ( Signature
    , encode
    )

{-| This syntax represents type signatures in Elm.

For example :

    add : Int -> Int -> Int


## Types

@docs Signature


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Json.Encode as JE exposing (Value)


{-| Type alias representing a signature in Elm.
-}
type alias Signature =
    { name : Node String
    , typeAnnotation : Node TypeAnnotation
    }


{-| Encode a `Signature` syntax element to JSON.
-}
encode : Signature -> Value
encode { name, typeAnnotation } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "typeAnnotation", Node.encode TypeAnnotation.encode typeAnnotation )
        ]
