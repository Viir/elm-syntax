module Elm.Syntax.Documentation exposing
    ( Documentation
    , encode
    )

{-| This syntax represents documentation comments in Elm.


## Types

@docs Documentation


## Serialization

@docs encode, decoder

-}

import Json.Encode as JE exposing (Value)


{-| Type representing the documentation syntax
-}
type alias Documentation =
    String



-- Serialization


{-| Encode a `Documentation` syntax element to JSON.
-}
encode : Documentation -> Value
encode =
    JE.string
