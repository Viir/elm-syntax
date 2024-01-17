module Elm.Syntax.Comments exposing
    ( Comment
    , encode
    )

{-| This syntax represents both single and multi line comments in Elm. For example:

    -- A comment




    {- Some
       multi
       line
       comment
    -}


## Types

@docs Comment


## Serialization

@docs encode, decoder

-}

import Json.Encode as JE exposing (Value)


{-| Type representing the comment syntax
-}
type alias Comment =
    String


{-| Encode a `Comment` syntax element to JSON.
-}
encode : Comment -> Value
encode =
    JE.string
