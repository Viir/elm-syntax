module Elm.Syntax.ModuleName exposing
    ( ModuleName
    , encode
    )

{-| This syntax represents the module names in Elm. These can be used for imports, module names (duh), and for qualified access.
For example:

    module Elm.Syntax.ModuleName ...

    import Foo.Bar ...

    import ... as Something

    My.Module.something

    My.Module.SomeType


## Types

@docs ModuleName


## Serialization

@docs encode, decoder

-}

import Json.Encode as JE exposing (Value)


{-| Base representation for a module name
-}
type alias ModuleName =
    List String



-- Serialization


{-| Encode a `ModuleName` syntax element to JSON.
-}
encode : ModuleName -> Value
encode =
    JE.list JE.string
