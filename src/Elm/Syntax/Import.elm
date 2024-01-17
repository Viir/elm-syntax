module Elm.Syntax.Import exposing
    ( Import
    , encode
    )

{-| This syntax represents imports in Elm.
For example:

    import Html.Attributes as HA exposing (style)


## Types

@docs Import


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Encode as JE exposing (Value)


{-| Type alias representing an Import
-}
type alias Import =
    { moduleName : Node ModuleName
    , moduleAlias : Maybe (Node ModuleName)
    , exposingList : Maybe (Node Exposing)
    }


{-| Encode a `Import` syntax element to JSON.
-}
encode : Import -> Value
encode { moduleName, moduleAlias, exposingList } =
    JE.object
        [ ( "moduleName", Node.encode ModuleName.encode moduleName )
        , ( "moduleAlias"
          , moduleAlias
                |> Maybe.map (Node.encode ModuleName.encode)
                |> Maybe.withDefault JE.null
          )
        , ( "exposingList"
          , exposingList
                |> Maybe.map (Node.encode Exposing.encode)
                |> Maybe.withDefault JE.null
          )
        ]
