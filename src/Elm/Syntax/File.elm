module Elm.Syntax.File exposing
    ( File
    , encode
    )

{-| This syntax represents a whole Elm file.


## Types

@docs File


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Comments as Comments exposing (Comment)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import as Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Encode as JE exposing (Value)


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node Declaration)
    , comments : List (Node Comment)
    }


{-| Encode a `File` syntax element to JSON.
-}
encode : File -> Value
encode { moduleDefinition, imports, declarations, comments } =
    JE.object
        [ ( "moduleDefinition", Node.encode Module.encode moduleDefinition )
        , ( "imports", JE.list (Node.encode Import.encode) imports )
        , ( "declarations", JE.list (Node.encode Declaration.encode) declarations )
        , ( "comments", JE.list (Node.encode Comments.encode) comments )
        ]
