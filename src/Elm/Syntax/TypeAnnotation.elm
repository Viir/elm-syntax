module Elm.Syntax.TypeAnnotation exposing
    ( TypeAnnotation(..), RecordDefinition, RecordField
    , encode, decoder
    )

{-|


# Type Annotation Syntax

This syntax represents the type annotation syntax.
For example:

    Int -> String


## Types

@docs TypeAnnotation, RecordDefinition, RecordField


## Serialization

@docs encode, decoder

-}

import Elm.Json.Util exposing (decodeTyped, encodeTyped)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Custom type for different type annotations. For example:

  - `GenericType`: `a`
  - `Type`: `Maybe (Int -> String)`
  - `Tuples`: `(a, b, c)` or Unit `()`
  - `Record`: `{ name : String}`
  - `GenericRecord`: `{ a | name : String}`
  - `FunctionTypeAnnotation`: `Int -> String`

-}
type TypeAnnotation
    = GenericType String
    | Type (Node ( ModuleName, String )) (List (Node TypeAnnotation))
    | Tuple (List (Node TypeAnnotation))
    | Record RecordDefinition (Maybe (Node String))
    | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)


{-| A list of fields in-order of a record type annotation.
-}
type alias RecordDefinition =
    List (Node RecordField)


{-| Single field of a record. A name and its type.
-}
type alias RecordField =
    ( Node String, Node TypeAnnotation )



-- Serialization


{-| Encode a `TypeAnnotation` syntax element to JSON.
-}
encode : TypeAnnotation -> Value
encode typeAnnotation =
    case typeAnnotation of
        GenericType name ->
            encodeTyped "generic" <|
                JE.object
                    [ ( "value", JE.string name )
                    ]

        Type moduleNameAndName args ->
            let
                inner : ( ModuleName, String ) -> Value
                inner ( mod, n ) =
                    JE.object
                        [ ( "moduleName", ModuleName.encode mod )
                        , ( "name", JE.string n )
                        ]
            in
            encodeTyped "type" <|
                JE.object
                    [ ( "moduleNameAndName", Node.encode inner moduleNameAndName )
                    , ( "args", JE.list (Node.encode encode) args )
                    ]

        Tuple t ->
            encodeTyped "tuple" <|
                JE.object
                    [ ( "values", JE.list (Node.encode encode) t )
                    ]

        FunctionTypeAnnotation left right ->
            encodeTyped "function" <|
                JE.object
                    [ ( "left", Node.encode encode left )
                    , ( "right", Node.encode encode right )
                    ]

        Record recordDefinition generic ->
            encodeTyped "record" <|
                JE.object
                    [ ( "value", encodeRecordDefinition recordDefinition )
                    , ( "generic"
                      , generic
                            |> Maybe.map (Node.encode JE.string)
                            |> Maybe.withDefault JE.null
                      )
                    ]


encodeRecordDefinition : RecordDefinition -> Value
encodeRecordDefinition =
    JE.list (Node.encode encodeRecordField)


encodeRecordField : RecordField -> Value
encodeRecordField ( name, ref ) =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "typeAnnotation", Node.encode encode ref )
        ]


decodeModuleNameAndName : Decoder ( ModuleName, String )
decodeModuleNameAndName =
    JD.map2 Tuple.pair
        (JD.field "moduleName" <| ModuleName.decoder)
        (JD.field "name" <| JD.string)


{-| JSON decoder for a `TypeAnnotation` syntax element.
-}
decoder : Decoder TypeAnnotation
decoder =
    JD.lazy
        (\() ->
            decodeTyped
                [ ( "generic", JD.map GenericType (JD.field "value" JD.string) )
                , ( "type"
                  , JD.map2 Type
                        (JD.field "moduleNameAndName" <| Node.decoder decodeModuleNameAndName)
                        (JD.field "args" (JD.list nestedDecoder))
                  )
                , ( "tuple", JD.map Tuple (JD.field "values" (JD.list nestedDecoder)) )
                , ( "function"
                  , JD.map2 FunctionTypeAnnotation
                        (JD.field "left" nestedDecoder)
                        (JD.field "right" nestedDecoder)
                  )
                , ( "record"
                  , JD.map2 Record
                        (JD.field "value" recordDefinitionDecoder)
                        (JD.field "generic"
                            (JD.oneOf
                                [ JD.null Nothing
                                , Node.decoder JD.string |> JD.map Just
                                ]
                            )
                        )
                  )
                ]
        )


nestedDecoder : Decoder (Node TypeAnnotation)
nestedDecoder =
    JD.lazy (\() -> Node.decoder decoder)


recordDefinitionDecoder : Decoder RecordDefinition
recordDefinitionDecoder =
    JD.lazy (\() -> JD.list <| Node.decoder recordFieldDecoder)


recordFieldDecoder : Decoder RecordField
recordFieldDecoder =
    JD.lazy
        (\() ->
            JD.map2 Tuple.pair
                (JD.field "name" <| Node.decoder JD.string)
                (JD.field "typeAnnotation" nestedDecoder)
        )
