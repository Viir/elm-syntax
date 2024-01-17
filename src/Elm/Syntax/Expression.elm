module Elm.Syntax.Expression exposing
    ( Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation
    , functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication
    , encode, encodeFunction
    )

{-| This syntax represents all that you can express in Elm.
Although it is a easy and simple language, you can express a lot! See the `Expression` type for all the things you can express.


## Types

@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation


## Functions

@docs functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication


## Serialization

@docs encode, encodeFunction, decoder, functionDecoder

-}

import Elm.Json.Util exposing (encodeTyped)
import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Infix as Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Json.Encode as JE exposing (Value)


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe (Node Documentation)
    , signature : Maybe (Node Signature)
    , declaration : Node FunctionImplementation
    }


{-| Get the full range of a function
-}
functionRange : Function -> Range
functionRange function =
    let
        { name, expression } =
            Node.value function.declaration

        startRange : Range
        startRange =
            case function.documentation of
                Just documentation ->
                    Node.range documentation

                Nothing ->
                    case function.signature of
                        Just (Node _ value) ->
                            Node.range value.name

                        Nothing ->
                            Node.range name
    in
    { start = startRange.start
    , end = (Node.range expression).end
    }


{-| Type alias for a function's implementation
-}
type alias FunctionImplementation =
    { name : Node String
    , arguments : List (Node Pattern)
    , expression : Node Expression
    }


{-| Custom type for all expressions such as:

  - `Unit`: `()`
  - `Application`: `add a b`
  - `OperatorApplication`: `a + b`
  - `FunctionOrValue`: `add` or `True`
  - `IfBlock`: `if a then b else c`
  - `PrefixOperator`: `(+)`
  - `Operator`: `+` (not possible to get in practice)
  - `Integer`: `42`
  - `Hex`: `0x1F`
  - `Floatable`: `42.0`
  - `Negation`: `-a`
  - `Literal`: `"text"`
  - `CharLiteral`: `'a'`
  - `TupledExpression`: `(a, b)` or `(a, b, c)`
  - `ParenthesizedExpression`: `(a)`
  - `LetExpression`: `let a = 4 in a`
  - `CaseExpression`: `case a of` followed by pattern matches
  - `LambdaExpression`: `(\a -> a)`
  - `RecordExpr`: `{ name = "text" }`
  - `ListExpr`: `[ x, y ]`
  - `RecordAccess`: `a.name`
  - `RecordAccessFunction`: `.name`
  - `RecordUpdateExpression`: `{ a | name = "text" }`
  - `GLSLExpression`: `[glsl| ... |]`

-}
type Expression
    = UnitExpr
    | Application (List (Node Expression))
    | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
    | FunctionOrValue ModuleName String
    | IfBlock (Node Expression) (Node Expression) (Node Expression)
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation (Node Expression)
    | Literal String
    | CharLiteral Char
    | TupledExpression (List (Node Expression))
    | ParenthesizedExpression (Node Expression)
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List (Node RecordSetter))
    | ListExpr (List (Node Expression))
    | RecordAccess (Node Expression) (Node String)
    | RecordAccessFunction String
    | RecordUpdateExpression (Node String) (List (Node RecordSetter))
    | GLSLExpression String


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( Node String, Node Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Node LetDeclaration)
    , expression : Node Expression
    }


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Node Pattern) (Node Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List (Node Pattern)
    , expression : Node Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Node Expression
    , cases : Cases
    }


{-| A case in a case block
-}
type alias Case =
    ( Node Pattern, Node Expression )


{-| Type alias for a list of cases
-}
type alias Cases =
    List Case


{-| Check whether an expression is a lambda-expression
-}
isLambda : Expression -> Bool
isLambda e =
    case e of
        LambdaExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is a let-expression
-}
isLet : Expression -> Bool
isLet e =
    case e of
        LetExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an if-else-expression
-}
isIfElse : Expression -> Bool
isIfElse e =
    case e of
        IfBlock _ _ _ ->
            True

        _ ->
            False


{-| Check whether an expression is a case-expression
-}
isCase : Expression -> Bool
isCase e =
    case e of
        CaseExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an operator application expression
-}
isOperatorApplication : Expression -> Bool
isOperatorApplication e =
    case e of
        OperatorApplication _ _ _ _ ->
            True

        _ ->
            False



-- Serialization


{-| Encode an `Expression` syntax element to JSON.
-}
encode : Expression -> Value
encode expr =
    case expr of
        UnitExpr ->
            encodeTyped "unit" JE.null

        Application l ->
            encodeTyped "application" (JE.list (Node.encode encode) l)

        OperatorApplication op dir left right ->
            encodeTyped "operatorapplication" (encodeOperatorApplication op dir left right)

        FunctionOrValue moduleName name ->
            encodeTyped "functionOrValue"
                (JE.object
                    [ ( "moduleName", ModuleName.encode moduleName )
                    , ( "name", JE.string name )
                    ]
                )

        IfBlock c t e ->
            encodeTyped "ifBlock" <|
                JE.object
                    [ ( "clause", Node.encode encode c )
                    , ( "then", Node.encode encode t )
                    , ( "else", Node.encode encode e )
                    ]

        PrefixOperator x ->
            encodeTyped "prefixoperator" (JE.string x)

        Operator x ->
            encodeTyped "operator" (JE.string x)

        Hex h ->
            encodeTyped "hex" (JE.int h)

        Integer x ->
            encodeTyped "integer" (JE.int x)

        Floatable x ->
            encodeTyped "float" (JE.float x)

        Negation x ->
            encodeTyped "negation" (Node.encode encode x)

        Literal x ->
            encodeTyped "literal" (JE.string x)

        CharLiteral c ->
            encodeTyped "charLiteral" (JE.string <| String.fromChar c)

        TupledExpression xs ->
            encodeTyped "tupled" (JE.list (Node.encode encode) xs)

        ListExpr xs ->
            encodeTyped "list" (JE.list (Node.encode encode) xs)

        ParenthesizedExpression x ->
            encodeTyped "parenthesized" (Node.encode encode x)

        LetExpression x ->
            encodeTyped "let" <| encodeLetBlock x

        CaseExpression x ->
            encodeTyped "case" <| encodeCaseBlock x

        LambdaExpression x ->
            encodeTyped "lambda" <| encodeLambda x

        RecordAccess exp name ->
            encodeTyped "recordAccess" <|
                JE.object
                    [ ( "expression", Node.encode encode exp )
                    , ( "name", Node.encode JE.string name )
                    ]

        RecordAccessFunction x ->
            encodeTyped "recordAccessFunction" (JE.string x)

        RecordExpr xs ->
            encodeTyped "record" (JE.list (Node.encode encodeRecordSetter) xs)

        RecordUpdateExpression name updates ->
            encodeTyped "recordUpdate" (encodeRecordUpdate name updates)

        GLSLExpression x ->
            encodeTyped "glsl" (JE.string x)


encodeOperatorApplication : String -> InfixDirection -> Node Expression -> Node Expression -> Value
encodeOperatorApplication operator direction left right =
    JE.object
        [ ( "operator", JE.string operator )
        , ( "direction", Infix.encodeDirection direction )
        , ( "left", Node.encode encode left )
        , ( "right", Node.encode encode right )
        ]


encodeLetBlock : LetBlock -> Value
encodeLetBlock { declarations, expression } =
    JE.object
        [ ( "declarations", JE.list (Node.encode encodeLetDeclaration) declarations )
        , ( "expression", Node.encode encode expression )
        ]


encodeRecordUpdate : Node String -> List (Node RecordSetter) -> Value
encodeRecordUpdate name updates =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "updates", JE.list (Node.encode encodeRecordSetter) updates )
        ]


encodeRecordSetter : RecordSetter -> Value
encodeRecordSetter ( field, expression ) =
    JE.object
        [ ( "field", Node.encode JE.string field )
        , ( "expression", Node.encode encode expression )
        ]


encodeLetDeclaration : LetDeclaration -> Value
encodeLetDeclaration letDeclaration =
    case letDeclaration of
        LetFunction f ->
            encodeTyped "function" (encodeFunction f)

        LetDestructuring pattern expression ->
            encodeTyped "destructuring" (encodeDestructuring pattern expression)


{-| Encode a `Function` syntax element to JSON.
-}
encodeFunction : Function -> Value
encodeFunction { documentation, signature, declaration } =
    JE.object
        [ ( "documentation", Maybe.map (Node.encode Documentation.encode) documentation |> Maybe.withDefault JE.null )
        , ( "signature", Maybe.map (Node.encode Signature.encode) signature |> Maybe.withDefault JE.null )
        , ( "declaration", Node.encode encodeFunctionDeclaration declaration )
        ]


encodeFunctionDeclaration : FunctionImplementation -> Value
encodeFunctionDeclaration { name, arguments, expression } =
    JE.object
        [ ( "name", Node.encode JE.string name )
        , ( "arguments", JE.list (Node.encode Pattern.encode) arguments )
        , ( "expression", Node.encode encode expression )
        ]


encodeDestructuring : Node Pattern -> Node Expression -> Value
encodeDestructuring pattern expression =
    JE.object
        [ ( "pattern", Node.encode Pattern.encode pattern )
        , ( "expression", Node.encode encode expression )
        ]


encodeCaseBlock : CaseBlock -> Value
encodeCaseBlock { cases, expression } =
    JE.object
        [ ( "cases", JE.list encodeCase cases )
        , ( "expression", Node.encode encode expression )
        ]


encodeCase : Case -> Value
encodeCase ( pattern, expression ) =
    JE.object
        [ ( "pattern", Node.encode Pattern.encode pattern )
        , ( "expression", Node.encode encode expression )
        ]


encodeLambda : Lambda -> Value
encodeLambda { args, expression } =
    JE.object
        [ ( "patterns", JE.list (Node.encode Pattern.encode) args )
        , ( "expression", Node.encode encode expression )
        ]
