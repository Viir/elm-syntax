module Elm.Parser.ExpressionV2Tests exposing (all)

import Elm.Parser.ExpressionV2 exposing (..)
import Elm.Syntax.DestructurePattern exposing (DestructurePattern(..))
import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (..)
import Expect
import Parser.Advanced as Parser
import Test exposing (..)


all : Test
all =
    describe "ExpressionTests"
        [ test "empty" <|
            \() ->
                parseExpression ""
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "Integer literal" <|
            \() ->
                parseExpression "101"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (IntegerLiteral 101))
        , Test.skip <|
            test "Float literal" <|
                \() ->
                    parseExpression "0.1001"
                        |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FloatLiteral 0.1001))
        , test "should not parse float literal that starts with ." <|
            \() ->
                parseExpression ".1001"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "String literal" <|
            \() ->
                parseExpression "\"Bar\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (StringLiteral SingleQuote "Bar"))
        , test "character literal" <|
            \() ->
                parseExpression "'c'"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (CharLiteral 'c'))
        , test "should not parse character literal with multiple characters" <|
            \() ->
                parseExpression "'abc'"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "should not parse empty character literal" <|
            \() ->
                parseExpression "''"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "should parse character literal with multiple characters if the first one is escaping a character" <|
            \() ->
                parseExpression "'\\''"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (CharLiteral '\''))
        , test "String literal multiline" <|
            \() ->
                parseExpression "\"\"\"Bar foo \n a\"\"\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 6 } } (StringLiteral TripleQuote "Bar foo \n a"))
        , test "Regression test for multiline strings with backslashes" <|
            \() ->
                parseExpression "\"\"\"\\{\\}\"\"\""
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "Regression test 2 for multiline strings with backslashes" <|
            \() ->
                parseExpression "\"\"\"\\\\{\\\\}\"\"\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (StringLiteral TripleQuote "\\{\\}"))
        , test "Regression test 3 for multiline strings with backslashes" <|
            \() ->
                parseExpression "\"\"\"\\\\a-blablabla-\\\\b\"\"\""
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 24 } } (StringLiteral TripleQuote "\\a-blablabla-\\b"))
        , test "tuple expression" <|
            \() ->
                parseExpression "(1,2)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (TupleExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (IntegerLiteral 1)
                                , Node { start = { row = 1, column = 4 }, end = { row = 1, column = 5 } } (IntegerLiteral 2)
                                ]
                            )
                        )
        , test "Prefix operator" <|
            \() ->
                parseExpression "(+)"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (PrefixOperator "+"))
        , test "Prefix operator (multi-characters)" <|
            \() ->
                parseExpression "(++)"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (PrefixOperator "++"))
        , test "Type expression for upper case" <|
            \() ->
                parseExpression "Bar"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "Bar"))
        , test "Type expression for lower case" <|
            \() ->
                parseExpression "bar"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "bar"))
        , test "Type expression for lower case but qualified" <|
            \() ->
                parseExpression "Bar.foo"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [ "Bar" ] "foo"))
        , test "parenthesizedExpression" <|
            \() ->
                parseExpression "(bar)"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (TupleExpression
                                [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 5 } } (FunctionOrValue [] "bar")
                                ]
                            )
                        )
        , Test.skip <|
            test "application expression" <|
                \() ->
                    parseExpression "List.concat []"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } } <|
                                FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } } <| FunctionOrValue [ "List" ] "concat")
                                    [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 15 } } <| ListLiteral []
                                    ]
                            )
        , test "application expression with operator" <|
            \() ->
                parseExpression "model + 1"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } }
                            (Operation "+"
                                Left
                                (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } } (FunctionOrValue [] "model"))
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (IntegerLiteral 1))
                            )
                        )
        , Test.skip <|
            test "application expression 2" <|
                \() ->
                    parseExpression "(\"\", always (List.concat [ [ fileName ], [] ]))"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 48 } } <|
                                TupleExpression
                                    [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 4 } } <| StringLiteral SingleQuote ""
                                    , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 47 } } <|
                                        FunctionCall
                                            (Node { start = { row = 1, column = 6 }, end = { row = 1, column = 12 } } <| FunctionOrValue [] "always")
                                            [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 47 } } <|
                                                TupleExpression
                                                    [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 46 } } <|
                                                        FunctionCall
                                                            (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } } <|
                                                                FunctionOrValue [ "List" ] "concat"
                                                            )
                                                            [ Node { start = { row = 1, column = 26 }, end = { row = 1, column = 46 } } <|
                                                                ListLiteral
                                                                    [ Node { start = { row = 1, column = 28 }, end = { row = 1, column = 40 } } <|
                                                                        ListLiteral
                                                                            [ Node { start = { row = 1, column = 30 }, end = { row = 1, column = 38 } } <|
                                                                                FunctionOrValue [] "fileName"
                                                                            ]
                                                                    , Node { start = { row = 1, column = 42 }, end = { row = 1, column = 44 } } <|
                                                                        ListLiteral []
                                                                    ]
                                                            ]
                                                    ]
                                            ]
                                    ]
                            )
        , Test.skip <|
            test "unit application" <|
                \() ->
                    parseExpression "Task.succeed ()"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (FunctionOrValue [ "Task" ] "succeed"))
                                    [ Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } (TupleExpression []) ]
                                )
                            )
        , Test.skip <|
            test "Function call" <|
                \() ->
                    parseExpression "foo bar"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                    [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "bar") ]
                                )
                            )
        , test "ifBlockExpression" <|
            \() ->
                parseExpression "if True then foo else bar"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 26 } }
                            (If
                                (Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "True"))
                                (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 17 } } (FunctionOrValue [] "foo"))
                                (Node { start = { row = 1, column = 23 }, end = { row = 1, column = 26 } } (FunctionOrValue [] "bar"))
                            )
                        )
        , test "Multi-line if expression" <|
            \() ->
                parseExpression """
    if x then
        1
    else
        2"""
                    |> expectAst
                        (Node { start = { row = 2, column = 5 }, end = { row = 5, column = 10 } }
                            (If
                                (Node { start = { row = 2, column = 8 }, end = { row = 2, column = 9 } } (FunctionOrValue [] "x"))
                                (Node { start = { row = 3, column = 9 }, end = { row = 3, column = 10 } } (IntegerLiteral 1))
                                (Node { start = { row = 5, column = 9 }, end = { row = 5, column = 10 } } (IntegerLiteral 2))
                            )
                        )
        , Test.skip <|
            test "nestedIfExpression" <|
                \() ->
                    parseExpression "if True then if False then foo else baz else bar"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 49 } }
                                (If
                                    (Node { start = { row = 1, column = 4 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "True"))
                                    (Node { start = { row = 1, column = 14 }, end = { row = 1, column = 40 } }
                                        (If
                                            (Node { start = { row = 1, column = 17 }, end = { row = 1, column = 22 } } (FunctionOrValue [] "False"))
                                            (Node { start = { row = 1, column = 28 }, end = { row = 1, column = 31 } } (FunctionOrValue [] "foo"))
                                            (Node { start = { row = 1, column = 37 }, end = { row = 1, column = 40 } } (FunctionOrValue [] "baz"))
                                        )
                                    )
                                    (Node { start = { row = 1, column = 46 }, end = { row = 1, column = 49 } } (FunctionOrValue [] "bar"))
                                )
                            )
        , test "recordExpression" <|
            \() ->
                parseExpression "{ model = 0, view = view, update = update }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 44 } }
                            (Record
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 12 } }
                                    ( Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model"
                                    , Node { start = { row = 1, column = 11 }, end = { row = 1, column = 12 } } (IntegerLiteral 0)
                                    )
                                , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 25 } }
                                    ( Node { start = { row = 1, column = 14 }, end = { row = 1, column = 18 } } "view"
                                    , Node { start = { row = 1, column = 21 }, end = { row = 1, column = 25 } } (FunctionOrValue [] "view")
                                    )
                                , Node { start = { row = 1, column = 27 }, end = { row = 1, column = 43 } }
                                    ( Node { start = { row = 1, column = 27 }, end = { row = 1, column = 33 } } "update"
                                    , Node { start = { row = 1, column = 36 }, end = { row = 1, column = 42 } } (FunctionOrValue [] "update")
                                    )
                                ]
                            )
                        )
        , test "Nested recordExpression" <|
            \() ->
                parseExpression "{ noFields = {}, oneField = { a = 1 }, twoFields = { b = 2, c = 3 } }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 70 } }
                            (Record
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 16 } }
                                    ( Node { start = { row = 1, column = 3 }, end = { row = 1, column = 11 } } "noFields"
                                    , Node { start = { row = 1, column = 14 }, end = { row = 1, column = 16 } } (Record [])
                                    )
                                , Node { start = { row = 1, column = 18 }, end = { row = 1, column = 38 } }
                                    ( Node { start = { row = 1, column = 18 }, end = { row = 1, column = 26 } } "oneField"
                                    , Node { start = { row = 1, column = 29 }, end = { row = 1, column = 38 } }
                                        (Record
                                            [ Node { start = { row = 1, column = 31 }, end = { row = 1, column = 36 } }
                                                ( Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } "a"
                                                , Node { start = { row = 1, column = 35 }, end = { row = 1, column = 36 } } (IntegerLiteral 1)
                                                )
                                            ]
                                        )
                                    )
                                , Node { start = { row = 1, column = 40 }, end = { row = 1, column = 69 } }
                                    ( Node { start = { row = 1, column = 40 }, end = { row = 1, column = 49 } } "twoFields"
                                    , Node { start = { row = 1, column = 52 }, end = { row = 1, column = 68 } }
                                        (Record
                                            [ Node { start = { row = 1, column = 54 }, end = { row = 1, column = 59 } }
                                                ( Node { start = { row = 1, column = 54 }, end = { row = 1, column = 55 } } "b"
                                                , Node { start = { row = 1, column = 58 }, end = { row = 1, column = 59 } } (IntegerLiteral 2)
                                                )
                                            , Node { start = { row = 1, column = 61 }, end = { row = 1, column = 67 } }
                                                ( Node { start = { row = 1, column = 61 }, end = { row = 1, column = 62 } } "c"
                                                , Node { start = { row = 1, column = 65 }, end = { row = 1, column = 66 } } (IntegerLiteral 3)
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
        , Test.skip <|
            test "recordExpression with comment" <|
                \() ->
                    parseExpression "{ foo = 1 -- bar\n , baz = 2 }"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 2, column = 13 } }
                                (Record
                                    [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 10 } }
                                        ( Node { start = { row = 1, column = 3 }, end = { row = 1, column = 6 } } "foo"
                                        , Node { start = { row = 1, column = 9 }, end = { row = 1, column = 10 } } (IntegerLiteral 1)
                                        )
                                    , Node { start = { row = 2, column = 4 }, end = { row = 2, column = 12 } }
                                        ( Node { start = { row = 2, column = 4 }, end = { row = 2, column = 7 } } "baz"
                                        , Node { start = { row = 2, column = 10 }, end = { row = 2, column = 11 } } (IntegerLiteral 2)
                                        )
                                    ]
                                )
                            )
        , test "Empty list expression" <|
            \() ->
                parseExpression "[]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                            (ListLiteral [])
                        )
        , test "listExpression" <|
            \() ->
                parseExpression "[ 1, 2 ]"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                            (ListLiteral
                                [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (IntegerLiteral 1)
                                , Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (IntegerLiteral 2)
                                ]
                            )
                        )
        , Test.skip <|
            test "listExpression singleton with comment" <|
                \() ->
                    parseExpression "[ 1 {- Foo-} ]"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 15 } }
                                (ListLiteral
                                    [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (IntegerLiteral 1)
                                    ]
                                )
                            )
        , Test.skip <|
            test "listExpression empty with comment" <|
                \() ->
                    parseExpression "[{-| Foo -}]"
                        |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 13 } } (ListLiteral []))
        , test "qualified expression" <|
            \() ->
                parseExpression "Html.text"
                    |> expectAst (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 10 } } (FunctionOrValue [ "Html" ] "text"))
        , Test.skip <|
            test "record access" <|
                \() ->
                    parseExpression "foo.bar"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                (RecordAccess
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                    (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                )
                            )
        , Test.skip <|
            test "multiple record access operations" <|
                \() ->
                    parseExpression "foo.bar.baz"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                (RecordAccess
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } }
                                        (RecordAccess
                                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "foo"))
                                            (Node { start = { row = 1, column = 5 }, end = { row = 1, column = 8 } } "bar")
                                        )
                                    )
                                    (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "baz")
                                )
                            )
        , Test.skip <|
            test "multiple record access operations with module name" <|
                \() ->
                    parseExpression "A.B.foo.bar.baz"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 16 } }
                                (RecordAccess
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 12 } }
                                        (RecordAccess
                                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [ "A", "B" ] "foo"))
                                            (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 12 } } "bar")
                                        )
                                    )
                                    (Node { start = { row = 1, column = 13 }, end = { row = 1, column = 16 } } "baz")
                                )
                            )
        , test "record update" <|
            \() ->
                parseExpression "{ model | count = 1, loading = True }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                            (RecordUpdate
                                (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                    ( Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                    , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (IntegerLiteral 1)
                                    )
                                )
                                [ Node { start = { row = 1, column = 22 }, end = { row = 1, column = 37 } }
                                    ( Node { start = { row = 1, column = 22 }, end = { row = 1, column = 29 } } "loading"
                                    , Node { start = { row = 1, column = 32 }, end = { row = 1, column = 36 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , Test.skip <|
            --  Ranges are off at the moment.
            test "record update with one field"
            <|
                \() ->
                    parseExpression "{ model | count = 1 }"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                                (RecordUpdate
                                    (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } } "model")
                                    (Node { start = { row = 1, column = 11 }, end = { row = 1, column = 20 } }
                                        ( Node { start = { row = 1, column = 11 }, end = { row = 1, column = 16 } } "count"
                                        , Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (IntegerLiteral 1)
                                        )
                                    )
                                    []
                                )
                            )
        , test "Should not parse record update without assignments" <|
            \() ->
                parseExpression "{ model | }"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "record update no spacing" <|
            \() ->
                parseExpression "{model| count = 1, loading = True }"
                    |> expectAst
                        (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 36 } }
                            (RecordUpdate
                                (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 7 } } "model")
                                (Node { start = { row = 1, column = 9 }, end = { row = 1, column = 18 } }
                                    ( Node { start = { row = 1, column = 9 }, end = { row = 1, column = 14 } } "count"
                                    , Node { start = { row = 1, column = 17 }, end = { row = 1, column = 18 } } (IntegerLiteral 1)
                                    )
                                )
                                [ Node { start = { row = 1, column = 20 }, end = { row = 1, column = 35 } }
                                    ( Node { start = { row = 1, column = 20 }, end = { row = 1, column = 27 } } "loading"
                                    , Node { start = { row = 1, column = 30 }, end = { row = 1, column = 34 } } (FunctionOrValue [] "True")
                                    )
                                ]
                            )
                        )
        , test "Record access function" <|
            \() ->
                parseExpression ".name"
                    |> expectAst
                        (Node
                            { start = { row = 1, column = 1 }, end = { row = 1, column = 6 } }
                            (RecordAccessFunction "name")
                        )
        , Test.skip <|
            test "Record access function (in application)" <|
                \() ->
                    parseExpression "List.map .name people"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 22 } }
                                (FunctionCall (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } } (FunctionOrValue [ "List" ] "map"))
                                    [ Node { start = { row = 1, column = 10 }, end = { row = 1, column = 15 } } (RecordAccessFunction "name")
                                    , Node { start = { row = 1, column = 16 }, end = { row = 1, column = 22 } } (FunctionOrValue [] "people")
                                    ]
                                )
                            )
        , Test.skip <|
            test "record access direct" <|
                \() ->
                    parseExpression "(.spaceEvenly Internal.Style.classes)"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 38 } }
                                (TupleExpression
                                    [ Node { start = { row = 1, column = 2 }, end = { row = 1, column = 37 } }
                                        (FunctionCall (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 14 } } (RecordAccessFunction "spaceEvenly"))
                                            [ Node { start = { row = 1, column = 15 }, end = { row = 1, column = 37 } } (FunctionOrValue [ "Internal", "Style" ] "classes")
                                            ]
                                        )
                                    ]
                                )
                            )
        , Test.skip <|
            test "positive integer should be invalid" <|
                \() ->
                    parseExpression "+1"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
        , Test.skip <|
            test "expression ending with an operator should not be valid" <|
                \() ->
                    parseExpression "1++"
                        |> Result.toMaybe
                        |> Expect.equal Nothing
        , Test.skip <|
            test "prefix notation" <|
                \() ->
                    parseExpression "(::) x"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                                (FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 5 } } (PrefixOperator "::"))
                                    [ Node { start = { row = 1, column = 6 }, end = { row = 1, column = 7 } } (FunctionOrValue [] "x") ]
                                )
                            )
        , Test.skip <|
            test "negated expression for value" <|
                \() ->
                    parseExpression "-x"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 3 } }
                                (Negation (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 3 } } (FunctionOrValue [] "x")))
                            )
        , Test.skip <|
            test "negated expression in application" <|
                \() ->
                    parseExpression "toFloat -5"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } }
                                (FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "toFloat"))
                                    [ Node { start = { row = 1, column = 9 }, end = { row = 1, column = 11 } }
                                        (Negation (Node { start = { row = 1, column = 10 }, end = { row = 1, column = 11 } } (IntegerLiteral 5)))
                                    ]
                                )
                            )
        , Test.skip <|
            test "negated expression for parenthesized" <|
                \() ->
                    parseExpression "-(x - y)"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 9 } }
                                (Negation
                                    (Node { start = { row = 1, column = 2 }, end = { row = 1, column = 9 } }
                                        (TupleExpression
                                            [ Node { start = { row = 1, column = 3 }, end = { row = 1, column = 8 } }
                                                (FunctionCall
                                                    (Node { start = { row = 1, column = 3 }, end = { row = 1, column = 4 } } (FunctionOrValue [] "x"))
                                                    [ Node { start = { row = 1, column = 5 }, end = { row = 1, column = 7 } } (Operator "-")
                                                    , Node { start = { row = 1, column = 7 }, end = { row = 1, column = 8 } } (FunctionOrValue [] "y")
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
        , Test.skip <|
            test "function with higher order" <|
                \() ->
                    parseExpression "chompWhile (\\c -> c == ' ' || c == '\\n' || c == '\\r')"
                        |> expectAst
                            (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 54 } }
                                (FunctionCall
                                    (Node { start = { row = 1, column = 1 }, end = { row = 1, column = 11 } } (FunctionOrValue [] "chompWhile"))
                                    [ Node { start = { row = 1, column = 12 }, end = { row = 1, column = 54 } }
                                        (TupleExpression
                                            [ Node { start = { row = 1, column = 13 }, end = { row = 1, column = 53 } }
                                                (LambdaExpression
                                                    { expression =
                                                        Node { start = { row = 1, column = 19 }, end = { row = 1, column = 53 } }
                                                            (FunctionCall
                                                                (Node { start = { row = 1, column = 19 }, end = { row = 1, column = 20 } } (FunctionOrValue [] "c"))
                                                                [ Node { start = { row = 1, column = 21 }, end = { row = 1, column = 23 } } (Operator "==")
                                                                , Node { start = { row = 1, column = 24 }, end = { row = 1, column = 27 } } (CharLiteral ' ')
                                                                , Node { start = { row = 1, column = 28 }, end = { row = 1, column = 30 } } (Operator "||")
                                                                , Node { start = { row = 1, column = 31 }, end = { row = 1, column = 32 } } (FunctionOrValue [] "c")
                                                                , Node { start = { row = 1, column = 33 }, end = { row = 1, column = 35 } } (Operator "==")
                                                                , Node { start = { row = 1, column = 36 }, end = { row = 1, column = 40 } } (CharLiteral '\n')
                                                                , Node { start = { row = 1, column = 41 }, end = { row = 1, column = 43 } } (Operator "||")
                                                                , Node { start = { row = 1, column = 44 }, end = { row = 1, column = 45 } } (FunctionOrValue [] "c")
                                                                , Node { start = { row = 1, column = 46 }, end = { row = 1, column = 48 } } (Operator "==")
                                                                , Node { start = { row = 1, column = 49 }, end = { row = 1, column = 53 } } (CharLiteral '\u{000D}')
                                                                ]
                                                            )
                                                    , firstArg = Node { start = { row = 1, column = 14 }, end = { row = 1, column = 15 } } (VarPattern_ "c")
                                                    , restOfArgs = []
                                                    }
                                                )
                                            ]
                                        )
                                    ]
                                )
                            )
        ]


parseExpression : String -> Result (List (Parser.DeadEnd c Elm.Parser.ExpressionV2.Problem)) (Node Expression)
parseExpression source =
    Parser.run expression source


expectAst : Node Expression -> Result (List (Parser.DeadEnd c Elm.Parser.ExpressionV2.Problem)) (Node Expression) -> Expect.Expectation
expectAst expected result =
    case result of
        Err problems ->
            ("Expected the source to be parsed correctly" :: List.map deadEndToString problems)
                |> String.join "\n"
                |> Expect.fail

        Ok actual ->
            actual
                |> Expect.equal expected
