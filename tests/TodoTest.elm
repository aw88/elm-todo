module TodoTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Todo exposing (..)


suite : Test
suite =
    describe "Todo"
        [ describe "notTodo"
            [ test "returns `Complete` when called with `Incomplete`" <|
                \_ ->
                    Expect.equal Complete (notTodo Incomplete)
            , test "returns `Incomplete` when called with `Complete`" <|
                \_ ->
                    Expect.equal Incomplete (notTodo Complete)
            ]
        , describe "toggleTodo"
            [ test "toggles the targetted Todo item's status" <|
                \_ ->
                    let
                        todo =
                            { id = 1, label = "Todo", status = Incomplete }
                    in
                    [ todo ]
                        |> toggleTodo 1
                        |> Expect.equal [ { todo | status = Complete } ]
            , test "leaves other Todo items untouched" <|
                \_ ->
                    let
                        todos =
                            [ { id = 1, label = "Todo 1", status = Incomplete }
                            , { id = 2, label = "Todo 2", status = Incomplete }
                            ]
                    in
                    todos
                        |> toggleTodo 2
                        |> Expect.equal
                            [ { id = 1, label = "Todo 1", status = Incomplete }
                            , { id = 2, label = "Todo 2", status = Complete }
                            ]
            ]
        ]
