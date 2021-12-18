module Template.Suite exposing (suite)

import Expect
import Fuzz exposing (string)
import Template
import Template.Params as Params
import Test exposing (..)


suite : Test
suite =
    describe "Template"
        [ test "render" <|
            \_ ->
                let
                    params =
                        Params.fromList
                            [ ( "name", "World" )
                            ]

                    want =
                        "Hello, World!"
                in
                Expect.equal
                    want
                    (Template.renderWithDefault params "Hello, {{name}}!")

        , test "missing keys are removed from result by default" <|
            \_ ->
                let
                    params =
                        Params.fromList []

                    want =
                        "the dog is "
                in
                Expect.equal
                    want
                    (Template.renderWithDefault params "the dog is {{barking}}")

        , test "escape left bracket" <|
            \_ ->
                let
                    params =
                        Params.fromList []
                in
                Expect.equal
                    "Print out the {{variable}}"
                    (Template.renderWithDefault params "Print out the \\{\\{variable}}")

        , test "escape itself" <|
            \_ ->
                let
                    params =
                        Params.fromList []
                in
                Expect.equal
                    "single backslash: \\"
                    (Template.renderWithDefault params "single backslash: \\\\")

        , fuzz string "renders any value in params" <|
            \str ->
                let
                    params =
                        Params.fromList [( "rand", str )]

                    t =
                        Template.compile Template.defaultOptions "I can show {{rand}}"
                in
                ("I can show " ++ str)
                    |> Expect.equal (t |> Template.apply params)

        ]
