module Template exposing (Options, Template, run, compile, defaultOptions, render, renderWithDefault)

{-| This module provides Mustache (or Handlebars) like template syntax.

# Template
@docs Template, Options, compile, defaultOptions

# Rendering
@docs run, render, renderWithDefault
-}

import Template.Params as Params exposing (Params)


{-| A compiled template function, accepts [`Params`](./Template-Params#Params) and render string.

Can be configured by [`Options`](#Options).
-}
type Template
    = Template (Params -> String)


{-| An `Options` value is used to configure rendering of a template.

Currently supports:

* `trim` - Trim spaces around interpolated value. Defaults to `True`
* `placeholder` - Specify placeholder text when no associated value is found in
template parameter. Defaults to empty string (`""`).
-}
type alias Options =
    { trim : Bool
    , placeholder : String
    }


{-| The default configuration for a template. -}
defaultOptions : Options
defaultOptions =
    { trim = True
    , placeholder = ""
    }


type alias State =
    { chunk : String
    , output : String
    , bracket : Bool
    , last : Maybe Char
    }


{-| Compiles string into a template function.

To render template with some parameters, use `run`.

```elm
tmpl : Template
tmpl =
    compile defaultOptions <|
        "Hello, {{name}}"
```
-}
compile : Options -> String -> Template
compile options template =
    Template <|
        \params ->
            render params options template


{-| Apply template function to some parameters and get rendered text.

See [`Template.Params`](./Template-Params) module to how you can interact with `Params` value.

```elm
params : Params
params =
    Params.fromList
        [ ( "browser", "Firefox" )
        , ( "language", "JavaScript" )
        ]

t : Template
t =
    compile defaultOptions "My favorite is {{browser}} and I can use {{language}}!"

str : String
str =
    run params t
    -- "My favorite is Firefox and I can use JavaScript!"
```
-}
run : Params -> Template -> String
run params (Template f) =
    f params


appendChar : Char -> String -> String
appendChar char str =
    String.append str (String.fromChar char)


{-| Same as `render` but uses default options.

The quickest way to run!

```elm
renderWithDefault
    (Params.fromList [( "animal", "dog" )])
    "How this {{animal}} sleep?"
```
-}
renderWithDefault : Params -> String -> String
renderWithDefault params template =
    render params defaultOptions template


{-| Render template string directly with given parameters and options.

Useful when you do not need the compiled function. A quicker way to run!

```elm
render
    (Params.fromList [( "food", "Okonomiyaki" )])
    defaultOptions
    "Give me some {{food}}"
```
-}
render : Params -> Options -> String -> String
render params options template =
    let
        { chunk, output } =
            renderHelp
                params
                options
                template
                initialState
    in
    output ++ chunk


renderHelp : Params -> Options -> String -> State -> State
renderHelp params options template init =
    case String.uncons template of
        Nothing ->
            init

        Just ( char, remaining ) ->
            let
                newState =
                    update params options char init
            in
            renderHelp params options remaining newState


initialState : State
initialState =
    { chunk = ""
    , output = ""
    , bracket = False
    , last = Nothing
    }


update : Params -> Options -> Char -> State -> State
update params options char state =
    let
        escape =
            state.last == Just '\\'
    in
    case char of
        '{' ->
            if escape then
                { state
                    | chunk = state.chunk |> appendChar char
                    , last = Nothing
                }

            else
                case state.last of
                    Just '{' ->
                        { state
                            | bracket = True
                            , last = Nothing
                            , chunk = ""
                            , output = state.output ++ state.chunk
                        }

                    _ ->
                        { state
                            | last = Just '{'
                        }

        '}' ->
            if state.bracket then
                case state.last of
                    Just '}' ->
                        let
                            key =
                                state.chunk

                            expanded =
                                Params.interpolate
                                    key
                                    options.placeholder
                                    params

                            trimmed =
                                if options.trim then
                                    String.trim expanded
                                else
                                    expanded
                        in
                        { state
                            | bracket = False
                            , last = Nothing
                            , chunk = ""
                            , output = state.output ++ trimmed
                        }

                    _ ->
                        { state
                            | last = Just '}'
                        }

            else
                { state
                    | chunk = state.chunk |> appendChar char
                    , last = Just char
                }

        '\\' ->
            if escape then
                { state
                    | chunk = state.chunk |> appendChar char
                    , last = Nothing
                }
            else
                { state
                    | last = Just char
                }

        _ ->
            { state
                | chunk = state.chunk |> appendChar char
                , last = Just char
            }
