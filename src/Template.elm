module Template exposing (Options, Template, apply, compile, defaultOptions, render, renderWithDefault)

import Template.Params as Params exposing (Params)


type Template
    = Template (Params -> String)


type alias Options =
    { trim : Bool
    , placeholder : String
    }


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


compile : Options -> String -> Template
compile options template =
    Template <|
        \params ->
            render params options template


apply : Params -> Template -> String
apply params (Template f) =
    f params


appendChar : Char -> String -> String
appendChar char str =
    String.append str (String.fromChar char)


renderWithDefault : Params -> String -> String
renderWithDefault params template =
    render params defaultOptions template


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
                        in
                        { state
                            | bracket = False
                            , last = Nothing
                            , chunk = ""
                            , output = state.output ++ expanded
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
