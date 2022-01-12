module Template.Params exposing (Params, fromDict, fromList, get, interpolate)

{-| `Params` is some "newtype" wrapper of `Dict String String`, designed to be
used especially for rendering templates.

You usually don't need to use functions defined here directly except when
getting `Params` value with [`fromDict`](#fromDict) or [`fromList`](#fromList).

# Params
@docs Params

# Construction
@docs fromDict, fromList

# Interpolate
@docs interpolate

# Query
@docs get
-}

import Dict exposing (Dict)


{-| A set of parameters for template function. -}
type Params
    = Params (Dict String String)


{-| Try to get a value associated with given key from `Params`. -}
get : String -> Params -> Maybe String
get key (Params dict) =
    Dict.get key dict


{-| Takes key and default value, returns associated value if one is found.
Otherwise returns default value.

```elm
params =
    fromList
        [ ( "database", "MySQL" )
        , ( "protocol", "HTTPS" )
        ]

params |> interpolate "protocol" "???"      -- => "HTTPS"
params |> interpolate "size" "NOT FOUND"    -- => "NOT FOUND"
```
-}
interpolate : String -> String -> Params -> String
interpolate key ifMissing params =
    case params |> get key of
        Just replacement ->
            replacement
        
        Nothing ->
            ifMissing


{-| Wrap a `Dict` into `Params`.

```elm
params =
    fromDict <|
        Dict.fromList
            [ ( "css", "Cascading Style Sheet" )
            , ( "html", "Hyper Text Markup Language" )
            ]
```
-}
fromDict : Dict String String -> Params
fromDict =
    Params


{-| Create `Params` by converting given `List`.

```elm
params =
    fromList
        [ ( "domain", "web development" )
        , ( "type", "static" )
        ]
```
-}
fromList : List ( String, String ) -> Params
fromList =
    fromDict << Dict.fromList
