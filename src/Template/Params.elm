module Template.Params exposing (Params, fromDict, fromList, get, interpolate)

import Dict exposing (Dict)


type Params
    = Params (Dict String String)


get : String -> Params -> Maybe String
get key (Params dict) =
    Dict.get key dict


interpolate : String -> String -> Params -> String
interpolate key ifMissing params =
    case params |> get key of
        Just replacement ->
            replacement
        
        Nothing ->
            ifMissing


fromDict : Dict String String -> Params
fromDict =
    Params


fromList : List ( String, String ) -> Params
fromList =
    fromDict << Dict.fromList
