module InteractionHandler exposing (..)

import Types exposing (Model, State, Msg)


type alias InteractionHandler a =
    State -> Maybe (a -> Model -> ( Model, Cmd Msg ))


check : InteractionHandler a -> Model -> Bool
check handler model =
    handler model.state
        |> Maybe.map (always True)
        |> Maybe.withDefault False


handle : InteractionHandler a -> a -> Model -> ( Model, Cmd Msg )
handle handler input model =
    handler model.state
        |> Maybe.map (\fn -> fn input model)
        |> Maybe.withDefault ( model, Cmd.none )
