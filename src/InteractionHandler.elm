module InteractionHandler exposing
    ( InteractionHandler
    , animate
    , check
    , handle
    , mouseDown
    , mouseMove
    , mouseUp
    )

import Logic
import Types exposing (Model, Msg, State(..))


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


mouseDown : InteractionHandler ( Float, Float )
mouseDown state =
    case state of
        WaitForUserInput ->
            Just Logic.mouseDown

        _ ->
            Nothing


mouseMove : InteractionHandler ( Float, Float )
mouseMove state =
    case state of
        Rotating source ->
            Just (Logic.rotation source)

        TransformStart cell position ->
            Just (Logic.transformStart cell position)

        Transforming cell transformation source ->
            Just (Logic.transforming cell transformation source)

        _ ->
            Nothing


mouseUp : InteractionHandler ( Float, Float )
mouseUp state =
    case state of
        Initial ->
            Just (\_ m -> Logic.startAnimationStart m)

        TransformStart _ _ ->
            Just (\_ m -> ( { m | state = WaitForUserInput }, Cmd.none ))

        Transforming _ transformation _ ->
            Just (\_ m -> Logic.transformationAnimationStart transformation m)

        Rotating _ ->
            Just (\_ m -> Logic.rotationEnd m)

        _ ->
            Nothing


animate : InteractionHandler Float
animate state =
    case state of
        Initial ->
            Just Logic.startAnimation

        Starting animation ->
            Just (Logic.startingAnimation animation)

        Ending animation ->
            Just (Logic.endingAnimation animation)

        Animating transformation transformations animation ->
            Just (Logic.transformationAnimation transformation transformations animation)

        _ ->
            Nothing
