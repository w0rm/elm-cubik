module Cubik exposing (main)

import Html exposing (Html, div)
import WebGL.Texture as Texture exposing (defaultOptions)
import Window
import Mouse
import Task
import Types exposing (..)
import View exposing (view)
import Decode
import Json.Encode exposing (Value)
import Json.Decode as Decode
import AnimationFrame
import MogeeFont
import InteractionHandler exposing (check, handle, InteractionHandler)
import Time exposing (Time)
import Logic


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Value -> ( Model, Cmd Msg )
init value =
    ( value
        |> Decode.decodeValue Decode.model
        |> Result.withDefault Decode.initial
    , Cmd.batch
        [ Task.perform Resize Window.size
        , Texture.loadWith
            { defaultOptions
                | magnify = Texture.nearest
                , flipY = False
            }
            MogeeFont.fontSrc
            |> Task.attempt FontLoaded
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FontLoaded textureResult ->
            ( { model | font = Result.toMaybe textureResult }
            , Cmd.none
            )

        Transform transformations ->
            Logic.initialTransformationAnimationStart
                transformations
                model

        Resize window ->
            ( { model | window = window }
            , Cmd.none
            )

        Down mouse ->
            handle mouseDown mouse model

        Move mouse ->
            handle mouseMove mouse model

        Up mouse ->
            handle mouseUp mouse model

        Tick diff ->
            handle animate diff { model | time = model.time + diff }


subscriptions : Model -> Sub Msg
subscriptions model =
    [ ( always True, Window.resizes Resize )
    , ( check animate, AnimationFrame.diffs Tick )
    , ( check mouseDown, Mouse.downs Down )
    , ( check mouseMove, Mouse.moves Move )
    , ( check mouseUp, Mouse.ups Up )
    ]
        |> List.filter (Tuple.first >> (|>) model)
        |> List.map Tuple.second
        |> Sub.batch


mouseDown : InteractionHandler Mouse.Position
mouseDown state =
    case state of
        WaitForUserInput ->
            Just Logic.mouseDown

        _ ->
            Nothing


mouseMove : InteractionHandler Mouse.Position
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


mouseUp : InteractionHandler Mouse.Position
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


animate : InteractionHandler Time
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
