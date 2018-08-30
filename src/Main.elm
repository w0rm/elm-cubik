module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Decode
import Html exposing (Html, div)
import InteractionHandler
    exposing
        ( InteractionHandler
        , animate
        , check
        , handle
        , mouseDown
        , mouseMove
        , mouseUp
        )
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Logic
import MogeeFont
import Task
import Types exposing (..)
import View exposing (view)
import WebGL.Texture as Texture exposing (defaultOptions)


main : Program Value Model Msg
main =
    Browser.element
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
        [ Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
        , Texture.loadWith
            { defaultOptions
                | magnify = Texture.nearest
                , flipY = False
            }
            MogeeFont.spriteSrc
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

        Resize width height ->
            ( { model | width = width, height = height }
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
    [ ( always True, onResize (\w h -> Resize (toFloat w) (toFloat h)) )
    , ( check animate, onAnimationFrameDelta Tick )
    ]
        |> List.filter (Tuple.first >> (|>) model)
        |> List.map Tuple.second
        |> Sub.batch
