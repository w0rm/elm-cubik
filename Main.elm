port module Cubik exposing (main)

import Html exposing (Html, div)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Window
import Mouse
import Task
import Types exposing (..)
import View exposing (view)
import Utils exposing (..)
import Decode exposing (origin)
import Encode
import Json.Encode exposing (Value)
import Json.Decode as Decode
import Random
import AnimationFrame
import Animation
import Quaternion


port save : String -> Cmd msg


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Window.resizes Resize
                    , Mouse.moves Move
                    , Mouse.downs Down
                    , Mouse.ups Up
                    , AnimationFrame.diffs Tick
                    ]
        }


init : Value -> ( Model, Cmd Msg )
init value =
    Decode.decodeValue Decode.model value
        |> Result.map (\model -> ( model, Cmd.none ))
        |> Result.withDefault
            ( Decode.initial
            , Cmd.batch
                [ Task.perform Resize Window.size
                , Random.generate Transform (randomTransformations 30)
                ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Transform transformations ->
            case transformations of
                [] ->
                    ( model, Cmd.none )

                t :: rest ->
                    ( { model
                        | state =
                            Animating t
                                rest
                                (Animation.animation model.time
                                    |> Animation.from 0
                                    |> Animation.to t.angle
                                    |> Animation.duration 100
                                )
                      }
                    , Cmd.none
                    )

        Resize window ->
            ( { model
                | window = window
                , perspective =
                    Mat4.makePerspective
                        45
                        (toFloat window.width / toFloat window.height)
                        0.01
                        100
              }
            , Cmd.none
            )

        -- Interactions
        Down mouse ->
            case model.state of
                Initial ->
                    case selectCell mouse model of
                        Just cell ->
                            ( { model | state = TransformStart cell mouse }, Cmd.none )

                        Nothing ->
                            ( { model | state = Rotating mouse }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Move mouse ->
            case model.state of
                Rotating source ->
                    ( rotate source mouse model, Cmd.none )

                TransformStart cell position ->
                    if (position.x - mouse.x) ^ 2 + (position.y - mouse.y) ^ 2 > 100 then
                        ( startTransforming cell position mouse model, Cmd.none )
                    else
                        ( model, Cmd.none )

                Transforming cell transformation source ->
                    ( transforming cell transformation source mouse model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Up mouse ->
            case model.state of
                Transforming _ transformation _ ->
                    let
                        closestAngle =
                            toFloat (round (transformation.angle / (pi / 2))) * pi / 2

                        animation =
                            Animation.animation model.time
                                |> Animation.from transformation.angle
                                |> Animation.to closestAngle
                                |> Animation.duration 100
                    in
                        ( { model
                            | state =
                                Animating
                                    { transformation | angle = closestAngle }
                                    []
                                    animation
                          }
                        , Cmd.none
                        )

                _ ->
                    ( { model | state = Initial }, save (Encode.model model) )

        Tick diff ->
            let
                time =
                    model.time + diff
            in
                case model.state of
                    Animating transformation transformations animation ->
                        if Animation.isDone time animation then
                            let
                                newModel =
                                    { model | cubik = transform transformation model.cubik }
                            in
                                case transformations of
                                    [] ->
                                        ( { newModel | state = Initial, time = time }
                                        , save (Encode.model newModel)
                                        )

                                    t :: rest ->
                                        ( { newModel
                                            | state =
                                                Animating
                                                    t
                                                    rest
                                                    (Animation.animation time
                                                        |> Animation.to t.angle
                                                        |> Animation.duration 200
                                                    )
                                            , time = time
                                          }
                                        , save (Encode.model newModel)
                                        )
                        else
                            ( { model | time = time }, Cmd.none )

                    _ ->
                        ( { model | time = time }, Cmd.none )


transform : Transformation -> List Cell -> List Cell
transform { coord, axis, angle } cubik =
    List.map
        (\cell ->
            if cellRotationCoord axis cell == coord then
                rotateCell axis angle cell
            else
                cell
        )
        cubik


rotate : Mouse.Position -> Mouse.Position -> Model -> Model
rotate source dest model =
    { model
        | state = Rotating dest
        , rotation =
            model.rotation
                |> Quaternion.mul (Quaternion.fromAngleAxis (toFloat (dest.x - source.x) * 0.005) Vec3.j)
                |> Quaternion.mul (Quaternion.fromAngleAxis (toFloat (source.y - dest.y) * 0.005) Vec3.i)
    }


startTransforming : Cell -> Mouse.Position -> Mouse.Position -> Model -> Model
startTransforming cell source dest model =
    let
        ( x, y, z ) =
            rotationDirection cell model source dest

        ( axis, angle ) =
            case round3 cell.normal of
                ( _, _, -1 ) ->
                    -- front
                    if abs x < abs y then
                        ( XAxis, -y )
                    else
                        ( YAxis, x )

                ( _, _, 1 ) ->
                    -- back
                    if abs x < abs y then
                        ( XAxis, y )
                    else
                        ( YAxis, -x )

                ( -1, _, _ ) ->
                    -- right
                    if abs z < abs y then
                        ( ZAxis, y )
                    else
                        ( YAxis, -z )

                ( 1, _, _ ) ->
                    -- left
                    if abs z < abs y then
                        ( ZAxis, -y )
                    else
                        ( YAxis, z )

                ( _, 1, _ ) ->
                    -- top
                    if abs x < abs z then
                        ( XAxis, -z )
                    else
                        ( ZAxis, x )

                ( _, -1, _ ) ->
                    -- bottom
                    if abs x < abs z then
                        ( XAxis, z )
                    else
                        ( ZAxis, -x )

                _ ->
                    Debug.crash "Shouldn't happen"

        coord =
            cellRotationCoord axis cell
    in
        { model | state = Transforming cell { coord = coord, axis = axis, angle = angle } source }


transforming : Cell -> Transformation -> Mouse.Position -> Mouse.Position -> Model -> Model
transforming cell transformation source dest model =
    let
        axis =
            transformation.axis

        ( x, y, z ) =
            rotationDirection cell model source dest

        angle =
            case round3 cell.normal of
                ( _, _, -1 ) ->
                    -- front
                    if axis == XAxis then
                        -y
                    else
                        x

                ( _, _, 1 ) ->
                    -- back
                    if axis == XAxis then
                        y
                    else
                        -x

                ( -1, _, _ ) ->
                    -- right
                    if axis == ZAxis then
                        y
                    else
                        -z

                ( 1, _, _ ) ->
                    -- left
                    if axis == ZAxis then
                        -y
                    else
                        z

                ( _, 1, _ ) ->
                    -- top
                    if axis == XAxis then
                        -z
                    else
                        x

                ( _, -1, _ ) ->
                    -- bottom
                    if axis == XAxis then
                        z
                    else
                        -x

                _ ->
                    Debug.crash "Shouldn't happen"
    in
        { model | state = Transforming cell { transformation | angle = angle } source }


selectCell : Mouse.Position -> Model -> Maybe Cell
selectCell mouse model =
    model.cubik
        |> List.filter
            (.transform
                >> Mat4.mul (Quaternion.toMat4 model.rotation)
                >> cellClickCoordinates (getMousePosition model mouse)
                >> (/=) Nothing
            )
        |> List.head


cellClickCoordinates : Vec3 -> Mat4 -> Maybe Vec3
cellClickCoordinates destination transform =
    [ ( Mat4.transform transform (vec3 -0.5 0.5 -0.5)
      , Mat4.transform transform (vec3 -0.5 -0.5 -0.5)
      , Mat4.transform transform (vec3 0.5 0.5 -0.5)
      )
    , ( Mat4.transform transform (vec3 0.5 0.5 -0.5)
      , Mat4.transform transform (vec3 -0.5 -0.5 -0.5)
      , Mat4.transform transform (vec3 0.5 -0.5 -0.5)
      )
    ]
        |> List.filterMap (rayTriangleIntersect origin destination)
        |> List.head


rotationDirection : Cell -> Model -> Mouse.Position -> Mouse.Position -> ( Float, Float, Float )
rotationDirection cell model source dest =
    let
        rotation =
            Quaternion.toMat4 model.rotation

        inverseRot =
            Mat4.inverseOrthonormal rotation

        fromCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model source)
                (Mat4.transform rotation (cellPosition cell))
                (Mat4.transform rotation cell.normal)

        toCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model dest)
                (Mat4.transform rotation (cellPosition cell))
                (Mat4.transform rotation cell.normal)
    in
        Maybe.map2 Vec3.sub fromCoord toCoord
            |> Maybe.map (Mat4.transform inverseRot >> Vec3.toTuple)
            |> Maybe.withDefault ( 0, 0, 0 )


getMousePosition : Model -> Mouse.Position -> Vec3
getMousePosition { window, perspective, camera } mouse =
    let
        homogeneousClipCoordinates =
            Vec4.vec4
                (toFloat mouse.x * 2 / toFloat window.width - 1)
                (1 - toFloat mouse.y * 2 / toFloat window.height)
                -1
                1

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse perspective)

        vec4CameraCoordinates =
            transform4 invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4
                (Vec4.getX vec4CameraCoordinates)
                (Vec4.getY vec4CameraCoordinates)
                -1
                0

        vec4WorldCoordinates =
            transform4 (Mat4.inverseOrthonormal camera) direction

        vec3WorldCoordinates =
            vec3
                (Vec4.getX vec4WorldCoordinates)
                (Vec4.getY vec4WorldCoordinates)
                (Vec4.getZ vec4WorldCoordinates)
    in
        Vec3.normalize vec3WorldCoordinates
