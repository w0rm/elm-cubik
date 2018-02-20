port module Logic exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Mouse
import Types exposing (..)
import View exposing (view)
import Utils exposing (..)
import Decode exposing (origin)
import Encode
import Random
import Animation
import Quaternion
import Time exposing (Time)


port save : String -> Cmd msg


numberOfTransformations : Int
numberOfTransformations =
    32


mouseDown : Mouse.Position -> Model -> ( Model, Cmd Msg )
mouseDown mouse model =
    case selectCell mouse model of
        Just cell ->
            ( { model | state = TransformStart cell mouse }, Cmd.none )

        Nothing ->
            ( { model | state = Rotating mouse }, Cmd.none )


startAnimation : Time.Time -> Model -> ( Model, Cmd Msg )
startAnimation diff model =
    ( { model
        | rotation =
            Quaternion.mul
                (Quaternion.fromAngleAxis (diff * 0.003) Vec3.j)
                model.rotation
      }
    , Cmd.none
    )


startingAnimation : Animation.Animation -> Time.Time -> Model -> ( Model, Cmd Msg )
startingAnimation animation diff model =
    if Animation.isDone model.time animation then
        ( { model | rotation = Decode.defaultRotation }
        , Random.generate Transform (randomTransformations numberOfTransformations)
        )
    else
        ( model, Cmd.none )


endingAnimation : Animation.Animation -> Time.Time -> Model -> ( Model, Cmd Msg )
endingAnimation animation diff model =
    if Animation.isDone model.time animation then
        ( { model | rotation = Decode.defaultRotation, state = Initial }, Cmd.none )
    else
        ( model, Cmd.none )


transformationAnimation : Transformation -> List Transformation -> Animation.Animation -> Time.Time -> Model -> ( Model, Cmd Msg )
transformationAnimation transformation transformations animation diff model =
    if Animation.isDone model.time animation then
        let
            newModel =
                { model | cubik = transform transformation model.cubik }
        in
            case transformations of
                [] ->
                    if Utils.checkSolved newModel.cubik then
                        ( { newModel | state = Ending (Animation.animation model.time |> Animation.duration 300) }
                        , save ""
                        )
                    else
                        ( { newModel | state = WaitForUserInput }
                        , save (Encode.model newModel)
                        )

                t :: rest ->
                    ( { newModel
                        | state =
                            Animating
                                t
                                rest
                                (Animation.animation model.time
                                    |> Animation.to t.angle
                                    |> Animation.duration 200
                                )
                      }
                    , Cmd.none
                    )
    else
        ( model, Cmd.none )


rotation : Mouse.Position -> Mouse.Position -> Model -> ( Model, Cmd Msg )
rotation source mouse model =
    ( rotate source mouse model, Cmd.none )


transformStart : Cell -> Mouse.Position -> Mouse.Position -> Model -> ( Model, Cmd Msg )
transformStart cell position mouse model =
    if (position.x - mouse.x) ^ 2 + (position.y - mouse.y) ^ 2 > 100 then
        ( startTransforming cell position mouse model, Cmd.none )
    else
        ( model, Cmd.none )


transforming : Cell -> Transformation -> Mouse.Position -> Mouse.Position -> Model -> ( Model, Cmd Msg )
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
        ( { model
            | state = Transforming cell { transformation | angle = angle } source
          }
        , Cmd.none
        )


startAnimationStart : Model -> ( Model, Cmd Msg )
startAnimationStart model =
    ( { model
        | state = Starting (Animation.animation model.time |> Animation.duration 300)
      }
    , Cmd.none
    )


initialTransformationAnimationStart : List Transformation -> Model -> ( Model, Cmd Msg )
initialTransformationAnimationStart transformations model =
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


transformationAnimationStart : Transformation -> Model -> ( Model, Cmd Msg )
transformationAnimationStart transformation model =
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


rotationEnd : Model -> ( Model, Cmd Msg )
rotationEnd model =
    ( { model | state = WaitForUserInput }, save (Encode.model model) )


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
getMousePosition model mouse =
    let
        homogeneousClipCoordinates =
            Vec4.vec4
                (toFloat mouse.x * 2 / toFloat model.window.width - 1)
                (1 - toFloat mouse.y * 2 / toFloat model.window.height)
                -1
                1

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse (View.perspective model))

        vec4CameraCoordinates =
            transform4 invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4
                (Vec4.getX vec4CameraCoordinates)
                (Vec4.getY vec4CameraCoordinates)
                -1
                0

        vec4WorldCoordinates =
            transform4 (Mat4.inverseOrthonormal (View.camera model)) direction

        vec3WorldCoordinates =
            vec3
                (Vec4.getX vec4WorldCoordinates)
                (Vec4.getY vec4WorldCoordinates)
                (Vec4.getZ vec4WorldCoordinates)
    in
        Vec3.normalize vec3WorldCoordinates
