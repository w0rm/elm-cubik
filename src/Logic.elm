port module Logic exposing
    ( camera
    , cellClickCoordinates
    , endingAnimation
    , getMousePosition
    , initialTransformationAnimationStart
    , mouseDown
    , numberOfTransformations
    , perspective
    , rotate
    , rotation
    , rotationDirection
    , rotationEnd
    , save
    , selectCell
    , startAnimation
    , startAnimationStart
    , startTransforming
    , startingAnimation
    , transform
    , transformStart
    , transformationAnimation
    , transformationAnimationStart
    , transforming
    )

import Animation
import Decode exposing (origin)
import Encode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Quaternion
import Random
import Types exposing (..)
import Utils exposing (..)


port save : String -> Cmd msg


perspective : Model -> Mat4
perspective { width, height } =
    Mat4.makePerspective 45 (width / height) 0.01 100


camera : Model -> Mat4
camera model =
    case model.state of
        Initial ->
            Mat4.makeLookAt Decode.startOrigin Decode.startDestination Vec3.j

        Starting animation ->
            Mat4.makeLookAt
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.startOrigin Decode.origin)
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.startDestination Decode.destination)
                Vec3.j

        Ending animation ->
            Mat4.makeLookAt
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.origin Decode.startOrigin)
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.destination Decode.startDestination)
                Vec3.j

        _ ->
            Mat4.makeLookAt Decode.origin Decode.destination Vec3.j


numberOfTransformations : Int
numberOfTransformations =
    32


mouseDown : ( Float, Float ) -> Model -> ( Model, Cmd Msg )
mouseDown mouse model =
    case selectCell mouse model of
        Just cell ->
            ( { model | state = TransformStart cell mouse }, Cmd.none )

        Nothing ->
            ( { model | state = Rotating mouse }, Cmd.none )


startAnimation : Float -> Model -> ( Model, Cmd Msg )
startAnimation diff model =
    ( { model
        | rotation =
            Quaternion.mul
                (Quaternion.fromAngleAxis (diff * 0.003) Vec3.j)
                model.rotation
      }
    , Cmd.none
    )


startingAnimation : Animation.Animation -> Float -> Model -> ( Model, Cmd Msg )
startingAnimation animation diff model =
    if Animation.isDone model.time animation then
        ( { model | rotation = Decode.defaultRotation }
        , Random.generate Transform (randomTransformations numberOfTransformations)
        )

    else
        ( model, Cmd.none )


endingAnimation : Animation.Animation -> Float -> Model -> ( Model, Cmd Msg )
endingAnimation animation diff model =
    if Animation.isDone model.time animation then
        ( { model | rotation = Decode.defaultRotation, state = Initial }, Cmd.none )

    else
        ( model, Cmd.none )


transformationAnimation : Transformation -> List Transformation -> Animation.Animation -> Float -> Model -> ( Model, Cmd Msg )
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


rotation : ( Float, Float ) -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
rotation source mouse model =
    ( rotate source mouse model, Cmd.none )


transformStart : Cell -> ( Float, Float ) -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
transformStart cell ( px, py ) ( mx, my ) model =
    if (px - mx) ^ 2 + (py - my) ^ 2 > 100 then
        ( startTransforming cell ( px, py ) ( mx, my ) model, Cmd.none )

    else
        ( model, Cmd.none )


transforming : Cell -> Transformation -> ( Float, Float ) -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
transforming cell transformation source dest model =
    let
        axis =
            transformation.axis

        ( x, y, z ) =
            rotationDirection cell model source dest

        ( nx, ny, nz ) =
            round3 cell.normal

        angle =
            if abs nz == 1 then
                -- front, back
                if axis == XAxis then
                    toFloat nz * y

                else
                    -(toFloat nz) * x

            else if abs nx == 1 then
                -- right, left
                if axis == ZAxis then
                    -(toFloat nx) * y

                else
                    toFloat nx * z

            else if abs ny == 1 then
                -- bottom, top
                if axis == XAxis then
                    -(toFloat ny) * z

                else
                    toFloat ny * x

            else
                -- sorry
                0
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


rotate : ( Float, Float ) -> ( Float, Float ) -> Model -> Model
rotate ( sx, sy ) ( dx, dy ) model =
    { model
        | state = Rotating ( dx, dy )
        , rotation =
            model.rotation
                |> Quaternion.mul (Quaternion.fromAngleAxis ((dx - sx) * 0.005) Vec3.j)
                |> Quaternion.mul (Quaternion.fromAngleAxis ((sy - dy) * 0.005) Vec3.i)
    }


startTransforming : Cell -> ( Float, Float ) -> ( Float, Float ) -> Model -> Model
startTransforming cell source dest model =
    let
        ( x, y, z ) =
            rotationDirection cell model source dest

        ( nx, ny, nz ) =
            round3 cell.normal

        ( axis, angle ) =
            if abs nz == 1 then
                -- front, back
                if abs x < abs y then
                    ( XAxis, toFloat nz * y )

                else
                    ( YAxis, -(toFloat nz) * x )

            else if abs nx == 1 then
                -- right, left
                if abs z < abs y then
                    ( ZAxis, -(toFloat nx) * y )

                else
                    ( YAxis, toFloat nx * z )

            else if abs ny == 1 then
                -- top, bottom
                if abs x < abs z then
                    ( XAxis, -(toFloat ny) * z )

                else
                    ( ZAxis, toFloat ny * x )

            else
                -- Sorry
                ( ZAxis, 0 )

        coord =
            cellRotationCoord axis cell
    in
    { model | state = Transforming cell { coord = coord, axis = axis, angle = angle } source }


selectCell : ( Float, Float ) -> Model -> Maybe Cell
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
cellClickCoordinates destination transform_ =
    [ ( Mat4.transform transform_ (vec3 -0.5 0.5 -0.5)
      , Mat4.transform transform_ (vec3 -0.5 -0.5 -0.5)
      , Mat4.transform transform_ (vec3 0.5 0.5 -0.5)
      )
    , ( Mat4.transform transform_ (vec3 0.5 0.5 -0.5)
      , Mat4.transform transform_ (vec3 -0.5 -0.5 -0.5)
      , Mat4.transform transform_ (vec3 0.5 -0.5 -0.5)
      )
    ]
        |> List.filterMap (rayTriangleIntersect origin destination)
        |> List.head


rotationDirection : Cell -> Model -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float, Float )
rotationDirection cell model source dest =
    let
        rotation_ =
            Quaternion.toMat4 model.rotation

        inverseRot =
            Mat4.inverseOrthonormal rotation_

        fromCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model source)
                (Mat4.transform rotation_ (cellPosition cell))
                (Mat4.transform rotation_ cell.normal)

        toCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model dest)
                (Mat4.transform rotation_ (cellPosition cell))
                (Mat4.transform rotation_ cell.normal)
    in
    Maybe.map2 Vec3.sub fromCoord toCoord
        |> Maybe.map (Mat4.transform inverseRot >> Vec3.toRecord >> (\{ x, y, z } -> ( x, y, z )))
        |> Maybe.withDefault ( 0, 0, 0 )


getMousePosition : Model -> ( Float, Float ) -> Vec3
getMousePosition model ( mx, my ) =
    let
        homogeneousClipCoordinates =
            Vec4.vec4
                (mx * 2 / model.width - 1)
                (1 - my * 2 / model.height)
                -1
                1

        invertedProjectionMatrix =
            Maybe.withDefault Mat4.identity (Mat4.inverse (perspective model))

        vec4CameraCoordinates =
            transform4 invertedProjectionMatrix homogeneousClipCoordinates

        direction =
            Vec4.vec4
                (Vec4.getX vec4CameraCoordinates)
                (Vec4.getY vec4CameraCoordinates)
                -1
                0

        vec4WorldCoordinates =
            transform4 (Mat4.inverseOrthonormal (camera model)) direction

        vec3WorldCoordinates =
            vec3
                (Vec4.getX vec4WorldCoordinates)
                (Vec4.getY vec4WorldCoordinates)
                (Vec4.getZ vec4WorldCoordinates)
    in
    Vec3.normalize vec3WorldCoordinates
