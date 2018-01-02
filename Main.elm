module Main exposing (main)

import Html exposing (Html, div)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Window
import Mouse
import Task
import Set exposing (Set)
import Dict exposing (Dict)
import Types exposing (..)
import View exposing (view, cellAttributes)
import Utils exposing (..)
import Json.Decode as Decode exposing (Value)


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
                    ]
        }


init : Value -> ( Model, Cmd Msg )
init value =
    let
        width =
            Decode.decodeValue (Decode.field "width" Decode.int) value
                |> Result.withDefault 0

        height =
            Decode.decodeValue (Decode.field "height" Decode.int) value
                |> Result.withDefault 0

        devicePixelRatio =
            Decode.decodeValue (Decode.field "devicePixelRatio" Decode.float) value
                |> Result.withDefault 1
    in
        ( { state = Initial
          , rotation =
                Mat4.identity
                    |> Mat4.mul (Mat4.makeRotate (pi / 4) Vec3.j)
                    |> Mat4.mul (Mat4.makeRotate (-pi / 4) Vec3.i)
          , perspective = Mat4.identity
          , camera = Mat4.makeLookAt origin (vec3 0 0 0) Vec3.j
          , window = Window.Size width height
          , devicePixelRatio = devicePixelRatio
          , cubik = cubik
          }
        , if width == 0 then
            Task.perform Resize Window.size
          else
            Cmd.none
        )


origin : Vec3
origin =
    vec3 0 0 -11


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "" msg of
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

        Down mouse ->
            case model.state of
                Initial ->
                    case selectCell mouse model of
                        Just ( id, cell ) ->
                            ( { model | state = TransformStart id mouse }, Cmd.none )

                        Nothing ->
                            ( { model | state = Rotating mouse }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Up mouse ->
            case model.state of
                Transforming _ cells axis angle _ ->
                    ( transform cells axis angle model, Cmd.none )

                _ ->
                    ( { model | state = Initial }, Cmd.none )

        Move mouse ->
            case model.state of
                Rotating source ->
                    ( rotate source mouse model, Cmd.none )

                TransformStart cellId position ->
                    if (position.x - mouse.x) ^ 2 + (position.y - mouse.y) ^ 2 > 100 then
                        ( startTransforming cellId position mouse model, Cmd.none )
                    else
                        ( model, Cmd.none )

                Transforming cellId cells axis angle source ->
                    ( transforming cellId cells source mouse axis angle model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


transform : Set Int -> Rotation -> Float -> Model -> Model
transform cells axis angle model =
    let
        closestAngle =
            toFloat (round (angle / (pi / 2))) * pi / 2
    in
        { model
            | state = Initial
            , cubik =
                Dict.map
                    (\cellId cell ->
                        if Set.member cellId cells then
                            rotateCell axis closestAngle cell
                        else
                            cell
                    )
                    model.cubik
        }


rotate : Mouse.Position -> Mouse.Position -> Model -> Model
rotate source dest model =
    { model
        | state = Rotating dest
        , rotation =
            model.rotation
                |> Mat4.mul (Mat4.makeRotate (toFloat (dest.x - source.x) * 0.005) Vec3.j)
                |> Mat4.mul (Mat4.makeRotate (toFloat (source.y - dest.y) * 0.005) Vec3.i)
    }


startTransforming : Int -> Mouse.Position -> Mouse.Position -> Model -> Model
startTransforming cellId source dest model =
    let
        cell =
            case Dict.get cellId model.cubik of
                Just c ->
                    c

                Nothing ->
                    Debug.crash "Shouldn't happen"

        ( x, y, z ) =
            rotationDirection cell model source dest

        ( axis, angle ) =
            case round3 cell.normal of
                ( 0, 0, -1 ) ->
                    -- front
                    if abs x < abs y then
                        ( XAxis, -y )
                    else
                        ( YAxis, x )

                ( 0, 0, 1 ) ->
                    -- back
                    if abs x < abs y then
                        ( XAxis, y )
                    else
                        ( YAxis, -x )

                ( -1, 0, 0 ) ->
                    -- right
                    if abs z < abs y then
                        ( ZAxis, y )
                    else
                        ( YAxis, -z )

                ( 1, 0, 0 ) ->
                    -- left
                    if abs z < abs y then
                        ( ZAxis, -y )
                    else
                        ( YAxis, z )

                ( 0, 1, 0 ) ->
                    -- top
                    if abs x < abs z then
                        ( XAxis, -z )
                    else
                        ( ZAxis, x )

                ( 0, -1, 0 ) ->
                    -- bottom
                    if abs x < abs z then
                        ( XAxis, z )
                    else
                        ( ZAxis, -x )

                _ ->
                    Debug.crash "Shouldn't happen"

        cells =
            selectCells cell axis model.cubik
    in
        { model | state = Transforming cellId cells axis angle source }


transforming : Int -> Set Int -> Mouse.Position -> Mouse.Position -> Rotation -> Float -> Model -> Model
transforming cellId cells source dest axis angle model =
    let
        cell =
            case Dict.get cellId model.cubik of
                Just c ->
                    c

                Nothing ->
                    Debug.crash "Shouldn't happen"

        ( x, y, z ) =
            rotationDirection cell model source dest

        angle =
            case round3 cell.normal of
                ( 0, 0, -1 ) ->
                    -- front
                    if axis == XAxis then
                        -y
                    else
                        x

                ( 0, 0, 1 ) ->
                    -- back
                    if axis == XAxis then
                        y
                    else
                        -x

                ( -1, 0, 0 ) ->
                    -- right
                    if axis == ZAxis then
                        y
                    else
                        -z

                ( 1, 0, 0 ) ->
                    -- left
                    if axis == ZAxis then
                        -y
                    else
                        z

                ( 0, 1, 0 ) ->
                    -- top
                    if axis == XAxis then
                        -z
                    else
                        x

                ( 0, -1, 0 ) ->
                    -- bottom
                    if axis == XAxis then
                        z
                    else
                        -x

                _ ->
                    Debug.crash "Shouldn't happen"
    in
        { model | state = Transforming cellId cells axis angle source }


selectCells : Cell -> Rotation -> Dict Int Cell -> Set Int
selectCells selectedCell axis cells =
    let
        selectedPosition =
            cellPosition selectedCell

        fn =
            case axis of
                XAxis ->
                    Vec3.getX

                YAxis ->
                    Vec3.getY

                ZAxis ->
                    Vec3.getZ
    in
        Dict.foldl
            (\cellId cell ->
                if round (fn (cellPosition cell)) == round (fn selectedPosition) then
                    Set.insert cellId
                else
                    identity
            )
            Set.empty
            cells


selectCell : Mouse.Position -> Model -> Maybe ( Int, Cell )
selectCell mouse model =
    model.cubik
        |> Dict.filter
            (\_ ->
                .transform
                    >> Mat4.mul model.rotation
                    >> cellClickCoordinates (getMousePosition model mouse)
                    >> (/=) Nothing
            )
        |> Dict.toList
        |> List.head


cubik : Dict Int Cell
cubik =
    List.concatMap makeSide [ Red, Green, White, Blue, Orange, Yellow ]
        |> List.indexedMap (,)
        |> Dict.fromList


makeSide : Color -> List Cell
makeSide color =
    case color of
        Green ->
            frontFace color

        Blue ->
            List.map (rotateCell XAxis pi) (frontFace color)

        White ->
            List.map (rotateCell XAxis (pi / 2)) (frontFace color)

        Yellow ->
            List.map (rotateCell XAxis (-pi / 2)) (frontFace color)

        Orange ->
            List.map (rotateCell YAxis (-pi / 2)) (frontFace color)

        Red ->
            List.map (rotateCell YAxis (pi / 2)) (frontFace color)


frontFace : Color -> List Cell
frontFace color =
    List.range -1 1
        |> List.concatMap
            (\x ->
                List.map
                    (\y -> Cell (Mat4.makeTranslate3 (toFloat x) (toFloat y) -1) color (vec3 0 0 -1))
                    (List.range -1 1)
            )


cellClickCoordinates : Vec3 -> Mat4 -> Maybe Vec3
cellClickCoordinates destination transform =
    cellAttributes
        |> List.map
            (\( p1, p2, p3 ) ->
                ( Mat4.transform transform p1.position
                , Mat4.transform transform p2.position
                , Mat4.transform transform p3.position
                )
            )
        |> List.filterMap (rayTriangleIntersect origin destination)
        |> List.head


rotationDirection : Cell -> Model -> Mouse.Position -> Mouse.Position -> ( Float, Float, Float )
rotationDirection cell model source dest =
    let
        inverseRot =
            Mat4.inverseOrthonormal model.rotation

        fromCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model source)
                (Mat4.transform model.rotation (cellPosition cell))
                (Mat4.transform model.rotation cell.normal)

        toCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model dest)
                (Mat4.transform model.rotation (cellPosition cell))
                (Mat4.transform model.rotation cell.normal)
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
