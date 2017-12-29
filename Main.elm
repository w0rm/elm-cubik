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


model : Model
model =
    { state = Initial
    , rotation = Mat4.identity
    , perspective = Mat4.identity
    , camera = Mat4.makeLookAt origin (vec3 0 0 0) Vec3.j
    , window = Window.Size 0 0
    , cubik = cubik
    }


origin : Vec3
origin =
    vec3 0 0 -10


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Task.perform Resize Window.size )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize window ->
            ( { model
                | window = window
                , perspective = Mat4.makePerspective 45 (toFloat window.width / toFloat window.height) 0.01 100
              }
            , Cmd.none
            )

        Down mouse ->
            case model.state of
                Initial ->
                    case selectCell mouse model of
                        Just ( id, cell ) ->
                            ( { model | state = Clicked id mouse }, Cmd.none )

                        Nothing ->
                            ( { model | state = Rotating mouse }, Cmd.none )

                Selected cellId ->
                    case selectCell mouse model of
                        Just ( id, cell ) ->
                            if id == cellId then
                                ( { model | state = TransformStart id mouse }, Cmd.none )
                            else
                                ( { model | state = Clicked id mouse }, Cmd.none )

                        Nothing ->
                            ( { model | state = Rotating mouse }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Up mouse ->
            case model.state of
                Clicked cellId mouse ->
                    case selectCell mouse model of
                        Just ( id, cell ) ->
                            if id == cellId then
                                ( { model | state = Selected id }, Cmd.none )
                            else
                                ( { model | state = Initial }, Cmd.none )

                        Nothing ->
                            ( { model | state = Initial }, Cmd.none )

                Transforming _ cells axis angle _ ->
                    let
                        closestAngle =
                            toFloat (round (angle / (pi / 2))) * pi / 2
                    in
                        ( { model
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
                        , Cmd.none
                        )

                _ ->
                    ( { model | state = Initial }, Cmd.none )

        Move mouse ->
            case model.state of
                Clicked _ position ->
                    ( { model
                        | state = Rotating mouse
                        , rotation =
                            model.rotation
                                |> Mat4.mul (Mat4.makeRotate (toFloat (mouse.x - position.x) * 0.005) Vec3.j)
                                |> Mat4.mul (Mat4.makeRotate (toFloat (position.y - mouse.y) * 0.005) Vec3.i)
                      }
                    , Cmd.none
                    )

                Rotating position ->
                    ( { model
                        | state = Rotating mouse
                        , rotation =
                            model.rotation
                                |> Mat4.mul (Mat4.makeRotate (toFloat (mouse.x - position.x) * 0.005) Vec3.j)
                                |> Mat4.mul (Mat4.makeRotate (toFloat (position.y - mouse.y) * 0.005) Vec3.i)
                      }
                    , Cmd.none
                    )

                TransformStart cellId position ->
                    if sqr (position.x - mouse.x) + sqr (position.y - mouse.y) > 100 then
                        ( startTransforming cellId position mouse model
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                Transforming cellId cells axis angle from ->
                    ( transforming cellId cells mouse axis angle from model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


startTransforming : Int -> Mouse.Position -> Mouse.Position -> Model -> Model
startTransforming cellId mouse dest model =
    let
        cell =
            case Dict.get cellId model.cubik of
                Just c ->
                    c

                Nothing ->
                    Debug.crash "shouldn't happen"

        inverseRot =
            Mat4.inverseOrthonormal model.rotation

        fromCoord =
            cell.transform
                |> Mat4.mul model.rotation
                |> cellClickCoordinates (getMousePosition model mouse)
                |> Maybe.withDefault (vec3 0 0 0)

        toCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model dest)
                (Mat4.transform model.rotation (cellPosition cell))
                (Mat4.transform model.rotation cell.normal)

        ( x, y, z ) =
            Maybe.map (Vec3.sub fromCoord) toCoord
                |> Maybe.map (Mat4.transform inverseRot)
                |> Maybe.withDefault (vec3 0 0 0)
                |> Vec3.toTuple

        ( axis, angle ) =
            case round3 cell.normal of
                ( 0, 0, -1 ) ->
                    --"red, front"
                    if abs x < abs y then
                        ( XAxis, -y )
                    else
                        ( YAxis, x )

                ( 0, 0, 1 ) ->
                    --"white, back"
                    if abs x < abs y then
                        ( XAxis, y )
                    else
                        ( YAxis, -x )

                ( -1, 0, 0 ) ->
                    --"yellow, right"
                    if abs z < abs y then
                        ( ZAxis, y )
                    else
                        ( YAxis, -z )

                ( 1, 0, 0 ) ->
                    --"orange, left"
                    if abs z < abs y then
                        ( ZAxis, -y )
                    else
                        ( YAxis, z )

                ( 0, 1, 0 ) ->
                    --"green, top"
                    if abs x < abs z then
                        ( XAxis, -z )
                    else
                        ( ZAxis, x )

                ( 0, -1, 0 ) ->
                    --"blue, bottom"
                    if abs x < abs z then
                        ( XAxis, z )
                    else
                        ( ZAxis, -x )

                _ ->
                    Debug.crash "Shouldn't happen"

        cells =
            selectCells cell axis model.cubik
    in
        { model
            | state = Transforming cellId cells axis angle fromCoord -- TODO: correct
        }


selectCells : Cell -> Rotation -> Dict Int Cell -> Set Int
selectCells selectedCell axis cells =
    let
        ( x, y, z ) =
            round3 (cellPosition selectedCell)
    in
        case axis of
            XAxis ->
                Dict.foldl
                    (\cellId cell ->
                        if round (Vec3.getX (cellPosition cell)) == x then
                            Set.insert cellId
                        else
                            identity
                    )
                    Set.empty
                    cells

            YAxis ->
                Dict.foldl
                    (\cellId cell ->
                        if round (Vec3.getY (cellPosition cell)) == y then
                            Set.insert cellId
                        else
                            identity
                    )
                    Set.empty
                    cells

            ZAxis ->
                Dict.foldl
                    (\cellId cell ->
                        if round (Vec3.getZ (cellPosition cell)) == z then
                            Set.insert cellId
                        else
                            identity
                    )
                    Set.empty
                    cells


transforming : Int -> Set Int -> Mouse.Position -> Rotation -> Float -> Vec3 -> Model -> Model
transforming cellId cells dest axis angle fromCoord model =
    let
        cell =
            case Dict.get cellId model.cubik of
                Just c ->
                    c

                Nothing ->
                    Debug.crash "shouldn't happen"

        inverseRot =
            Mat4.inverseOrthonormal model.rotation

        toCoord =
            rayPlaneIntersect
                origin
                (getMousePosition model dest)
                (Mat4.transform model.rotation (cellPosition cell))
                (Mat4.transform model.rotation cell.normal)

        ( x, y, z ) =
            Maybe.map (Vec3.sub fromCoord) toCoord
                |> Maybe.map (Mat4.transform inverseRot)
                |> Maybe.withDefault (vec3 0 0 0)
                |> Vec3.toTuple

        angle =
            case round3 cell.normal of
                ( 0, 0, -1 ) ->
                    -- red, front
                    if axis == XAxis then
                        -y
                    else
                        x

                ( 0, 0, 1 ) ->
                    -- white, back
                    if axis == XAxis then
                        y
                    else
                        -x

                ( -1, 0, 0 ) ->
                    -- yellow, right
                    if axis == ZAxis then
                        y
                    else
                        -z

                ( 1, 0, 0 ) ->
                    -- orange, left
                    if axis == ZAxis then
                        -y
                    else
                        z

                ( 0, 1, 0 ) ->
                    -- green, top
                    if axis == XAxis then
                        -z
                    else
                        x

                ( 0, -1, 0 ) ->
                    -- blue, bottom
                    if axis == XAxis then
                        z
                    else
                        -x

                _ ->
                    0
    in
        { model
            | state = Transforming cellId cells axis angle fromCoord -- TODO: correct
        }


sqr : number -> number
sqr a =
    a * a


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
        Red ->
            frontFace color

        White ->
            List.map (rotateCell XAxis pi) (frontFace color)

        Green ->
            List.map (rotateCell XAxis (pi / 2)) (frontFace color)

        Blue ->
            List.map (rotateCell XAxis (-pi / 2)) (frontFace color)

        Orange ->
            List.map (rotateCell YAxis (-pi / 2)) (frontFace color)

        Yellow ->
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

        invertedViewMatrix =
            Mat4.inverseOrthonormal camera

        vec4WorldCoordinates =
            transform4 invertedViewMatrix direction

        vec3WorldCoordinates =
            vec3
                (Vec4.getX vec4WorldCoordinates)
                (Vec4.getY vec4WorldCoordinates)
                (Vec4.getZ vec4WorldCoordinates)
    in
        Vec3.normalize vec3WorldCoordinates
