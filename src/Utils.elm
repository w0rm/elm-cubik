module Utils exposing (cellPosition, cellRotationCoord, checkSolved, checkSolvedHelp, colorToInt, colorToVec3, interpolateVec3, makeRotation, randomAngle, randomRotation, randomTransformation, randomTransformations, randomTransformationsHelp, rayPlaneIntersect, rayTriangleIntersect, rotateCell, round3, transform4)

import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Random exposing (Generator)
import Types exposing (..)


checkSolved : List Cell -> Bool
checkSolved =
    checkSolvedHelp Dict.empty


checkSolvedHelp : Dict Int ( Int, Int, Int ) -> List Cell -> Bool
checkSolvedHelp memo list =
    case list of
        [] ->
            True

        { color, normal } :: rest ->
            let
                intColor =
                    colorToInt color

                roundNormal =
                    round3 normal
            in
            case Dict.get intColor memo of
                Just checkedNormal ->
                    if roundNormal == checkedNormal then
                        checkSolvedHelp memo rest

                    else
                        False

                Nothing ->
                    checkSolvedHelp
                        (Dict.insert (colorToInt color) roundNormal memo)
                        rest


randomTransformations : Int -> Generator (List Transformation)
randomTransformations i =
    randomTransformationsHelp
        (i - 1)
        (Random.map (\t -> [ t ]) randomTransformation)


randomTransformationsHelp : Int -> Generator (List Transformation) -> Generator (List Transformation)
randomTransformationsHelp i =
    if i == 0 then
        identity

    else
        Random.andThen
            (\transforms -> Random.map (\t -> t :: transforms) randomTransformation)
            >> randomTransformationsHelp (i - 1)


randomTransformation : Generator Transformation
randomTransformation =
    Random.map3 Transformation
        randomRotation
        randomAngle
        (Random.int -1 1)


randomRotation : Generator Rotation
randomRotation =
    Random.int 0 2
        |> Random.map
            (\i ->
                case i of
                    0 ->
                        XAxis

                    1 ->
                        YAxis

                    _ ->
                        ZAxis
            )


randomAngle : Generator Float
randomAngle =
    Random.map2
        (\n negative ->
            if negative == 1 then
                toFloat n * pi / 2

            else
                toFloat n * pi / -2
        )
        (Random.int 1 2)
        (Random.int 0 1)


cellRotationCoord : Rotation -> Cell -> Int
cellRotationCoord axis cell =
    case axis of
        XAxis ->
            round (Vec3.getX (cellPosition cell))

        YAxis ->
            round (Vec3.getY (cellPosition cell))

        ZAxis ->
            round (Vec3.getZ (cellPosition cell))


rotateCell : Rotation -> Float -> Cell -> Cell
rotateCell rotation angle cell =
    let
        rotationMat4 =
            makeRotation rotation angle
    in
    { cell
        | transform = Mat4.mul rotationMat4 cell.transform
        , normal = Mat4.transform rotationMat4 cell.normal
    }


makeRotation : Rotation -> Float -> Mat4
makeRotation rotation angle =
    case rotation of
        XAxis ->
            Mat4.makeRotate angle Vec3.i

        YAxis ->
            Mat4.makeRotate angle Vec3.j

        ZAxis ->
            Mat4.makeRotate angle Vec3.k


cellPosition : Cell -> Vec3
cellPosition cell =
    vec3 0 0 -0.45
        |> Mat4.transform cell.transform


transform4 : Mat4 -> Vec4 -> Vec4
transform4 mat v =
    let
        r =
            Mat4.toRecord mat
    in
    vec4
        (Vec4.dot (vec4 r.m11 r.m12 r.m13 r.m14) v)
        (Vec4.dot (vec4 r.m21 r.m22 r.m23 r.m24) v)
        (Vec4.dot (vec4 r.m31 r.m32 r.m33 r.m34) v)
        (Vec4.dot (vec4 r.m41 r.m42 r.m43 r.m44) v)


round3 : Vec3 -> ( Int, Int, Int )
round3 v =
    let
        { x, y, z } =
            Vec3.toRecord v
    in
    ( round x, round y, round z )


rayPlaneIntersect : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Maybe Vec3
rayPlaneIntersect rayOrigin rayDirection point normal =
    let
        epsilon =
            0.000001

        vectorDotNormal =
            Vec3.dot rayDirection normal
    in
    if vectorDotNormal > -epsilon then
        Nothing

    else
        let
            t =
                Vec3.dot (Vec3.sub point rayOrigin) normal / vectorDotNormal
        in
        Just (Vec3.add rayOrigin (Vec3.scale t rayDirection))


rayTriangleIntersect : Vec3 -> Vec3 -> ( Vec3, Vec3, Vec3 ) -> Maybe Vec3
rayTriangleIntersect rayOrigin rayDirection ( triangle0, triangle2, triangle1 ) =
    let
        epsilon =
            0.000001

        edge1 =
            Vec3.sub triangle1 triangle0

        edge2 =
            Vec3.sub triangle2 triangle0

        pvec =
            Vec3.cross rayDirection edge2

        det =
            Vec3.dot edge1 pvec
    in
    if det < epsilon then
        Nothing

    else
        let
            tvec =
                Vec3.sub rayOrigin triangle0

            u =
                Vec3.dot tvec pvec
        in
        if u < 0 || u > det then
            Nothing

        else
            let
                qvec =
                    Vec3.cross tvec edge1

                v =
                    Vec3.dot rayDirection qvec
            in
            if v < 0 || u + v > det then
                Nothing

            else
                Just (Vec3.add rayOrigin (Vec3.scale (Vec3.dot edge2 qvec / det) rayDirection))


colorToInt : Color -> Int
colorToInt color =
    case color of
        Red ->
            0

        Green ->
            1

        White ->
            2

        Blue ->
            3

        Orange ->
            4

        Yellow ->
            5


colorToVec3 : Color -> Vec3
colorToVec3 color =
    case color of
        Red ->
            vec3 0.757 0.117 0.227

        Green ->
            vec3 0.003 0.619 0.372

        White ->
            vec3 1 1 1

        Blue ->
            vec3 0 0.317 0.729

        Orange ->
            vec3 0.937 0.321 0.015

        Yellow ->
            vec3 1 0.835 0


interpolateVec3 : Float -> Vec3 -> Vec3 -> Vec3
interpolateVec3 progress v1 v2 =
    Vec3.add v1 (Vec3.scale progress (Vec3.sub v2 v1))
