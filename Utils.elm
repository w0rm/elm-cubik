module Utils exposing (..)

import Types exposing (..)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)


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
        ( x, y, z ) =
            Vec3.toTuple v
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
                            Just (Vec3.add rayOrigin (Vec3.scale ((Vec3.dot edge2 qvec) / det) (rayDirection)))
