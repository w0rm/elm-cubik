module Quaternion exposing (fromAngleAxis, identity, mul, slerp, toMat4)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)


identity : Vec4
identity =
    vec4 0 0 0 1


fromAngleAxis : Float -> Vec3 -> Vec4
fromAngleAxis angle axis =
    let
        { x, y, z } =
            Vec3.toRecord (Vec3.normalize axis)

        theta =
            angle / 2.0

        c =
            cos theta

        s =
            sin theta
    in
    vec4 (x * s) (y * s) (z * s) c


toMat4 : Vec4 -> Mat4
toMat4 q =
    let
        { x, y, z, w } =
            Vec4.toRecord q
    in
    Mat4.fromRecord
        { m11 = 1 - 2 * y * y - 2 * z * z
        , m12 = 2 * x * y - 2 * w * z
        , m13 = 2 * x * z + 2 * w * y
        , m14 = 0
        , m21 = 2 * x * y + 2 * w * z
        , m22 = 1 - 2 * x * x - 2 * z * z
        , m23 = 2 * y * z - 2 * w * x
        , m24 = 0
        , m31 = 2 * x * z - 2 * w * y
        , m32 = 2 * y * z + 2 * w * x
        , m33 = 1 - 2 * x * x - 2 * y * y
        , m34 = 0
        , m41 = 0
        , m42 = 0
        , m43 = 0
        , m44 = 1
        }


mul : Vec4 -> Vec4 -> Vec4
mul qtr1 qtr2 =
    let
        q1 =
            Vec4.toRecord qtr1

        q2 =
            Vec4.toRecord qtr2
    in
    vec4
        (q1.x * q2.w + q1.y * q2.z - q1.z * q2.y + q1.w * q2.x)
        (-q1.x * q2.z + q1.y * q2.w + q1.z * q2.x + q1.w * q2.y)
        (q1.x * q2.y - q1.y * q2.x + q1.z * q2.w + q1.w * q2.z)
        (-q1.x * q2.x - q1.y * q2.y - q1.z * q2.z + q1.w * q2.w)


{-| Spherical linear interpolation
<http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/index.htm>

Elm implementation copied from <https://github.com/kfish/quaternion>

-}
slerp : Float -> Vec4 -> Vec4 -> Vec4
slerp t qa qb =
    let
        -- Calculate angle between them.
        cosHalfTheta =
            Vec4.dot qa qb

        halfTheta =
            acos cosHalfTheta

        sinHalfTheta =
            sqrt (1.0 - cosHalfTheta * cosHalfTheta)

        a =
            Vec4.toRecord qa

        b =
            Vec4.toRecord qb

        hw =
            a.w * 0.5 + b.w * 0.5

        hx =
            a.x * 0.5 + b.x * 0.5

        hy =
            a.y * 0.5 + b.y * 0.5

        hz =
            a.z * 0.5 + b.z * 0.5

        ratioA =
            sin ((1 - t) * halfTheta) / sinHalfTheta

        ratioB =
            sin (t * halfTheta) / sinHalfTheta

        mw =
            a.w * ratioA + b.w * ratioB

        mx =
            a.x * ratioA + b.x * ratioB

        my =
            a.y * ratioA + b.y * ratioB

        mz =
            a.z * ratioA + b.z * ratioB
    in
    -- if qa=qb or qa=-qb then theta = 0 and we can return qa
    if abs cosHalfTheta >= 1.0 then
        qa
        -- if theta = 180 degrees then result is not fully defined
        -- we could rotate around any axis normal to qa or qb

    else if abs sinHalfTheta < 0.001 then
        vec4 hw hx hy hz

    else
        vec4 mw mx my mz
