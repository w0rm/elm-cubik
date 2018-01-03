module Encode exposing (model)

import Json.Encode as Encode exposing (Value)
import Types exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Dict


model : Model -> Value
model { cubik, rotation } =
    Encode.object
        [ ( "cubik", Encode.list (List.map cell (Dict.values cubik)) )
        , ( "rotation", mat4 rotation )
        ]


cell : Cell -> Value
cell c =
    Encode.object
        [ ( "transform", mat4 c.transform )
        , ( "color", color c.color )
        , ( "normal", vec3 c.normal )
        ]


mat4 : Mat4 -> Value
mat4 mat =
    let
        r =
            Mat4.toRecord mat
    in
        [ r.m11, r.m21, r.m31, r.m41, r.m12, r.m22, r.m32, r.m42, r.m13, r.m23, r.m33, r.m43, r.m14, r.m24, r.m34, r.m44 ]
            |> List.map Encode.float
            |> Encode.list


vec3 : Vec3 -> Value
vec3 vec =
    let
        r =
            Vec3.toRecord vec
    in
        [ r.x, r.y, r.z ]
            |> List.map Encode.float
            |> Encode.list


color : Color -> Value
color col =
    case col of
        Red ->
            Encode.string "red"

        White ->
            Encode.string "white"

        Blue ->
            Encode.string "blue"

        Orange ->
            Encode.string "orange"

        Yellow ->
            Encode.string "yellow"

        Green ->
            Encode.string "green"
