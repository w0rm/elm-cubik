module Types exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector4 as Vec4 exposing (Vec4)
import Window
import Mouse
import Time exposing (Time)
import Animation exposing (Animation)


type Color
    = Red
    | Green
    | White
    | Blue
    | Orange
    | Yellow


type Rotation
    = XAxis
    | YAxis
    | ZAxis


type alias Cell =
    { transform : Mat4
    , color : Color
    , normal : Vec3
    }


type alias Transformation =
    { axis : Rotation
    , angle : Float
    , coord : Int
    }


type alias Attributes =
    { position : Vec3 }


type alias Model =
    { state : State
    , rotation : Vec4
    , perspective : Mat4
    , camera : Mat4
    , window : Window.Size
    , devicePixelRatio : Float
    , cubik : List Cell
    , time : Float
    }


type State
    = Initial
    | Rotating Mouse.Position -- rotating the cube
    | TransformStart Cell Mouse.Position -- transform started (accumulating minimum distance)
    | Transforming Cell Transformation Mouse.Position -- calculated which cells are rotating and axis
    | Animating Transformation (List Transformation) Animation


type Msg
    = Tick Time
    | Resize Window.Size
    | Move Mouse.Position
    | Up Mouse.Position
    | Down Mouse.Position
    | Transform (List Transformation)
