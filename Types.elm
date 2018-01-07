module Types exposing (..)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Window
import Mouse
import Dict exposing (Dict)


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
    , angle : Float -- todo make int of pi/2 ?
    , coord : Int
    }


type alias Attributes =
    { position : Vec3 }


type alias Model =
    { state : State
    , rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , window : Window.Size
    , devicePixelRatio : Float
    , cubik : Dict Int Cell
    }


type State
    = Initial
    | Rotating Mouse.Position -- rotating the cube
    | TransformStart Int Mouse.Position -- transform started (accumulating minimum distance)
    | Transforming Int Transformation Mouse.Position -- calculated which cells are rotating and axis


type Msg
    = Resize Window.Size
    | Move Mouse.Position
    | Up Mouse.Position
    | Down Mouse.Position
    | Transform (List Transformation)
