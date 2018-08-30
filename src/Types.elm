module Types exposing (Attributes, Cell, Color(..), Model, Msg(..), Rotation(..), State(..), Transformation)

import Animation exposing (Animation)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import WebGL.Texture exposing (Error, Texture)


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
    , font : Maybe Texture
    , rotation : Vec4
    , width : Float
    , height : Float
    , devicePixelRatio : Float
    , cubik : List Cell
    , time : Float
    }


type State
    = Initial
    | Starting Animation
    | Ending Animation
    | WaitForUserInput
    | Rotating ( Float, Float ) -- rotating the cube
    | TransformStart Cell ( Float, Float ) -- transform started (accumulating minimum distance)
    | Transforming Cell Transformation ( Float, Float ) -- calculated which cells are rotating and axis
    | Animating Transformation (List Transformation) Animation -- animating between transformations


type Msg
    = Tick Float
    | Resize Float Float
    | Move ( Float, Float )
    | Up ( Float, Float )
    | Down ( Float, Float )
    | Transform (List Transformation)
    | FontLoaded (Result Error Texture)
