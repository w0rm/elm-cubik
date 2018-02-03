module View exposing (view, cellAttributes)

import Html.Attributes exposing (width, height, style)
import Html exposing (Html, div)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Settings
import WebGL.Settings.DepthTest
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Types exposing (..)
import Utils exposing (..)
import Dict
import Touch
import SingleTouch
import Mouse
import Animation
import Quaternion


type alias Uniforms =
    { color : Vec3
    , camera : Mat4
    , perspective : Mat4
    , rotation : Mat4
    , transform : Mat4
    }


cellMesh : Mesh Attributes
cellMesh =
    WebGL.triangles cellAttributes


cellAttributes : List ( Attributes, Attributes, Attributes )
cellAttributes =
    [ ( Attributes (vec3 -0.45 0.45 -0.5)
      , Attributes (vec3 -0.45 -0.45 -0.5)
      , Attributes (vec3 0.45 0.45 -0.5)
      )
    , ( Attributes (vec3 0.45 0.45 -0.5)
      , Attributes (vec3 -0.45 -0.45 -0.5)
      , Attributes (vec3 0.45 -0.45 -0.5)
      )
    ]


cubeMesh : Mesh Attributes
cubeMesh =
    let
        rft =
            vec3 0.5 0.5 0.5

        lft =
            vec3 -0.5 0.5 0.5

        lbt =
            vec3 -0.5 -0.5 0.5

        rbt =
            vec3 0.5 -0.5 0.5

        rbb =
            vec3 0.5 -0.5 -0.5

        rfb =
            vec3 0.5 0.5 -0.5

        lfb =
            vec3 -0.5 0.5 -0.5

        lbb =
            vec3 -0.5 -0.5 -0.5
    in
        [ face rft rfb rbb rbt
        , face rft rfb lfb lft
        , face rft lft lbt rbt
        , face rfb lfb lbb rbb
        , face lft lfb lbb lbt
        , face rbt rbb lbb lbt
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
face a b c d =
    [ ( Attributes a, Attributes b, Attributes c )
    , ( Attributes c, Attributes d, Attributes a )
    ]


touchToMouse : Touch.Coordinates -> Mouse.Position
touchToMouse coordinates =
    let
        ( x, y ) =
            Touch.clientPos coordinates
    in
        Mouse.Position (round x) (round y)


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.clearColor 0.003 0.003 0.251 1
        , WebGL.antialias
        , WebGL.alpha True
        ]
        [ width (round (toFloat model.window.width * model.devicePixelRatio))
        , height (round (toFloat model.window.height * model.devicePixelRatio))
        , style
            [ ( "display", "block" )
            , ( "width", toString model.window.width ++ "px" )
            , ( "height", toString model.window.height ++ "px" )
            ]
        , SingleTouch.onStart (touchToMouse >> Down)
        , SingleTouch.onMove (touchToMouse >> Move)
        , SingleTouch.onEnd (touchToMouse >> Up)
        ]
        (Dict.foldl (cellEntity model) [] model.cubik)


cellEntity : Model -> Int -> Cell -> List Entity -> List Entity
cellEntity model id cell =
    let
        perspective =
            Mat4.makePerspective 45 (toFloat model.window.width / toFloat model.window.height) 0.01 100

        ( isHighlighted, rotationFunc ) =
            case model.state of
                TransformStart cellId _ ->
                    ( cellId == id, identity )

                Transforming cellId { coord, axis, angle } _ ->
                    if cellRotationCoord axis cell == coord then
                        ( cellId == id, Mat4.mul (makeRotation axis angle) )
                    else
                        ( False, identity )

                Animating { coord, axis, angle } _ animation ->
                    if cellRotationCoord axis cell == coord then
                        ( False, Mat4.mul (makeRotation axis (Animation.animate model.time animation)) )
                    else
                        ( False, identity )

                _ ->
                    ( False, identity )

        highlightFunc =
            if isHighlighted then
                Vec3.scale 0.6
            else
                identity
    in
        (++)
            [ WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.front
                , WebGL.Settings.DepthTest.default
                ]
                vertexShader
                fragmentShader
                cellMesh
                { camera = model.camera
                , perspective = perspective
                , rotation = Quaternion.toMat4 model.rotation
                , transform = rotationFunc cell.transform
                , color = highlightFunc (colorToVec3 cell.color)
                }
            , WebGL.entityWith
                [ WebGL.Settings.DepthTest.default
                , WebGL.Settings.polygonOffset 5 5
                ]
                vertexShader
                fragmentShader
                cubeMesh
                { camera = model.camera
                , perspective = perspective
                , rotation = Quaternion.toMat4 model.rotation
                , transform = rotationFunc cell.transform
                , color = vec3 0.003 0.003 0.251
                }
            ]


vertexShader : Shader Attributes Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 rotation;
        uniform mat4 transform;

        void main () {
          gl_Position = perspective * camera * rotation * transform * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;

        void main () {
          gl_FragColor = vec4(color, 1.0);
        }
    |]
