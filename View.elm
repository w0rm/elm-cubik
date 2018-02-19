module View exposing (view, perspective, camera, cellAttributes)

import Html.Attributes exposing (width, height, style)
import Html exposing (Html, div)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Settings
import WebGL.Settings.DepthTest
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Types exposing (..)
import Utils exposing (..)
import Touch
import SingleTouch
import Mouse
import Animation
import Quaternion
import Text
import Decode


perspective : Model -> Mat4
perspective { window } =
    Mat4.makePerspective
        45
        (toFloat window.width / toFloat window.height)
        0.01
        100


camera : Model -> Mat4
camera model =
    case model.state of
        Initial ->
            Mat4.makeLookAt Decode.startOrigin Decode.startDestination Vec3.j

        Starting animation ->
            Mat4.makeLookAt
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.startOrigin Decode.origin)
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.startDestination Decode.destination)
                Vec3.j

        Ending animation ->
            Mat4.makeLookAt
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.origin Decode.startOrigin)
                (Utils.interpolateVec3 (Animation.animate model.time animation) Decode.destination Decode.startDestination)
                Vec3.j

        _ ->
            Mat4.makeLookAt Decode.origin Decode.destination Vec3.j


rotation : Model -> Mat4
rotation model =
    case model.state of
        Initial ->
            Quaternion.toMat4 model.rotation

        Starting animation ->
            Quaternion.slerp (Animation.animate model.time animation) model.rotation Decode.defaultRotation
                |> Quaternion.toMat4

        Ending animation ->
            Quaternion.slerp (Animation.animate model.time animation) model.rotation Decode.defaultRotation
                |> Quaternion.toMat4

        _ ->
            Quaternion.toMat4 model.rotation


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
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.003 0.003 0.251 1
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
        ((case model.font of
            Just texture ->
                [ Text.render
                    texture
                    (perspective model)
                    (camera model)
                    (Mat4.makeScale3 0.08 0.08 0.08
                        |> Mat4.mul (Mat4.makeTranslate3 1 0 0)
                        |> Mat4.mul (Mat4.makeRotate (pi / 15) Vec3.j)
                        |> Mat4.mul (Mat4.makeTranslate3 0 -0.6 -11)
                    )
                    Text.clickToStart
                ]

            Nothing ->
                []
         )
            ++ List.foldl (cellEntity model) [] model.cubik
        )


cellEntity : Model -> Cell -> List Entity -> List Entity
cellEntity model cell =
    let
        ( isHighlighted, rotationFunc ) =
            case model.state of
                TransformStart activeCell _ ->
                    ( cell == activeCell, identity )

                Transforming activeCell { coord, axis, angle } _ ->
                    if cellRotationCoord axis cell == coord then
                        ( cell == activeCell, Mat4.mul (makeRotation axis angle) )
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
                { camera = camera model
                , perspective = perspective model
                , rotation = rotation model
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
                { camera = camera model
                , perspective = perspective model
                , rotation = rotation model
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
