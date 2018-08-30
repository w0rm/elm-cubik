module View exposing (cellAttributes, view)

import Animation
import Decode
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (height, style, width)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import InteractionHandler exposing (check, mouseDown, mouseMove, mouseUp)
import Logic
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Quaternion
import Text
import Types exposing (..)
import Utils exposing (..)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings
import WebGL.Settings.DepthTest


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


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


{-| Only add necessary events
-}
events : Model -> List (Attribute Msg)
events model =
    [ ( check mouseDown, Touch.onStart (touchCoordinates >> Down) )
    , ( check mouseMove, Touch.onMove (touchCoordinates >> Move) )
    , ( check mouseUp, Touch.onEnd (touchCoordinates >> Up) )
    , ( check mouseDown, Mouse.onDown (.pagePos >> Down) )
    , ( check mouseMove, Mouse.onMove (.pagePos >> Move) )
    , ( check mouseUp, Mouse.onUp (.pagePos >> Up) )
    ]
        |> List.filter (Tuple.first >> (|>) model)
        |> List.map Tuple.second


view : Model -> Html Msg
view model =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.003 0.003 0.251 1
        ]
        ([ width (round (model.width * model.devicePixelRatio))
         , height (round (model.height * model.devicePixelRatio))
         , style "position" "absolute"
         , style "left" "0"
         , style "top" "0"
         , style "width" (String.fromFloat model.width ++ "px")
         , style "height" (String.fromFloat model.height ++ "px")
         ]
            ++ events model
        )
        ((case model.font of
            Just texture ->
                [ Text.render
                    texture
                    (Logic.perspective model)
                    (Logic.camera model)
                    (Mat4.makeScale3 0.08 0.08 0.4
                        |> Mat4.mul (Mat4.makeTranslate3 0.9 -2 0)
                        |> Mat4.mul (Mat4.makeRotate (pi / 14) Vec3.j)
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
            { camera = Logic.camera model
            , perspective = Logic.perspective model
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
            { camera = Logic.camera model
            , perspective = Logic.perspective model
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
