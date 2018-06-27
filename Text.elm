module Text exposing (clickToStart, render)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import MogeeFont exposing (Letter)
import WebGL exposing (Entity, Mesh, Shader, Texture)
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture as Texture exposing (Error, defaultOptions)


clickToStart : Mesh Vertex
clickToStart =
    text "Click\nto Play"


render : Texture -> Mat4 -> Mat4 -> Mat4 -> Mesh Vertex -> Entity
render texture perspective camera transform textMesh =
    WebGL.entityWith
        [ WebGL.Settings.DepthTest.default
        , WebGL.Settings.cullFace WebGL.Settings.front
        ]
        texturedVertexShader
        texturedFragmentShader
        textMesh
        { texture = texture
        , perspective = perspective
        , camera = camera
        , transform = transform
        , textureSize =
            vec2
                (toFloat (Tuple.first (Texture.size texture)))
                (toFloat (Tuple.second (Texture.size texture)))
        }


text : String -> Mesh Vertex
text =
    MogeeFont.text addLetter >> WebGL.triangles


addLetter : MogeeFont.Letter -> List ( Vertex, Vertex, Vertex )
addLetter { x, y, width, height, textureX, textureY } =
    List.foldl
        (\dx l ->
            List.foldl
                (\dy -> addPixel (x + toFloat dx) (y + toFloat dy) (textureX + toFloat dx) (textureY + toFloat dy))
                l
                (List.range 0 (round height - 1))
        )
        []
        (List.range 0 (round width - 1))


addPixel : Float -> Float -> Float -> Float -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addPixel x y textureX textureY =
    let
        offset =
            vec3 -x -y 0

        texturePosition =
            vec2 textureX textureY

        rft =
            Vec3.add (vec3 0.5 0.5 0.5) offset

        lft =
            Vec3.add (vec3 -0.5 0.5 0.5) offset

        lbt =
            Vec3.add (vec3 -0.5 -0.5 0.5) offset

        rbt =
            Vec3.add (vec3 0.5 -0.5 0.5) offset

        rbb =
            Vec3.add (vec3 0.5 -0.5 -0.5) offset

        rfb =
            Vec3.add (vec3 0.5 0.5 -0.5) offset

        lfb =
            Vec3.add (vec3 -0.5 0.5 -0.5) offset

        lbb =
            Vec3.add (vec3 -0.5 -0.5 -0.5) offset

        red =
            vec3 1 1 1

        white =
            vec3 0.2 0.2 0.3

        darkblue =
            vec3 0.003 0.003 0.251
    in
    face lfb rfb rft lft texturePosition white
        >> face rft rfb rbb rbt texturePosition white
        >> face lbt lft rft rbt texturePosition white
        >> face rfb lfb lbb rbb texturePosition red
        >> face lbb lfb lft lbt texturePosition white
        >> face rbb lbb lbt rbt texturePosition white


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec2 -> Vec3 -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
face a b c d texturePosition color =
    (::) ( Vertex a texturePosition color, Vertex b texturePosition color, Vertex c texturePosition color )
        >> (::) ( Vertex c texturePosition color, Vertex d texturePosition color, Vertex a texturePosition color )



-- Shaders


type alias Vertex =
    { position : Vec3
    , texturePosition : Vec2
    , color : Vec3
    }


type alias Uniform =
    { texture : Texture
    , textureSize : Vec2
    , camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    }


type alias Varying =
    { vcolor : Vec3
    }


texturedVertexShader : Shader Vertex Uniform Varying
texturedVertexShader =
    [glsl|
        precision mediump float;
        attribute vec3 color;
        attribute vec3 position;
        attribute vec2 texturePosition;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 transform;
        varying vec3 vcolor;

        void main () {
            vec4 textureColor = texture2D(texture, texturePosition / textureSize);
            vcolor = color;
            if (dot(textureColor, textureColor) == 4.0) {
                gl_Position = vec4(-2.0, -2.0, -2.0, 1.0);
            } else {
                gl_Position = perspective * camera * transform * vec4(position, 1.0);
            }
        }
    |]


texturedFragmentShader : Shader {} Uniform Varying
texturedFragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
