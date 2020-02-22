module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, div)
import Html.Attributes as Attr
import Keyboard
import Keyboard.Arrows
import Task


type Msg
    = ScreenSize Int Int
    | Tick Float
    | Resources Resources.Msg
    | Keys Keyboard.Msg


type alias Model =
    { mario : Mario
    , resources : Resources
    , keys : List Keyboard.Key
    , time : Float
    , screen : ( Int, Int )
    , camera : Camera
    , flags : Flags
    }


type alias Mario =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    }


type Direction
    = Up
    | Down
    | Left
    | Right


mario : Mario
mario =
    { x = 8
    , y = 8
    , vx = 0
    , vy = 0
    , dir = Right
    }


type alias Flags =
    { pink : String
    , red : String
    , map1Png : String
    , map1Tmx : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { mario = mario
      , resources = Resources.init
      , keys = []
      , time = 0
      , screen = ( 32 * 16, 32 * 16 )
      , camera = Camera.fixedArea (16 * 10) ( 0, 0 )
      , flags = flags
      }
    , Cmd.batch
        [ Cmd.map Resources (Resources.loadTextures [ flags.red, flags.map1Png ])
        , Task.perform (\{ viewport } -> ScreenSize (round viewport.width) (round (viewport.height - 32))) getViewport
        ]
    )


type alias Input =
    { x : Int
    , y : Int
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenSize width height ->
            ( { model | screen = ( width, height ) }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | mario = tick dt model.keys model.mario
                , time = dt + model.time
                , camera = Camera.moveTo ( model.mario.x, model.mario.y + 0.75 ) model.camera
              }
            , Cmd.none
            )

        Resources rMsg ->
            ( { model | resources = Resources.update rMsg model.resources }
            , Cmd.none
            )

        Keys keyMsg ->
            let
                keys =
                    Keyboard.update keyMsg model.keys
            in
            ( { model | keys = keys }, Cmd.none )


tick : Float -> List Keyboard.Key -> Mario -> Mario
tick dt keys guy =
    let
        arrows =
            Keyboard.Arrows.arrows keys
    in
    guy
        |> gravity dt
        |> walkX arrows
        |> walkY arrows
        |> physics dt


gravity : Float -> Mario -> Mario
gravity dt guy =
    guy


physics : Float -> Mario -> Mario
physics dt guy =
    { guy
        | x = guy.x + (dt * guy.vx)
        , y = guy.y + (dt * guy.vy)
    }


walkX : Input -> Mario -> Mario
walkX keys guy =
    { guy
        | vx = toFloat keys.x * 1.8
        , dir =
            if keys.x < 0 then
                Left

            else if keys.x > 0 then
                Right

            else
                guy.dir
    }


walkY : Input -> Mario -> Mario
walkY keys guy =
    { guy
        | vy = toFloat keys.y
        , dir =
            if keys.y < 0 then
                Left

            else if keys.y > 0 then
                Right

            else
                guy.dir
    }


render : Model -> List Renderable
render ({ resources, camera, flags } as model) =
    [ renderMario flags resources model.mario
    , Render.spriteWithOptions
        { position = ( 0, 0, 0 )
        , size = ( 16, 16 )
        , texture = Resources.getTexture flags.map1Png resources
        , rotation = 0
        , pivot = ( 0, 0 )
        , tiling = ( 1, 1 )
        }
    ]


renderMario : Flags -> Resources -> Mario -> Renderable
renderMario flags resources { x, y, dir } =
    let
        d =
            if dir == Left then
                -1

            else
                1
    in
    Render.animatedSpriteWithOptions
        { position = ( x, y, 0 )
        , size = ( 1, 1 )
        , texture = Resources.getTexture flags.red resources
        , bottomLeft = ( 0, 0 )
        , topRight = ( 1, 1 )
        , duration = 1
        , numberOfFrames = 1
        , rotation = 0
        , pivot = ( 0, 0 )
        }


view : Model -> Html msg
view ({ time, screen } as model) =
    div [ Attr.style "overflow" "hidden", Attr.style "width" "100%", Attr.style "height" "100%" ]
        [ Game.render
            { camera = model.camera
            , time = time
            , size = screen
            }
            (render model)
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { update = update
        , init = init
        , view = view
        , subscriptions = subs
        }


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ onResize ScreenSize
        , Sub.map Keys Keyboard.subscriptions
        , onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
        ]
