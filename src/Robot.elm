module Robot exposing (..)

import Browser
import Browser.Events
import Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Keyboard exposing (Key(..))
import Svg.Attributes
import Svg.Events
import Time


type alias Model =
    { left1 : Float
    , top1 : Float
    , bulletExist : Bool
    , bulletX : Float
    , bulletY : Float
    , downkey : String
    , leftkey : String
    , rightkey : String
    , upkey : String
    , shootkey : String
    , bulletSpeedX : Float
    , bulletSpeedY : Float
    , heading : Float
    , bulletHeading : Float
    , hp : Int
    , speedUp : Float
    , speedDown : Float
    , speedRight : Float
    , speedLeft : Float
    }


type Msg
    = KeyDown String
    | KeyUp String
    | BulletMove Time.Posix
    | RobotMove Time.Posix
    | Regroup Time.Posix
    | Rotate Time.Posix
    | HPdown Time.Posix


init : Float -> Float -> String -> String -> String -> String -> String -> Model
init left top upkey leftkey downkey rightkey shootkey =
    { left1 = left
    , top1 = top
    , bulletExist = False
    , bulletX = 0
    , bulletY = 0
    , downkey = downkey
    , leftkey = leftkey
    , rightkey = rightkey
    , upkey = upkey
    , shootkey = shootkey
    , bulletSpeedX = 2
    , bulletSpeedY = 0
    , heading = 0
    , bulletHeading = 0
    , hp = 3
    , speedUp = 0
    , speedDown = 0
    , speedRight = 0
    , speedLeft = 0
    }


bulletspeed : Float
bulletspeed =
    2


speed : Float
speed =
    0.25


update : Msg -> Model -> Model
update msg model =
    let
        _ =
            Debug.log "HP" model.hp
    in
    case msg of
        KeyDown input ->
            if input == model.upkey then
                { model
                    | speedUp = speed
                }

            else if input == model.downkey then
                { model
                    | speedDown = -speed
                }

            else if input == model.leftkey then
                { model
                    | speedLeft = -speed
                }

            else if input == model.rightkey then
                { model
                    | speedRight = speed
                }

            else if input == model.shootkey then
                { model
                    | bulletExist = True
                    , bulletSpeedX = bulletspeed * Basics.cos model.heading
                    , bulletSpeedY = bulletspeed * Basics.sin model.heading
                    , bulletHeading = model.heading
                }

            else
                model

        KeyUp input ->
            if input == model.upkey then
                { model
                    | speedUp = 0
                }

            else if input == model.downkey then
                { model
                    | speedDown = 0
                }

            else if input == model.leftkey then
                { model
                    | speedLeft = 0
                }

            else if input == model.rightkey then
                { model
                    | speedRight = 0
                }

            else
                model

        RobotMove _ ->
            { model
                | top1 = model.top1 - model.speedUp - model.speedDown
                , left1 = model.left1 + model.speedRight + model.speedLeft
            }

        BulletMove _ ->
            if model.bulletX > 100 || model.bulletX < 0 || model.bulletY > 100 || model.bulletY < 0 then
                { model
                    | bulletExist = False
                    , bulletX = model.left1
                    , bulletY = model.top1
                }

            else
                { model
                    | bulletX = model.bulletX + model.bulletSpeedX
                    , bulletY = model.bulletY + model.bulletSpeedY
                }

        Regroup _ ->
            { model | bulletX = model.left1, bulletY = model.top1 }

        Rotate _ ->
            if model.speedUp + model.speedDown == 0 && model.speedRight + model.speedLeft == 0 then
                model

            else
                { model | heading = (Basics.atan2 -(model.speedUp + model.speedDown) (model.speedRight + model.speedLeft) - model.heading) * 0.15 + model.heading }

        HPdown _ ->
            { model | hp = model.hp - 1 }


subscriptions : Model -> Bool -> Sub Msg
subscriptions model gotHurt =
    if model.bulletExist then
        if gotHurt then
            Sub.batch
                [ Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)
                , Browser.Events.onKeyUp (Json.Decode.map KeyUp <| Json.Decode.field "key" Json.Decode.string)
                , Time.every 10 BulletMove
                , Time.every 10 Rotate
                , Time.every 10 RobotMove
                , Time.every 9 HPdown
                ]

        else
            Sub.batch
                [ Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)
                , Browser.Events.onKeyUp (Json.Decode.map KeyUp <| Json.Decode.field "key" Json.Decode.string)
                , Time.every 10 BulletMove
                , Time.every 10 Rotate
                , Time.every 10 RobotMove
                ]

    else if gotHurt then
        Sub.batch
            [ Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)
            , Browser.Events.onKeyUp (Json.Decode.map KeyUp <| Json.Decode.field "key" Json.Decode.string)
            , Time.every 10 BulletMove
            , Time.every 10 Rotate
            , Time.every 10 RobotMove
            , Time.every 9 HPdown
            ]

    else
        Sub.batch
            [ Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)
            , Browser.Events.onKeyUp (Json.Decode.map KeyUp <| Json.Decode.field "key" Json.Decode.string)
            , Time.every 10 Regroup
            , Time.every 10 Rotate
            , Time.every 10 RobotMove
            ]


view : Model -> String -> String -> Html.Html Msg
view model color number =
    if model.hp <= 0 then
        Html.div []
            [ Html.div
                [ style "position" "absolute"
                , style "background-color" "black"
                , style "top" "0%"
                , style "left" "0%"
                , style "border" "solid"
                , style "border-width" "0px"
                , style "border-radius" "0px"
                , style "border-color" color
                ]
                [ Html.text number ]
            , Html.div
                [ style "position" "absolute"
                , style "background-color" "black"
                , style "top" ((++) (String.fromFloat model.bulletY) "%")
                , style "left" ((++) (String.fromFloat model.bulletX) "%")
                , style "height" "0%"
                , style "width" "0%"
                ]
                []
            ]

    else if not model.bulletExist then
        Html.div []
            [ Html.div
                [ style "position" "absolute"
                , style "background-color" "gray"
                , style "top" ((++) (String.fromFloat model.top1) "%")
                , style "left" ((++) (String.fromFloat model.left1) "%")
                , style "transform" <| (++) "rotate(" <| (++) (String.fromFloat model.heading) "rad)"
                , style "border" "solid"
                , style "border-width" "2px"
                , style "border-radius" "3px"
                , style "border-color" color
                ]
                [ Html.text number ]
            , Html.div
                [ style "position" "absolute"
                , style "background-color" "black"
                , style "top" ((++) (String.fromFloat model.bulletY) "%")
                , style "left" ((++) (String.fromFloat model.bulletX) "%")
                , style "height" "0%"
                , style "width" "0%"
                ]
                []
            ]

    else
        Html.div []
            [ Html.div
                [ style "position" "absolute"
                , style "background-color" "gray"
                , style "top" ((++) (String.fromFloat model.top1) "%")
                , style "left" ((++) (String.fromFloat model.left1) "%")
                , style "transform" <| (++) "rotate(" <| (++) (String.fromFloat model.heading) "rad)"
                , style "border" "solid"
                , style "border-radius" "3px"
                , style "border-color" color
                , style "border-width" "2px"
                ]
                [ Html.text number ]
            , Html.div
                [ style "position" "absolute"
                , style "background-color" color
                , style "top" ((++) (String.fromFloat model.bulletY) "%")
                , style "left" ((++) (String.fromFloat model.bulletX) "%")
                , style "height" "1%"
                , style "width" "1%"
                , style "transform" <| (++) "rotate(" <| (++) (String.fromFloat model.bulletHeading) "rad)"
                ]
                []
            ]
