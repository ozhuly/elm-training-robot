module Main exposing (..)

import Browser
import Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Robot
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Robot1Msg Robot.Msg
    | Robot2Msg Robot.Msg


type alias Model =
    { robot1 : Robot.Model, robot2 : Robot.Model }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( { robot1 =
            Robot.init 10
                10
                "ArrowUp"
                "ArrowLeft"
                "ArrowDown"
                "ArrowRight"
                " "
      , robot2 =
            Robot.init 70
                10
                "w"
                "a"
                "s"
                "d"
                "f"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Robot1Msg robotmsg ->
            ( { model
                | robot1 =
                    Robot.update robotmsg
                        model.robot1
              }
            , Cmd.none
            )

        Robot2Msg robotmsg ->
            ( { model
                | robot2 =
                    Robot.update robotmsg
                        model.robot2
              }
            , Cmd.none
            )


gothurt : Robot.Model -> Robot.Model -> Bool
gothurt defendingRobot shootingRobot =
    (defendingRobot.top1 + 5 >= shootingRobot.bulletY)
        && (defendingRobot.top1 - 5 <= shootingRobot.bulletY)
        && (defendingRobot.left1 + 5 >= shootingRobot.bulletX)
        && (defendingRobot.left1 - 5 <= shootingRobot.bulletX)
        && shootingRobot.bulletExist


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        _ =
            Debug.log "got hurt" (gothurt model.robot2 model.robot1)
    in
    Sub.batch
        [ Sub.map Robot1Msg <|
            Robot.subscriptions model.robot1 (gothurt model.robot1 model.robot2)
        , Sub.map Robot2Msg <|
            Robot.subscriptions model.robot2 (gothurt model.robot2 model.robot1)
        ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.map Robot1Msg <| Robot.view model.robot1 "blue" "1690"
        , Html.map Robot2Msg <| Robot.view model.robot2 "red" "1574"
        ]
