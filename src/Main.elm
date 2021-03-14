module Main exposing (main)

import Browser
import Coordinate exposing (Coordinate)
import Css exposing (border2, borderCollapse, center, collapse, cursor, height, margin, pointer, px, solid, textAlign, width)
import Game exposing (Game)
import GameBoard exposing (GameBoard, GameBoardSpace, Mark(..))
import Html.Styled exposing (Html, button, div, h1, table, td, text, toUnstyled, tr)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra exposing (isJust)
import Player exposing (..)
import Player.Mark
import Process
import Random
import Task


type State
    = Menu
    | InGame Game


type alias Model =
    { state : State }


initialModel : Model
initialModel =
    Model Menu


type InGameMsg
    = Capture Coordinate
    | CaptureRandom Coordinate
    | ThinkingPause Coordinate


type Msg
    = StartSinglePlayerGame
    | StartTwoPlayerGame
    | GameMsg InGameMsg
    | Escape


randomCoordinate =
    -- FIXME This could be smarter if I picked from known empty spaces
    Random.pair (Random.int 0 2) (Random.int 0 2)


doCaptureRandom =
    Random.generate CaptureRandom randomCoordinate |> Cmd.map GameMsg


doCapture coordinate =
    coordinate
        |> ThinkingPause
        |> Task.succeed
        |> Task.perform GameMsg


captureUpdate : Model -> Coordinate -> ( Model, Cmd Msg )
captureUpdate model coordinate =
    case model.state of
        Menu ->
            -- doesn't happen
            ( model, Cmd.none )

        InGame game ->
            let
                updatedGame =
                    Game.capture game coordinate

                cmd =
                    if Game.isFinished updatedGame then
                        Cmd.none

                    else
                        case (Game.currentPlayer updatedGame).typ of
                            HumanPlayer ->
                                Cmd.none

                            RandomPlayer ->
                                doCaptureRandom
            in
            ( { model | state = InGame updatedGame }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Escape ->
            ( initialModel, Cmd.none )

        StartSinglePlayerGame ->
            let
                updatedModel =
                    Game.singlePlayer
                        |> InGame
                        |> Model
            in
            ( updatedModel, Cmd.none )

        StartTwoPlayerGame ->
            let
                updatedModel =
                    Game.twoPlayer
                        |> InGame
                        |> Model
            in
            ( updatedModel, Cmd.none )

        GameMsg inGameMsg ->
            case inGameMsg of
                Capture coordinate ->
                    captureUpdate model coordinate

                CaptureRandom coordinate ->
                    case model.state of
                        Menu ->
                            -- doesn't happen
                            ( model, Cmd.none )

                        InGame game ->
                            if Maybe.withDefault False (GameBoard.isEmpty coordinate game.board) then
                                ( model, doCapture coordinate )

                            else
                                ( model, doCaptureRandom )

                ThinkingPause coordinate ->
                    let
                        delay seconds message =
                            Process.sleep (seconds * 1000)
                                |> Task.andThen (always <| Task.succeed message)
                                |> Task.perform identity
                    in
                    ( model, coordinate |> Capture |> GameMsg |> delay 0.8 )


boardSpaceView : Player -> GameBoardSpace -> Html Msg
boardSpaceView currentPlayer boardSpace =
    let
        styling =
            css [ border2 (px 1) solid, height (px 25), width (px 25) ]
    in
    case boardSpace.mark of
        Empty ->
            let
                action =
                    if currentPlayer.typ == HumanPlayer then
                        [ onClick <| GameMsg (Capture boardSpace.coordinate) ]

                    else
                        []
            in
            td (styling :: action) [ text "" ]

        X ->
            td [ styling ] [ text "X" ]

        O ->
            td [ styling ] [ text "O" ]


gameRowsView : Player -> GameBoard -> List (Html Msg)
gameRowsView currentPlayer =
    GameBoard.map (tr []) (boardSpaceView currentPlayer)


winnerMessage player =
    let
        basicMessage =
            "The " ++ Player.Mark.toString player.mark ++ "'s Win!"
    in
    if player.typ == RandomPlayer then
        basicMessage ++ " ... Wait... Are you serious?? The random computer won?!"

    else
        basicMessage


finishedView game =
    let
        message =
            if isJust game.winner then
                game.winner
                    |> Maybe.map winnerMessage
                    |> Maybe.withDefault "You're finished!"

            else
                "It's a draw!"
    in
    div [] [ text message, div [] [ button [ onClick Escape ] [ text "Play Again" ] ] ]


stateView : Model -> Html Msg
stateView model =
    case model.state of
        Menu ->
            div []
                [ button [ onClick StartSinglePlayerGame ] [ text "Single Player" ]
                , button [ onClick StartTwoPlayerGame ] [ text "Two Player" ]
                ]

        InGame game ->
            let
                currentPlayer =
                    Game.currentPlayer game

                mTurnHtml =
                    [ div [] [ text <| Player.Mark.toString currentPlayer.mark ++ "'s Turn" ] ]
                        |> Just
                        |> Maybe.Extra.filter ((not <| Game.isFinished game) |> always)

                tableHtml =
                    game.board
                        |> gameRowsView (Game.currentPlayer game)
                        |> table [ css [ borderCollapse collapse, textAlign center, cursor pointer ] ]

                mFinishedHtml =
                    [ finishedView game ]
                        |> Just
                        |> Maybe.Extra.filter (Game.isFinished game |> always)
            in
            div [] (List.concat [ [ tableHtml ], Maybe.withDefault [] mTurnHtml, Maybe.withDefault [] mFinishedHtml ])


view : Model -> Html Msg
view model =
    div [ css [ margin (px 20) ] ]
        [ h1 [] [ text "Tic Tac Toe" ]
        , stateView model
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view >> toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }
