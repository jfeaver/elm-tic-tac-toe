module Main exposing (main)

import Browser
import Coordinate exposing (Coordinate)
import Extra exposing (delay, isJust)
import Game exposing (Game)
import GameBoard exposing (GameBoard, GameBoardSpace, Mark(..))
import Html exposing (Html, button, div, h1, table, td, text, tr)
import Html.Events exposing (onClick)
import Player exposing (..)
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
    | GameMsg InGameMsg


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
        StartSinglePlayerGame ->
            let
                updatedModel =
                    Game.singlePlayer
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
                    ( model, coordinate |> Capture |> GameMsg |> delay 0.8 )


boardSpaceView : Player -> GameBoardSpace -> Html Msg
boardSpaceView currentPlayer boardSpace =
    case boardSpace.mark of
        Empty ->
            let
                action =
                    if currentPlayer.typ == HumanPlayer then
                        [ onClick <| GameMsg (Capture boardSpace.coordinate) ]

                    else
                        []
            in
            td action [ text "." ]

        X ->
            td [] [ text "X" ]

        O ->
            td [] [ text "O" ]


gameRowsView : Player -> GameBoard -> List (Html Msg)
gameRowsView currentPlayer =
    GameBoard.map (tr []) (boardSpaceView currentPlayer)


winnerMessage player =
    let
        basicMessage =
            "The " ++ toMarkString player.mark ++ "'s Win!"
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
    div [] [ text message ]


stateView : Model -> Html Msg
stateView model =
    case model.state of
        Menu ->
            div [] [ button [ onClick StartSinglePlayerGame ] [ text "Play Single Player" ] ]

        InGame game ->
            let
                tableHtml =
                    game.board
                        |> gameRowsView (Game.currentPlayer game)
                        |> table []

                mFinishedHtml =
                    if Game.isFinished game then
                        Just <| [ finishedView game ]

                    else
                        Nothing
            in
            div [] (List.append [ tableHtml ] (Maybe.withDefault [] mFinishedHtml))


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , stateView model
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
