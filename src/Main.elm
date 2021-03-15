module Main exposing (main)

import Browser
import Coordinate exposing (Coordinate)
import Css exposing (..)
import Game exposing (Game, currentPlayer)
import Game.Resolution exposing (WinPath(..))
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
    | CaptureRandom Int
    | ThinkingPause Coordinate


type Msg
    = StartSinglePlayerGame
    | StartTwoPlayerGame
    | GameMsg InGameMsg
    | Escape
    | Again ( Player, Player )


doCaptureRandom game =
    let
        emptyNum =
            GameBoard.randomEmptySpaceGenerator game.board
    in
    Random.generate CaptureRandom emptyNum |> Cmd.map GameMsg


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
                        |> Game.nextTurn

                cmd =
                    if Game.isFinished updatedGame then
                        Cmd.none

                    else
                        case (Game.currentPlayer updatedGame).typ of
                            HumanPlayer ->
                                Cmd.none

                            RandomPlayer ->
                                doCaptureRandom updatedGame
            in
            ( { model | state = InGame updatedGame }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Escape ->
            ( initialModel, Cmd.none )

        Again players ->
            let
                -- Using the existing players means that the loser gets to go first in the next game or swaps in case of a draw
                newGame =
                    Game.new players

                cmd =
                    case currentPlayer newGame |> .typ of
                        HumanPlayer ->
                            Cmd.none

                        RandomPlayer ->
                            doCaptureRandom newGame
            in
            ( Model (InGame newGame), cmd )

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

                CaptureRandom emptySpaceNum ->
                    case model.state of
                        Menu ->
                            -- doesn't happen
                            ( model, Cmd.none )

                        InGame game ->
                            ( model, doCapture <| .coordinate <| GameBoard.nthEmptySpace emptySpaceNum game.board )

                ThinkingPause coordinate ->
                    let
                        delay seconds message =
                            Process.sleep (seconds * 1000)
                                |> Task.andThen (always <| Task.succeed message)
                                |> Task.perform identity
                    in
                    ( model, coordinate |> Capture |> GameMsg |> delay 0.8 )


boardSpaceDim =
    50


boardSpaceBorder =
    1


boardSpaceView : Player -> GameBoardSpace -> Html Msg
boardSpaceView currentPlayer boardSpace =
    let
        styling =
            css
                [ border2 (px boardSpaceBorder) solid
                , height (px boardSpaceDim)
                , width (px boardSpaceDim)
                , padding (px 0)
                ]
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


winnerMessage : Player -> String
winnerMessage player =
    let
        basicMessage =
            "The " ++ Player.Mark.toString player.mark ++ "'s Win!"
    in
    if player.typ == RandomPlayer then
        basicMessage ++ " ... Wait... Are you serious?? The random computer won?!"

    else
        basicMessage


winPathStyles : WinPath -> List Style
winPathStyles winPath =
    let
        crossPadding =
            boardSpaceDim / 10

        crossWidth =
            3

        crossDim =
            3 * boardSpaceDim + 2 * boardSpaceBorder - 2 * crossPadding

        diagCrossDim =
            sqrt (2 * (crossDim - crossWidth) * (crossDim - crossWidth))

        crossOpacity =
            0.25

        topPos n =
            (n - 1) * boardSpaceDim + n * boardSpaceBorder + boardSpaceDim / 2 - crossWidth / 3 - 1

        leftPos =
            topPos

        cStyles n =
            [ height (px crossDim)
            , borderLeft2 (px crossWidth) solid
            , position absolute
            , top (px (boardSpaceBorder + crossPadding))
            , left (px (leftPos n))
            , opacity (num crossOpacity)
            ]

        rStyles n =
            [ width (px crossDim)
            , borderTop2 (px crossWidth) solid
            , position absolute
            , top (px (topPos n))
            , left (px (boardSpaceBorder + crossPadding))
            , opacity (num crossOpacity)
            ]

        diagStyles rot =
            [ transform (rotate (deg rot))
            , height (px diagCrossDim)
            , borderLeft2 (px crossWidth) solid
            , position absolute
            , top (px (topPos 2 - (diagCrossDim / 2) + (2 * boardSpaceBorder)))
            , left (px (leftPos 2))
            , opacity (num crossOpacity)
            ]
    in
    case winPath of
        D1 ->
            diagStyles -45

        D2 ->
            diagStyles 45

        R1 ->
            rStyles 1

        R2 ->
            rStyles 2

        R3 ->
            rStyles 3

        C1 ->
            cStyles 1

        C2 ->
            cStyles 2

        C3 ->
            cStyles 3


finishedView : Game -> Html Msg
finishedView game =
    let
        message =
            if isJust game.winner then
                game.winner
                    |> Maybe.map (Tuple.first >> winnerMessage)
                    |> Maybe.withDefault "You're finished!"

            else
                "It's a draw!"

        winPathEls =
            case game.winner of
                Just ( _, winPaths ) ->
                    List.map (\winPath -> div [ css (winPathStyles winPath) ] []) winPaths

                Nothing ->
                    []
    in
    div []
        [ text message
        , div []
            [ button [ onClick <| Again game.players ] [ text "Play Again" ]
            , button [ onClick Escape ] [ text "Menu" ]
            ]
        , div [] winPathEls
        ]


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

                gameIsFinished =
                    Game.isFinished game

                mTurnHtml =
                    if not gameIsFinished then
                        Just [ div [] [ text <| Player.Mark.toString currentPlayer.mark ++ "'s Turn" ] ]

                    else
                        Nothing

                tableHtml =
                    game.board
                        |> gameRowsView (Game.currentPlayer game)
                        |> Html.Styled.table [ css [ borderCollapse collapse, textAlign center, cursor pointer ] ]

                mFinishedHtml =
                    if gameIsFinished then
                        Just [ finishedView game ]

                    else
                        Nothing
            in
            div [ css [ position relative ] ] (List.concat [ [ tableHtml ], Maybe.withDefault [] mTurnHtml, Maybe.withDefault [] mFinishedHtml ])


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
