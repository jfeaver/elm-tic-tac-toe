module Game.Resolution exposing (wonBy)

import Binary exposing (Bits)
import Game.WinAlgorithmStep exposing (WinAlgorithmStep, steps)
import GameBoard exposing (GameBoard, Mark)
import Maybe.Extra
import Player exposing (Player)
import Player.Mark


b =
    Binary.fromIntegers


handleStep : Mark -> GameBoard -> WinAlgorithmStep -> ( Bool, Bits ) -> ( Bool, Bits )
handleStep mark gameBoard { coordinate, win, turnOff } ( isWon, winPathPossibilities ) =
    if isWon then
        ( isWon, winPathPossibilities )

    else if Binary.toDecimal winPathPossibilities == 0 then
        ( False, winPathPossibilities )

    else if Maybe.Extra.isJust (GameBoard.hasMark mark coordinate gameBoard) then
        ( Binary.toDecimal (Binary.and winPathPossibilities win) > 0, winPathPossibilities )

    else
        ( False, Binary.and winPathPossibilities turnOff )


wonBy : Player -> GameBoard -> Bool
wonBy player gameBoard =
    let
        gameBoardMark =
            Player.Mark.toGameBoardMark player.mark

        initialValue =
            ( False, b (List.repeat (gameBoard.size * 2 + 2) 1) )
    in
    List.foldl (handleStep gameBoardMark gameBoard) initialValue steps |> Tuple.first
