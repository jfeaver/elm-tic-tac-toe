module Game.Resolution exposing (WinPath(..), wonBy)

import Array
import Binary exposing (Bits)
import Coordinate exposing (Coordinate)
import GameBoard exposing (GameBoard, Mark)
import Player exposing (Player)
import Player.Mark


type alias WinAlgorithmStep =
    { coordinate : Coordinate
    , turnOff : Bits
    }


type WinPath
    = D1
    | D2
    | R1
    | R2
    | R3
    | C1
    | C2
    | C3


b =
    Binary.fromIntegers


indexToWinPath : Int -> Maybe WinPath
indexToWinPath index =
    Array.fromList [ D1, D2, R1, R2, R3, C1, C2, C3 ]
        |> Array.get index


collectWinPaths : ( Maybe WinPath, Bool ) -> List WinPath -> List WinPath
collectWinPaths winPathResult winners =
    if Tuple.second winPathResult then
        let
            mWinPath =
                Tuple.first winPathResult
        in
        case mWinPath of
            Just winPath ->
                winPath :: winners

            Nothing ->
                winners

    else
        winners


winBitsToWinPaths : Bits -> List WinPath
winBitsToWinPaths winBits =
    winBits
        |> Binary.toBooleans
        |> List.indexedMap (indexToWinPath >> Tuple.pair)
        |> List.foldl collectWinPaths []


{-| Each step is visiting the given coordinate. We check for the player's mark.
If the mark is not found then we turn off win path possibilities using a binary
`and` and the turnOff value.
-}
steps : List WinAlgorithmStep
steps =
    [ { coordinate = ( 0, 0 ), turnOff = b [ 1, 0, 1, 1, 0, 1, 1 ] }
    , { coordinate = ( 1, 1 ), turnOff = b [ 1, 0, 1, 1, 0, 1 ] }
    , { coordinate = ( 2, 2 ), turnOff = b [ 1, 1, 1, 0, 1, 1, 0 ] }
    , { coordinate = ( 2, 0 ), turnOff = b [ 1, 0, 0, 1, 1, 1, 1, 0 ] }
    , { coordinate = ( 0, 2 ), turnOff = b [ 1, 0, 1, 1, 0, 0, 1, 1 ] }
    , { coordinate = ( 1, 0 ), turnOff = b [ 1, 1, 0, 1, 1, 1, 0, 1 ] }
    , { coordinate = ( 0, 1 ), turnOff = b [ 1, 1, 1, 0, 1, 0, 1, 1 ] }
    , { coordinate = ( 2, 1 ), turnOff = b [ 1, 1, 1, 0, 1, 1, 1, 0 ] }
    , { coordinate = ( 1, 2 ), turnOff = b [ 1, 1, 1, 1, 0, 1, 0, 1 ] }
    ]


handleStep : Mark -> GameBoard -> WinAlgorithmStep -> Bits -> Bits
handleStep mark gameBoard { coordinate, turnOff } winPathPossibilities =
    if Binary.toDecimal winPathPossibilities == 0 then
        winPathPossibilities

    else
        case GameBoard.hasMark mark coordinate gameBoard of
            Just _ ->
                winPathPossibilities

            Nothing ->
                Binary.and winPathPossibilities turnOff


wonBy : Player -> GameBoard -> Maybe ( Player, List WinPath )
wonBy player gameBoard =
    let
        gameBoardMark =
            Player.Mark.toGameBoardMark player.mark
    in
    List.foldl (handleStep gameBoardMark gameBoard) (b (List.repeat 8 1)) steps
        |> winBitsToWinPaths
        |> (\winPaths ->
                if List.length winPaths > 0 then
                    Just (Tuple.pair player winPaths)

                else
                    Nothing
           )
