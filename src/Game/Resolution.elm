module Game.Resolution exposing (wonBy)

import Binary exposing (Bits)
import Coordinate exposing (Coordinate)
import GameBoard exposing (GameBoard, Mark)
import Player exposing (Player)
import Player.Mark


type alias WinAlgorithmStep =
    { coordinate : Coordinate
    , win : Bits
    , turnOff : Bits
    }


b =
    Binary.fromIntegers


{-| Each step is visiting the given coordinate. We check for the player's mark.
If the mark is not found then we turn off win path possibilities using a binary
`and` and the turnOff value. If the mark is found then we check for a win from
the current possibilities and the win value, again using `and`.
-}
steps : List WinAlgorithmStep
steps =
    [ { coordinate = ( 0, 0 ), win = b [ 0 ], turnOff = b [ 1, 0, 1, 1, 0, 1, 1 ] }
    , { coordinate = ( 1, 1 ), win = b [ 0 ], turnOff = b [ 1, 0, 1, 1, 0, 1 ] }
    , { coordinate = ( 2, 2 ), win = b [ 1, 0, 0, 0, 0, 0, 0, 0 ], turnOff = b [ 1, 1, 1, 0, 1, 1, 0 ] }
    , { coordinate = ( 2, 0 ), win = b [ 0 ], turnOff = b [ 1, 0, 0, 1, 1, 1, 1, 0 ] }
    , { coordinate = ( 0, 2 ), win = b [ 1, 0, 0, 0, 0, 0, 0 ], turnOff = b [ 1, 0, 1, 1, 0, 0, 1, 1 ] }
    , { coordinate = ( 1, 0 ), win = b [ 1, 0, 0, 0, 0, 0 ], turnOff = b [ 1, 1, 0, 1, 1, 1, 0, 1 ] }
    , { coordinate = ( 0, 1 ), win = b [ 1, 0, 0 ], turnOff = b [ 1, 1, 1, 0, 1, 0, 1, 1 ] }
    , { coordinate = ( 2, 1 ), win = b [ 1, 0, 0, 0, 1 ], turnOff = b [ 1, 1, 1, 0, 1, 1, 1, 0 ] }
    , { coordinate = ( 1, 2 ), win = b [ 1, 0, 1, 0 ], turnOff = b [ 1, 1, 1, 1, 0, 1, 0, 1 ] }
    ]


handleStep : Mark -> GameBoard -> WinAlgorithmStep -> ( Bool, Bits ) -> ( Bool, Bits )
handleStep mark gameBoard { coordinate, win, turnOff } ( isWon, winPathPossibilities ) =
    if isWon then
        ( isWon, winPathPossibilities )

    else if Binary.toDecimal winPathPossibilities == 0 then
        ( False, winPathPossibilities )

    else if Maybe.withDefault False <| GameBoard.hasMark mark coordinate gameBoard then
        ( Binary.toDecimal (Binary.and winPathPossibilities win) > 0, winPathPossibilities )

    else
        ( False, Binary.and winPathPossibilities turnOff )


wonBy : Player -> GameBoard -> Bool
wonBy player gameBoard =
    let
        gameBoardMark =
            Player.Mark.toGameBoardMark player.mark

        initialValue =
            ( False, b [ 1, 1, 1, 1, 1, 1, 1, 1 ] )
    in
    List.foldl (handleStep gameBoardMark gameBoard) initialValue steps |> Tuple.first
