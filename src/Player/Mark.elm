module Player.Mark exposing (..)

import GameBoard exposing (Mark(..))


type PlayerMark
    = XMark
    | OMark


toGameBoardMark : PlayerMark -> Mark
toGameBoardMark pMark =
    case pMark of
        XMark ->
            X

        OMark ->
            O


toString : PlayerMark -> String
toString pMark =
    case pMark of
        XMark ->
            "X"

        OMark ->
            "O"
