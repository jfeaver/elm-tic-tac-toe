module Player exposing (..)

import GameBoard exposing (Mark(..))


type PlayerType
    = HumanPlayer
    | RandomPlayer


type PlayerMark
    = XMark
    | OMark


toGameMark pMark =
    case pMark of
        XMark ->
            X

        OMark ->
            O


toMarkString pMark =
    case pMark of
        XMark ->
            "X"

        OMark ->
            "O"


type alias Player =
    { typ : PlayerType
    , mark : PlayerMark
    }


human =
    Player HumanPlayer


bot =
    Player RandomPlayer
