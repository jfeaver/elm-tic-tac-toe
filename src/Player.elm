module Player exposing (..)

import Player.Mark exposing (PlayerMark)


type PlayerType
    = HumanPlayer
    | RandomPlayer


type alias Player =
    { typ : PlayerType
    , mark : PlayerMark
    }


human =
    Player HumanPlayer


bot =
    Player RandomPlayer
