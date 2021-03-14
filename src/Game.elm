module Game exposing (..)

import Coordinate exposing (Coordinate)
import Game.Resolution
import GameBoard exposing (GameBoard, GameBoardSpace)
import Maybe.Extra exposing (isJust)
import Player exposing (Player)
import Player.Mark exposing (PlayerMark(..))
import Random exposing (Generator)
import Tuple2


{-| Current player is the first in the tuple
-}
type alias Game =
    { players : ( Player, Player )
    , board : GameBoard
    , isDraw : Bool
    , winner : Maybe Player
    }


currentPlayer : Game -> Player
currentPlayer game =
    game.players |> Tuple.first


singlePlayer : Game
singlePlayer =
    Game ( Player.human XMark, Player.bot OMark ) GameBoard.empty False Nothing


twoPlayer : Game
twoPlayer =
    Game ( Player.human XMark, Player.human OMark ) GameBoard.empty False Nothing


isFinished : Game -> Bool
isFinished game =
    game.isDraw || isJust game.winner


randomChoiceGenerator : Game -> Generator Coordinate
randomChoiceGenerator game =
    -- FIXME This could be smarter if I picked from known empty spaces
    Random.pair (Random.int 0 2) (Random.int 0 2)


capture : Game -> Coordinate -> Game
capture game coordinate =
    let
        player =
            game |> currentPlayer

        mark =
            player |> .mark |> Player.Mark.toGameBoardMark

        updatedBoard =
            GameBoard.set coordinate (GameBoardSpace mark coordinate) game.board

        winner =
            if Game.Resolution.wonBy player updatedBoard then
                Just player

            else
                Nothing
    in
    { game
        | board = updatedBoard
        , players = Tuple2.swap game.players
        , isDraw = GameBoard.isDraw updatedBoard
        , winner = winner
    }
