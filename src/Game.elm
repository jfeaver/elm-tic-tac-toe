module Game exposing (..)

import Coordinate exposing (Coordinate)
import Extra exposing (isJust)
import Game.Resolution
import GameBoard exposing (GameBoard, GameBoardSpace)
import Player exposing (Player, PlayerMark(..))
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


isFinished : Game -> Bool
isFinished game =
    game.isDraw || isJust game.winner


capture : Game -> Coordinate -> Game
capture game coordinate =
    let
        player =
            game |> currentPlayer

        mark =
            player |> .mark |> Player.toGameMark

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
