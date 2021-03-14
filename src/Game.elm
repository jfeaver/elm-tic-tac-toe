module Game exposing (..)

import Coordinate exposing (Coordinate)
import Game.Resolution
import GameBoard exposing (GameBoard, GameBoardSpace)
import Maybe.Extra exposing (isJust)
import Player exposing (Player)
import Player.Mark exposing (PlayerMark(..))
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


new : ( Player, Player ) -> Game
new players =
    Game players GameBoard.empty False Nothing


singlePlayer : Game
singlePlayer =
    new ( Player.human XMark, Player.bot OMark )


twoPlayer : Game
twoPlayer =
    new ( Player.human XMark, Player.human OMark )


isFinished : Game -> Bool
isFinished game =
    game.isDraw || isJust game.winner


nextTurn : Game -> Game
nextTurn game =
    { game | players = Tuple2.swap game.players }


capture : Game -> Coordinate -> Game
capture game coordinate =
    let
        player =
            currentPlayer game

        mark =
            player.mark |> Player.Mark.toGameBoardMark

        updatedBoard =
            GameBoard.capture coordinate (GameBoardSpace mark coordinate) game.board

        winner =
            if Game.Resolution.wonBy player updatedBoard then
                Just player

            else
                Nothing
    in
    { game
        | board = updatedBoard
        , isDraw = GameBoard.isDraw updatedBoard
        , winner = winner
    }
