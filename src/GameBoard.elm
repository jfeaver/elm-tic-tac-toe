module GameBoard exposing (GameBoard, GameBoardSpace, Mark(..), empty, hasMark, isDraw, isEmpty, isMark, map, set)

import Array
import Coordinate exposing (Coordinate)
import Grid exposing (Grid)
import Maybe.Extra


type Mark
    = Empty
    | X
    | O


type alias GameBoardSpace =
    { mark : Mark
    , coordinate : Coordinate
    }


type alias GameBoard =
    Grid GameBoardSpace


isEmptySpace : GameBoardSpace -> Bool
isEmptySpace gameBoardSpace =
    gameBoardSpace.mark == Empty


isMark : Mark -> GameBoardSpace -> Bool
isMark mark gameBoardSpace =
    gameBoardSpace.mark == mark


empty : GameBoard
empty =
    Grid.initialize 3 3 (\x y -> GameBoardSpace Empty ( x, y ))


map : (List a -> a) -> (GameBoardSpace -> a) -> GameBoard -> List a
map rowMapper spaceMapper gameBoard =
    let
        gameBoardLists =
            Grid.rows
                >> Array.map Array.toList
                >> Array.toList
    in
    gameBoard
        |> gameBoardLists
        |> List.map (\row -> rowMapper (List.map spaceMapper row))


isDraw : GameBoard -> Bool
isDraw =
    Grid.foldl (\gameBoardSpace all -> all && (not <| isEmptySpace gameBoardSpace)) True


isEmpty : Coordinate -> GameBoard -> Maybe Coordinate
isEmpty coordinate gameBoard =
    Grid.get coordinate gameBoard
        |> Maybe.Extra.filter isEmptySpace
        |> Maybe.map (always coordinate)


hasMark : Mark -> Coordinate -> GameBoard -> Maybe Mark
hasMark mark coordinate gameBoard =
    Grid.get coordinate gameBoard
        |> Maybe.Extra.filter (isMark mark)
        |> Maybe.map (always mark)


set : Coordinate -> GameBoardSpace -> GameBoard -> GameBoard
set coordinate gameBoardSpace =
    Grid.set coordinate gameBoardSpace
