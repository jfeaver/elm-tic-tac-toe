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
    { grid : Grid GameBoardSpace
    , size : Int
    }


isEmptySpace : GameBoardSpace -> Bool
isEmptySpace gameBoardSpace =
    gameBoardSpace.mark == Empty


isMark : Mark -> GameBoardSpace -> Bool
isMark mark gameBoardSpace =
    gameBoardSpace.mark == mark


empty : Int -> GameBoard
empty size =
    let
        grid =
            Grid.initialize size size (\x y -> GameBoardSpace Empty ( x, y ))
    in
    GameBoard grid size


map : (List a -> a) -> (GameBoardSpace -> a) -> GameBoard -> List a
map rowMapper spaceMapper gameBoard =
    let
        gameBoardLists =
            Grid.rows
                >> Array.map Array.toList
                >> Array.toList
    in
    gameBoard.grid
        |> gameBoardLists
        |> List.map (\row -> rowMapper (List.map spaceMapper row))


isDraw : GameBoard -> Bool
isDraw { grid } =
    grid
        |> Grid.foldl (\gameBoardSpace all -> all && (not <| isEmptySpace gameBoardSpace)) True


isEmpty : Coordinate -> GameBoard -> Maybe Coordinate
isEmpty coordinate { grid } =
    Grid.get coordinate grid
        |> Maybe.Extra.filter isEmptySpace
        |> Maybe.map (always coordinate)


hasMark : Mark -> Coordinate -> GameBoard -> Maybe Mark
hasMark mark coordinate { grid } =
    Grid.get coordinate grid
        |> Maybe.Extra.filter (isMark mark)
        |> Maybe.map (always mark)


set : Coordinate -> GameBoardSpace -> GameBoard -> GameBoard
set coordinate gameBoardSpace gameBoard =
    { gameBoard | grid = Grid.set coordinate gameBoardSpace gameBoard.grid }
