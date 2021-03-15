module GameBoard exposing
    ( GameBoard
    , GameBoardSpace
    , Mark(..)
    , capture
    , empty
    , foldl
    , hasMark
    , isDraw
    , isMark
    , map
    , nthEmptySpace
    , randomEmptySpaceGenerator
    )

import Array
import Coordinate exposing (Coordinate)
import Grid exposing (Grid)
import Maybe.Extra exposing (isJust)
import Random exposing (Generator)


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
    , captured : Int
    }


randomEmptySpaceGenerator : GameBoard -> Generator Int
randomEmptySpaceGenerator gameBoard =
    Random.int 1 (9 - gameBoard.captured)


isEmptySpace : GameBoardSpace -> Bool
isEmptySpace gameBoardSpace =
    gameBoardSpace.mark == Empty


isMark : Mark -> GameBoardSpace -> Bool
isMark mark gameBoardSpace =
    gameBoardSpace.mark == mark


empty : GameBoard
empty =
    GameBoard (Grid.initialize 3 3 (\x y -> GameBoardSpace Empty ( x, y ))) 0


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


type alias EmptySpaceFinderStep =
    { space : GameBoardSpace
    , count : Int
    , emptySpaceNumber : Int
    }


emptySpaceFinder : GameBoardSpace -> EmptySpaceFinderStep -> EmptySpaceFinderStep
emptySpaceFinder checkingSpace { space, count, emptySpaceNumber } =
    if count == emptySpaceNumber || not (isEmptySpace checkingSpace) then
        EmptySpaceFinderStep space count emptySpaceNumber

    else
        EmptySpaceFinderStep checkingSpace (count + 1) emptySpaceNumber


nthEmptySpace : Int -> GameBoard -> GameBoardSpace
nthEmptySpace n gameBoard =
    let
        initialFoldValue =
            EmptySpaceFinderStep (GameBoardSpace Empty ( -1, -1 )) 0 n
    in
    foldl emptySpaceFinder initialFoldValue gameBoard
        |> .space


foldl : (GameBoardSpace -> a -> a) -> a -> GameBoard -> a
foldl func acc { grid } =
    Grid.foldl func acc grid


isDraw : GameBoard -> Bool
isDraw =
    foldl (\gameBoardSpace all -> all && (not <| isEmptySpace gameBoardSpace)) True


isEmpty : Coordinate -> GameBoard -> Bool
isEmpty coordinate { grid } =
    Grid.get coordinate grid
        |> Maybe.Extra.filter isEmptySpace
        |> isJust


hasMark : Mark -> Coordinate -> GameBoard -> Maybe Mark
hasMark mark coordinate { grid } =
    Grid.get coordinate grid
        |> Maybe.Extra.filter (isMark mark)
        |> Maybe.map (always mark)


set : Coordinate -> GameBoardSpace -> GameBoard -> GameBoard
set coordinate gameBoardSpace gameBoard =
    { gameBoard | grid = Grid.set coordinate gameBoardSpace gameBoard.grid }


capture : Coordinate -> GameBoardSpace -> GameBoard -> GameBoard
capture coordinate gameBoardSpace gameBoard =
    let
        captured =
            if isEmpty coordinate gameBoard then
                gameBoard.captured + 1

            else
                gameBoard.captured

        setGameBoard =
            set coordinate gameBoardSpace gameBoard
    in
    { setGameBoard | captured = captured }
