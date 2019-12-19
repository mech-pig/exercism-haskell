{-# LANGUAGE NamedFieldPuns #-}

module Minesweeper (annotate) where

import           Data.Char (intToDigit)
import qualified Data.Map as Map
import           Data.Maybe (Maybe(..))


annotate :: [String] -> [String]
annotate = displayBoard . parseBoard

parseBoard :: [String] -> State
parseBoard boardString =
  foldlIx
    (\rowState (row, y) ->
      foldlIx
        (\colState (cell, x) -> updateBoard cell (x, y) (updateWidth x colState))
        (updateHeight y rowState)
        row
    )
    initialState
    boardString

displayBoard :: State -> [String]
displayBoard State { board, height, width } =
  map
    (\y ->
      map
        (\x -> displayCell (x, y) board)
        [ 0..(width-1) ]
    )
    [ 0..(height-1) ]
  where
    displayCell :: Position -> Board -> Char
    displayCell position board =
      case Map.findWithDefault (Empty 0) position board of
        (Empty 0)     -> ' '
        (Empty count) -> intToDigit count
        otherwise     -> '*'

data State = State
  { board  :: Board
  , width  :: Int
  , height :: Int
  } deriving (Show)

type Board = Map.Map Position Cell

type Position = (Int, Int)

data Cell = Mine | Empty AdjacentMineCount
  deriving (Show)

type AdjacentMineCount = Int

initialState :: State
initialState = State
  { board  = Map.empty
  , width  = 0
  , height = 0
  }

updateHeight :: Int -> State -> State
updateHeight i state = state { height = max (i+1) (height state) }

updateWidth :: Int -> State -> State
updateWidth i state = state { width = max (i+1) (width state) }

updateBoard :: Char -> Position -> State -> State
updateBoard '*' position state = state { board = handleMineSymbol  position (board state) }
updateBoard  _  position state = state { board = handleEmptySymbol position (board state) }

handleMineSymbol :: Position -> Board -> Board
handleMineSymbol position board =
  foldl
    increaseAdjacentMineCount
    (Map.insert position Mine board)
    (getNeighbors position)

handleEmptySymbol :: Position -> Board -> Board
handleEmptySymbol _ board = board

increaseAdjacentMineCount :: Board -> Position -> Board
increaseAdjacentMineCount board position = Map.alter increaseMineCount position board
  where
    increaseMineCount :: Maybe Cell -> Maybe Cell
    increaseMineCount Nothing     = Just (Empty 1)
    increaseMineCount (Just cell) = Just (addMineCount cell)

    addMineCount :: Cell -> Cell
    addMineCount Mine          = Mine
    addMineCount (Empty count) = Empty (count + 1)

getNeighbors :: Position -> [Position]
getNeighbors (x, y) =
  [ (x + 1, y)
  , (x + 1, y - 1)
  , (x    , y - 1)
  , (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x    , y + 1)
  , (x + 1, y + 1)
  ]

foldlIx :: (b -> (a, Int) -> b) -> b -> [a] -> b
foldlIx f b as = foldl f b (zip as [ 0.. ])
