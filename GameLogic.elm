module GameLogic where

import Model exposing (..)
import Array exposing (Array)

import Matrix


toggle : Board -> (Int, Int) -> Board
toggle board (i, j) = Matrix.update i j (\x -> (x + 1) % 2) board

on : Board -> (Int, Int) -> Board
on board (i, j) = Matrix.set j i 1 board

off : Board -> (Int, Int) -> Board
off board (i, j) = Matrix.set j i 0 board

xSteps : Board -> Int -> Board
xSteps board x =
  if x <= 0 then board
  else xSteps (gameStep board) (x - 1) 

gameStep : Board -> Board
gameStep board = Matrix.indexedMap (\i j v -> fastGameRule board i j v) board

-- when x is 0
bringToLife neighbours = 
  if neighbours == 3 then 1 else 0
-- when x is > 0
alreadyAlive neighbours = 
  if neighbours == 2 || neighbours == 3 then 1 else 0
  
fastGameRule : Board -> Int -> Int -> Int -> Int
fastGameRule board i j v =
  let 
    neighbours = fastLivingNeighbours board i j
  in
    if v == 0 then bringToLife neighbours else alreadyAlive neighbours

fastLivingNeighbours : Board -> Int -> Int -> Int
fastLivingNeighbours board i j =
  let 
    grab di dj = case Matrix.get (i + di) (j + dj) board of
      Just x -> x
      Nothing -> 0
  in
    List.sum
      <| 
        [
          -- left
          grab -1 -1, 
          grab -1 0, 
          grab -1 1, 

          -- middle, exclude center
          grab 0 -1, 
          grab 0 1, 

          -- right
          grab 1 -1, 
          grab 1 0,
          grab 1 1
        ]