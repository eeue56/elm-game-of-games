module GameLogic where

import Model exposing (..)
import Array exposing (Array)

arrayUpdate : Board -> (Board -> Int -> Int -> Int) -> Board
arrayUpdate array f = 
  Array.indexedMap (\y n -> Array.indexedMap (\x _ -> f array x y) n) array

toggle : Board -> (Int, Int) -> Board
toggle board (i, j) = update board i j (\x -> (x + 1) % 2)

on : Board -> (Int, Int) -> Board
on board (i, j) = fastSet board i j 1

off : Board -> (Int, Int) -> Board
off board (i, j) = fastSet board i j 0

xSteps : Board -> Int -> Board
xSteps board x =
  if x <= 0 then board
  else xSteps (gameStep board) (x - 1) 


gameStep : Board -> Board
gameStep array = arrayUpdate array fastGameRule

gameRule : Array (Array Int) -> Int -> Int -> Int
gameRule array i j =
  let 
    neighbours = livingNeighbours array i j
    populate x neighbours = if
      | x == 0 -> if neighbours == 3 then 1 else 0
      | neighbours == 2 || neighbours == 3 -> 1
      | otherwise -> 0
      
  in
    case matrixGet array i j of
      Just x -> populate x neighbours
      Nothing -> 0

-- when x is 0
bringToLife neighbours = 
  if neighbours == 3 then 1 else 0
-- when x is > 0
alreadyAlive neighbours = 
  if neighbours == 2 || neighbours == 3 then 1 else 0
  
fastGameRule : Array (Array Int) -> Int -> Int -> Int
fastGameRule array i j =
  let 
    neighbours = fastLivingNeighbours array i j
  in
    case matrixGet array i j of
      Just x -> if x == 0 then bringToLife neighbours else alreadyAlive neighbours
      Nothing -> 0

livingNeighbours : Board -> Int -> Int -> Int
livingNeighbours array i j = 
  let 
    guardedGet di dj = 
      if (di, dj) /= (0, 0) then matrixGet array (i + di) (j + dj) else Nothing
    binSwap x =  case x of 
      Just x -> x 
      Nothing -> 0
  in
    List.sum 
      <| List.map binSwap 
      <| List.concat 
      <| List.map (\x -> List.map (guardedGet x) [-1, 0, 1]) [-1, 0, 1] 

fastLivingNeighbours : Board -> Int -> Int -> Int
fastLivingNeighbours array i j =
  let 
    grab di dj = case matrixGet array (i + di) (j + dj) of
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

matrixGet : Array (Array a) -> Int -> Int -> Maybe a 
matrixGet array i j = 
  case Array.get j array of
    Just x ->  Array.get i x
    Nothing -> Nothing

update : Board -> Int -> Int -> (Int -> Int) -> Board
update m i j f = 
    let
        row = case Array.get j m of
          Just r -> r 
          Nothing -> Array.empty
        element = case Array.get i row of
          Just e -> e
          Nothing -> 0
    in
        Array.set j (Array.set i (f element) row) m

fastSet : Board -> Int -> Int -> Int -> Board
fastSet m i j v = 
  let 
    row = case Array.get j m of
      Just r -> r
      Nothing -> Array.empty
  in
    Array.set j (Array.set i v row) m