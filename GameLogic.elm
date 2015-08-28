module GameLogic where

import Model exposing (..)
import Array exposing (Array)

arrayUpdate : Board -> (Board -> Int -> Int -> Int) -> Board
arrayUpdate array f = 
  Array.indexedMap (\y n -> Array.indexedMap (\x m -> f array x y) n) array

toggle : Board -> (Int, Int) -> Board
toggle board (i, j) = update board i j (\x -> (x + 1) % 2)

on : Board -> (Int, Int) -> Board
on board (i, j) = update board i j (\_ -> 1)

off : Board -> (Int, Int) -> Board
off board (i, j) = update board i j (\_ -> 0)


gameStep : Board -> Board
gameStep array = arrayUpdate array gameRule

gameRule : Array (Array Int) -> Int -> Int -> Int
gameRule array i j =
  let 
    neighbours = livingNeighbours array i j
    populate x neighbours = if
      | neighbours < 2 -> 0
      | neighbours > 3 -> 0
      | x == 0 -> if neighbours == 3 then 1 else 0
      | otherwise -> 1
      
  in
    case matrixGet array i j of
      Just x -> populate x neighbours
      Nothing -> 0

livingNeighbours : Board -> Int -> Int ->Int
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
      <| List.map (\x -> List.map (guardedGet x) [-1..1]) [-1..1] 

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