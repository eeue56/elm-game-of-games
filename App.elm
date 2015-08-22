import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (..)

import Color exposing (red, black)

rectSize = 5

main =
  show <| gameStep board 2 2 

drawRect x y value =
  rect rectSize rectSize
    |> filled (if value > 0 then red else black)
    |> move (rectSize * x) (rectSize * y)

drawRow i rowArray =
  Array.toList <| Array.indexedMap (\j v -> drawRect i j v) rowArray

draw : Array (Array a) -> Form
draw array = 
  collage [] 
    <| (List.concat << Array.toList) <| Array.indexedMap (\i arr -> drawRow i arr) array

gameStep : Array (Array a) -> Array (Array a)
gameRule array = let
    updateBlock x y = gameRule array x y
  in
    Array.indexedMap (\x n -> Array.indexedMap (\y m -> updateBlock x y)) 

gameRule : Array (Array a) -> Int -> Int -> Int
gameRule array i j 
  let 
    neighbours = livingNeighbours array i j
    populate x neighbours = if
      | x == 0 -> if 
        | neighbours > 1 || neighbours < 3 -> 1
        | otherwise -> 0
      | x == 1 -> if
        | neighbours == 2 -> 1
        | otherwise -> 0
  in
    case matrixGet array i j of
      Just x -> populate x neighbours
      Nothing -> 0

livingNeighbours : Array (Array a) -> Int -> Int ->Int
livingNeighbours array i j = 
  let 
    guardedGet = (\x y -> if x /= j and y /= j then matrixGet array x else Nothing)
  sum 
    List.map (\x -> 
      case x of 
        Just _ -> 1 
        Nothing -> 0)
    <| List.map (\x -> List.map (guardedGet x) [i-1..i+1]) [j-1..j+1] 

matrixGet : Array (Array a) -> Int -> Int -> Maybe a 
matrixGet array i j = Array.get i <| Array.get j array 

update array i j f = 
  let 
    updatePart x = case Array.get j x of
      Just y -> Array.set i (Array.set j (f y) x) array
      Nothing -> array
  in
  case Array.get i array of
    Just x -> updatePart x
    Nothing -> array
    
board = Array.fromList <|  List.map (\x -> Array.repeat 5 0) [0..4] 
