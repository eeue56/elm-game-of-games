import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (..)

import Color exposing (red, black)

rectSize = 5

main =
  show <| gameStep <| board

drawRect : Float -> Float -> Int -> Form
drawRect x y value =
  rect rectSize rectSize
    |> filled (if value > 0 then red else black)
    |> move (rectSize * x, rectSize * y)

drawRow i rowArray =
  let
    rekt j v = drawRect (toFloat i) (toFloat j) (v)
  in
     Array.toList <| Array.indexedMap rekt rowArray

draw : Array (Array Int) -> Form
draw array = 
  toForm <| collage 500 500
    <| (List.concat << Array.toList) <| Array.indexedMap (\i arr -> drawRow i arr) array

gameStep : Array (Array Int) -> Array (Array Int)
gameStep array = let
    updateBlock x y = gameRule array x y
  in
    Array.indexedMap (\x n -> Array.indexedMap (\y m -> updateBlock x y) n) array

gameRule : Array (Array Int) -> Int -> Int -> Int
gameRule array i j =
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

livingNeighbours : Array (Array Int) -> Int -> Int ->Int
livingNeighbours array i j = 
  let 
    guardedGet = (\x y -> if (x /= j) && (y /= j) then matrixGet array x y else Nothing)
    binSwap x =  case x of 
        Just _ -> 1 
        Nothing -> 0
  in
  List.sum <| List.map binSwap <| List.concat <| List.map (\x -> List.map (guardedGet x) [i-1..i+1]) [j-1..j+1] 

matrixGet : Array (Array a) -> Int -> Int -> Maybe a 
matrixGet array i j =case Array.get j array of
  Just x ->  Array.get i x
  Nothing -> Nothing

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