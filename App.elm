import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


import Array exposing (..)

import Mouse
import Keyboard

import Color exposing (red, black)

type Update = NextStep Bool | Reset | Noop

rectSize = 25
--board = Array.fromList <|  List.map (\x -> Array.repeat 5 0) [0..4] 
--board' =
--  Array.fromList 
--    <| [ Array.repeat 5 0,
--           Array.repeat 5 0,
--           Array.repeat 5 1,
--           Array.repeat 5 1,
--           Array.repeat 5 0]
           
board' = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [1, 1, 0],
        [1, 1, 0],
        [0, 0, 0] ]

model = {
  board = board',
  clicks = 0 }


resetModel model = 
  let
    resetBoard model = { model | board <- board'}
    resetClick model = { model | clicks <- 0 }
  in
    resetBoard <| resetClick model

view model = above (container 500 100 middle <| show model.clicks) <| draw <| model.board

addClick model = { model | clicks <- model.clicks + 1 }

updateModel action model =
  case action of
    NextStep bool -> if
      | bool -> addClick { model | board <- gameStep model.board }
      | otherwise -> model
    Reset -> resetModel model
    _ -> model

stepSignal = 
  Signal.merge 
    (Signal.map (NextStep) Mouse.isDown)
    (Signal.map NextStep <| Keyboard.isDown 78)

model' =
  Signal.foldp
    updateModel
    model
    <| Signal.merge 
        stepSignal
        (Signal.map (\_ -> Reset) <| Keyboard.isDown 82)
     

main = Signal.map (view) model'

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

drawArray array =
  (List.concat << Array.toList) <| Array.indexedMap (drawRow) array


draw : Array (Array Int) -> Element
draw array = 
  collage 500 500
    <| drawArray array

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
      | neighbours < 2 -> 0
      | neighbours > 3 -> 0
      | x == 0  -> if neighbours == 3 then 1 else 0
      | otherwise -> 1
      
  in
    case matrixGet array i j of
      Just x -> populate x neighbours
      Nothing -> 0

isMe : (Int, Int) -> (Int, Int) -> Bool
isMe (x, y) (i, j) = x == i && y == j

withinBounds : Int -> Int -> Bool
withinBounds x y = x > -1 && y > -1

livingNeighbours : Array (Array Int) -> Int -> Int ->Int
livingNeighbours array i j = 
  let 
    isMe' = isMe i j
    guardedGet x y = if withinBounds x y && not <| isMe' (x, y) then matrixGet array x y else Nothing
    binSwap x =  case x of 
        Just x -> x 
        Nothing -> 0
  in
    List.sum 
      <| List.map binSwap 
      <| List.concat 
      <| List.map (\x -> List.map (guardedGet x) [i-1..i+1]) [j-1..j+1] 

matrixGet : Array (Array a) -> Int -> Int -> Maybe a 
matrixGet array i j = case Array.get j array of
  Just x ->  Array.get i x
  Nothing -> Nothing

update array i j f = 
  let 
    updatePart x = case Array.get j x of
      Just y -> Array.set j (Array.set i (f y) x) array
      Nothing -> array
  in
  case Array.get i array of
    Just x -> updatePart x
    Nothing -> array
