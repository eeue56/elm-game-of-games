import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


import Array exposing (..)

import Mouse
import Keyboard
import Boards

import Color exposing (red, black, toRgb, rgb)

type Update = NextStep Bool | Reset | Noop
type alias Board = Array (Array Int)


rectSize = 25

board' : Board
board' = Boards.stampBoard 4 5 Boards.stillSquare <| Boards.stampBoard 1 1 Boards.oscillatorCol <| Boards.emptyBoard 10 15

model = {
  board = board',
  clicks = 0 }


resetModel model = 
  let
    resetBoard model = { model | board <- board'}
    resetClick model = { model | clicks <- 0 }
  in
    resetBoard <| resetClick model

view model = above (container 500 100 middle <| show model.clicks) <| draw model.board -- <| arrayUpdate model.board livingNeighbours

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
    |> filled (rgb (value * 255) 0 0 )
    |> move (rectSize * x, rectSize * y)

drawRow i rowArray =
  let
    rekt j v = drawRect (toFloat i) (toFloat j) (v)
  in
     Array.toList <| Array.indexedMap rekt rowArray

drawArray array =
  (List.concat << Array.toList) <| Array.indexedMap (drawRow) array


draw : Board -> Element
draw array = 
  collage 1000 750
    <| drawArray array

arrayUpdate : Board -> (Board -> Int -> Int -> Int) -> Board
arrayUpdate array f = Array.indexedMap (\x n -> Array.indexedMap (\y m -> f array x y) n) array

gameStep : Board -> Board
gameStep array = arrayUpdate array gameRule

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

livingNeighbours : Board -> Int -> Int ->Int
livingNeighbours array i j = 
  let 
    guardedGet x y = if (i, j) /= (x, y) then matrixGet array x y else Nothing
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
