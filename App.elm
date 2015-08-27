import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


import Array exposing (..)

import Mouse
import Keyboard
import Boards

import Color exposing (red, black, toRgb, rgb)

type Update = MouseClick Int Int | NextStep Bool | Reset | Noop
type alias Board = Array (Array Int)
type alias Model = {board : Board, clicks : Int, debug : String}

collageWidth = 1100
collageHeight = 400
boardWidth = 15
boardHeight = 10

rectSize = 25

board' : Board
board' =  Boards.stampBoard 1 3 Boards.noahsLessBeautifulGithubAvatar <| Boards.emptyBoard boardWidth boardHeight

model : Model
model = {
  board = board',
  clicks = 0,
  debug = "" }


resetModel model = 
  let
    resetBoard model = { model | board <- board'}
    resetClick model = { model | clicks <- 0 }
    resetDebug model = { model | debug <- ""}
  in
    resetBoard <| resetClick model

view model = 
  below (container 1000 20 middle <| show model.debug) <| below (container 500 20 middle <| show model.clicks) <| draw model.board  -- <| arrayUpdate model.board livingNeighbours

addClick model = { model | clicks <- model.clicks + 1 }
addDebug : String -> Model -> Model
addDebug msg model = { model | debug <- msg }

updateModel : Update -> Model -> Model
updateModel action model =
  case action of
    NextStep bool -> if
      | bool -> addClick { model | board <- gameStep model.board }
      | otherwise -> model
    MouseClick x y -> addDebug (toString <| toSquare x y) { model | board <- toggle model.board <| toSquare x y }
    Reset -> resetModel model
    _ -> model

clickSignal = Signal.map2 (\isDown (x,y) -> if isDown then MouseClick x y else Noop) Mouse.isDown Mouse.position

model' : Signal Model
model' =
  Signal.foldp
    updateModel
    model
    <| Signal.mergeMany
      [
        clickSignal,
        (Signal.map NextStep <| Keyboard.isDown 78),
        (Signal.map (\_ -> Reset) <| Keyboard.isDown 82)
      ]
     

main = Signal.map view model'

toSquare : Int -> Int -> (Int, Int)
toSquare x y = 
  (
    x // rectSize,
    y // rectSize
  )

drawRect : Float -> Float -> Int -> Form
drawRect x y value = 
  let 
    originX = -1* (collageWidth/2) + rectSize/2
    originY = (collageHeight/2) - rectSize/2
  in
    rect rectSize rectSize
      |> filled (rgb (value * 255) 0 0 )
      |> move (originX + rectSize * x, originY - (rectSize * y))

drawRow j rowArray =
  let
    rekt i v = drawRect (toFloat i) (toFloat j) (v)
  in
     Array.toList <| Array.indexedMap rekt rowArray

drawArray : Board -> List Form
drawArray array =
  (List.concat << Array.toList) <| Array.indexedMap (drawRow) array


draw : Board -> Element
draw array = 
  collage collageWidth collageHeight
    <| (filled (rgb 255 200 255) <| rect collageWidth collageHeight) :: drawArray array

arrayUpdate : Board -> (Board -> Int -> Int -> Int) -> Board
arrayUpdate array f = Array.indexedMap (\y n -> Array.indexedMap (\x m -> f array x y) n) array

toggle : Board -> (Int, Int) -> Board
toggle board (i,j) = update board i j (\x -> (x+1)%2 )

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
    guardedGet di dj = if (di, dj) /= (0, 0) then matrixGet array (i+di) (j+dj) else Nothing
    binSwap x =  case x of 
        Just x -> x 
        Nothing -> 0
  in
    List.sum 
      <| List.map binSwap 
      <| List.concat 
      <| List.map (\x -> List.map (guardedGet x) [-1..1]) [-1..1] 

matrixGet : Array (Array a) -> Int -> Int -> Maybe a 
matrixGet array i j = case Array.get j array of
  Just x ->  Array.get i x
  Nothing -> Nothing

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


--update array i j f = 
--  let 
--    updatePart x = case Array.get j x of
--      Just y -> Array.set j (Array.set i (f y) x) array
--      Nothing -> array
--  in
--    case Array.get i array of
--      Just x -> updatePart x
--      Nothing -> array
