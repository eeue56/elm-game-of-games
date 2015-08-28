import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


import Array exposing (..)

import Time

import Mouse
import Keyboard
import Boards

import Window

import Color exposing (red, black, toRgb, rgb)

type Update = 
  MouseClick Int Int | 
  NextStep Bool | 
  WindowResize (Int, Int) | 
  TickerStep | 
  ToggleAutoplay | 
  SaveBoardAsInit |
  Reset | 
  Noop 

type alias Board = Array (Array Int)

type alias Model = {
  initBoard : Board,
  board : Board, 
  clicks : Int, 
  autoplay: Bool, 
  debug : String,
  width : Int,
  height : Int }

collageWidth = 2000
collageHeight = 1500
rectSize = 10

boardWidth = collageWidth // rectSize
boardHeight = collageHeight // rectSize

board' : Board
board' =  
  Boards.stampBoard 5 9 Boards.sheepsBeautifulGithubAvatar
    <| Boards.stampBoard 5 3 Boards.noahsLessBeautifulGithubAvatar 
    <| Boards.emptyBoard boardWidth boardHeight

model : Model
model = {
  initBoard = board',
  board = board',
  clicks = 0,
  autoplay = False,
  debug = "",
  width = collageWidth,
  height = collageHeight }


resetModel model = 
  let
    resetBoard model = { model | board <- model.initBoard }
    resetClick model = { model | clicks <- 0 }
    resetAutoplay model = { model | autoplay <- False}
    resetDebug model = { model | debug <- ""}
  in
    resetBoard <| resetClick model

view model = 
  draw model

addClick model = { model | clicks <- model.clicks + 1 }
addDebug : String -> Model -> Model
addDebug msg model = { model | debug <- msg }

updateModel : Update -> Model -> Model
updateModel action model =
  case action of
    NextStep bool -> if
      | bool -> addClick { model | board <- gameStep model.board }
      | otherwise -> model
    --WindowResize (x, y) -> { model | width <- x }
    TickerStep -> if model.autoplay then updateModel (NextStep True) model else model
    ToggleAutoplay -> addDebug (toString model.autoplay) { model | autoplay <- not model.autoplay}
    MouseClick x y -> { model | board <- toggle model.board <| toSquare x y }
    SaveBoardAsInit -> { model | initBoard <- model.board }
    Reset -> resetModel model
    _ -> model

clickSignal = Signal.map2 (\isDown (x,y) -> if isDown then MouseClick x y else Noop) Mouse.isDown Mouse.position
metronome = Time.every (Time.second / 2)

model' : Signal Model
model' =
  Signal.foldp
    updateModel
    model
    <| Signal.mergeMany
      [
        clickSignal,
        Signal.map (\(x, y) -> WindowResize (x, y)) Window.dimensions,
        Signal.map NextStep <| Keyboard.isDown 78,
        Signal.map (\isDown -> if isDown then ToggleAutoplay else Noop) <| Keyboard.isDown 80,
        Signal.map (\_ -> TickerStep) <| metronome,
        Signal.map (\_ -> Reset) <| Keyboard.isDown 82,
        Signal.map (\_ -> SaveBoardAsInit) <| Keyboard.isDown 83
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
    originX = (rectSize / 2) - (collageWidth / 2) 
    originY = (collageHeight / 2) - rectSize / 2
  in 
    rect rectSize rectSize
      |> filled (rgb (value * 255) 0 0 )
      |> move (originX + rectSize * x, originY - (rectSize * y))

drawRow j rowArray =
  let
    rekt (i,v) = drawRect (toFloat i) (toFloat j) (v)
  in
    List.map rekt
    <| List.filter (\(_, x) -> x > 0) 
    <| Array.toIndexedList rowArray

drawArray : Board -> List Form
drawArray array =
  (List.concat << Array.toList) <| Array.indexedMap (drawRow) array

background =
  rect collageWidth collageHeight
    |> filled black

draw : Model -> Element
draw model = 
  collage model.width model.height
    <| background :: drawArray model.board

arrayUpdate : Board -> (Board -> Int -> Int -> Int) -> Board
arrayUpdate array f = 
  Array.indexedMap (\y n -> Array.indexedMap (\x m -> f array x y) n) array

toggle : Board -> (Int, Int) -> Board
toggle board (i, j) = update board i j (\x -> (x + 1) % 2)

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