import Keyboard
import Mouse
import Window

import Time
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Boards
import Model exposing (..)
import Drawing exposing (..)
import GameLogic exposing (..)
import Update exposing (..)

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
  rectSize = rectSize,
  initBoard = board',
  board = board',
  iterations = 0,
  autoplay = False,
  mouseDown = False,
  debug = "",
  width = collageWidth,
  height = collageHeight }


view model =
  below (draw model) (show model.iterations)

clickSignal : Signal Update
clickSignal = 
  let 
    clickChecker isDown (x, y) shiftDown =
      if isDown then MouseClick x y (if shiftDown then OffClick else OnClick) 
        else NoMouse
  in
    Signal.map3 clickChecker Mouse.isDown Mouse.position Keyboard.shift

metronome = Time.fps 15

model' : Signal Model
model' =
  Signal.foldp
    updateModel
    model
    <| Signal.mergeMany
      [
        clickSignal,
        Signal.map (\(x, y) -> WindowResize (x, y)) Window.dimensions,
        Signal.map NextStep <| Signal.merge (Keyboard.isDown 78) (Keyboard.isDown 39),
        Signal.map (\isDown -> if isDown then ToggleAutoplay else Noop) <| Keyboard.isDown 80,
        Signal.map (\_ -> TickerStep) <| metronome,
        Signal.map (\_ -> Reset) <| Keyboard.isDown 82,
        Signal.map (\_ -> SaveBoardAsInit) <| Keyboard.isDown 83,
        Signal.map (\_ -> BackStep 1) <| Keyboard.isDown 37
      ]
     

main = Signal.map view model'




