import Keyboard
import Mouse
import Window

import Matrix

import Time
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Boards exposing (..)
import Model exposing (..)
import Drawing exposing (..)
import GameLogic exposing (..)
import Update exposing (..)

rectSize = 100
collageWidth = rectSize * 128
collageHeight = rectSize * 20

-- 128 * 50 = 12800 / 2 = 12799 
-- 1024
-- 512 x 2
-- 256 x 4
-- 128 x 8
-- 64 x 16
-- 123 6

-- 23 * 100 + 10 = 2310
-- 24 * 50 + 20 = 1220
-- 24 * 50 + 20 = 1220
-- 24 * 24 + 40 = 616

boardWidth = collageWidth // rectSize
boardHeight = collageHeight // rectSize

board' : Board
board' =  
  Boards.emptyBoard boardWidth boardHeight

  --Boards.stampBoard 5 9 Boards.sheepsBeautifulGithubAvatar
  --  <| Boards.stampBoard 5 3 Boards.noahsLessBeautifulGithubAvatar 
--board' = Matrix.repeat 5 5 1

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
  draw model `above` show model.debug

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




