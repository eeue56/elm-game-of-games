import Keyboard

import Window

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
  clicks = 0,
  autoplay = False,
  debug = "",
  width = collageWidth,
  height = collageHeight }


view model = 
  draw model

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




