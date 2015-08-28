module Update where

import Time

import Mouse
import Keyboard

import Window

import Model exposing (..)
import GameLogic exposing (..)


type Update = 
  MouseClick Int Int | 
  NextStep Bool | 
  WindowResize (Int, Int) | 
  TickerStep | 
  ToggleAutoplay | 
  SaveBoardAsInit |
  Reset | 
  Noop 


toSquare : Model -> Int -> Int -> (Int, Int)
toSquare model x y = 
  (
    x // round model.rectSize,
    y // round model.rectSize
  )

resetModel model = 
  let
    resetBoard model = { model | board <- model.initBoard }
    resetClick model = { model | clicks <- 0 }
    resetAutoplay model = { model | autoplay <- False}
    resetDebug model = { model | debug <- ""}
  in
    resetBoard <| resetClick model

addClick : Model -> Model
addClick model = { model | clicks <- model.clicks + 1 }

addDebug : String -> Model -> Model
addDebug msg model = { model | debug <- msg }

clickSignal = Signal.map2 (\isDown (x,y) -> if isDown then MouseClick x y else Noop) Mouse.isDown Mouse.position
metronome = Time.every (Time.second / 2)

updateModel : Update -> Model -> Model
updateModel action model =
  case action of
    NextStep bool -> if
      | bool -> addClick { model | board <- gameStep model.board }
      | otherwise -> model
    --WindowResize (x, y) -> { model | width <- x }
    TickerStep -> if model.autoplay then updateModel (NextStep True) model else model
    ToggleAutoplay -> addDebug (toString model.autoplay) { model | autoplay <- not model.autoplay}
    MouseClick x y -> { model | board <- toggle model.board <| toSquare model x y }
    SaveBoardAsInit -> { model | initBoard <- model.board }
    Reset -> resetModel model
    _ -> model