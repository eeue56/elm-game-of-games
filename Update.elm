module Update where

import Time

import Mouse
import Keyboard

import Window

import Model exposing (..)
import GameLogic exposing (..)


type Clicked = OnClick | OffClick

type Update = 
  MouseClick Int Int Clicked | 
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

clickSignal : Signal Update
clickSignal = 
  let 
    clickChecker isDown (x, y) shiftDown =
      if isDown then MouseClick x y (if shiftDown then OffClick else OnClick) 
        else Noop
  in
    Signal.map3 clickChecker Mouse.isDown Mouse.position Keyboard.shift
metronome = Time.every (Time.second / 4)

updateModel : Update -> Model -> Model
updateModel action model =
  case action of
    NextStep bool -> if
      | bool -> addClick { model | board <- gameStep model.board }
      | otherwise -> model
    --WindowResize (x, y) -> { model | width <- x }
    TickerStep -> if model.autoplay then updateModel (NextStep True) model else model
    ToggleAutoplay -> addDebug (toString model.autoplay) { model | autoplay <- not model.autoplay}
    MouseClick x y clickType -> case clickType of
      OffClick -> { model | board <- off model.board <| toSquare model x y} 
      OnClick -> { model | board <- on model.board <| toSquare model x y }
    SaveBoardAsInit -> { model | initBoard <- model.board }
    Reset -> resetModel model
    _ -> model