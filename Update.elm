module Update where


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
  NoMouse |
  BackStep Int |
  Noop 


toSquare : Model -> Int -> Int -> (Int, Int)
toSquare model x y = 
  (
    x // round model.rectSize,
    y // round model.rectSize
  )

resetBoard model = { model | board <- model.initBoard }
resetClick model = { model | iterations <- 0 }
resetAutoplay model = { model | autoplay <- False}
resetDebug model = { model | debug <- ""}
resetModel model = 
  let
  in
    resetBoard <| resetClick model

addIteration : Model -> Model
addIteration model = { model | iterations <- model.iterations + 1 }

addDebug : String -> Model -> Model
addDebug msg model = { model | debug <- msg }

backIteration : Model -> Int -> Model
backIteration model steps = 
  let 
    newStep = (model.iterations - steps)
  in
    if newStep <= 0 then { model | board <- model.initBoard, iterations <- 0 }
    else
      { model | 
        board <- xSteps model.initBoard newStep, 
        iterations <- newStep }

updateModel : Update -> Model -> Model
updateModel action model =
  case action of
    NextStep bool -> if
      | bool -> addIteration { model | board <- gameStep model.board }
      | otherwise -> model
    --WindowResize (x, y) -> { model | width <- x }
    TickerStep -> if model.autoplay && not model.mouseDown then updateModel (NextStep True) model else model
    ToggleAutoplay -> addDebug (toString model.autoplay) { model | autoplay <- not model.autoplay}
    MouseClick x y clickType -> case clickType of
      OffClick -> { model | board <- off model.board <| toSquare model x y, mouseDown <- True} 
      OnClick -> { model | board <- on model.board <| toSquare model x y, mouseDown <- True}
    SaveBoardAsInit -> { model | initBoard <- model.board }
    Reset -> resetModel model
    NoMouse -> { model | mouseDown <- False}
    BackStep x -> backIteration model x
    _ -> model