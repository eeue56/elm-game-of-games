module Drawing where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red, black, toRgb, rgb)

import Array exposing (Array)
import Model exposing (..)

collageWidth = 4000
collageHeight = 4000

drawRect : Model -> Float -> Float -> Int -> Form
drawRect model x y value = 
  let 
    originX = (model.rectSize / 2) - (toFloat model.width / 2) 
    originY = (toFloat model.height / 2) - model.rectSize / 2
  in 
    rect model.rectSize model.rectSize
      |> filled (rgb (value * 255) 0 0 )
      |> move (originX + model.rectSize * x, originY - (model.rectSize * y))

{-| 
Assume that the canvas has already been drawn as black
  -}
fastDrawRect : Model -> Shape -> (Float, Float) -> Float -> Float -> Int -> Form
fastDrawRect model myRect (originX, originY) x y value = 
    myRect
      |> filled red
      |> move (originX + model.rectSize * x, originY - (model.rectSize * y))


drawRow : Model -> Int -> Array Int -> List Form
drawRow model j rowArray =
  let
    rekt (i, v) = (drawRect model) (toFloat i) (toFloat j) (v)
  in
    List.map rekt
    <| List.filter (\(_, x) -> x > 0) 
    <| Array.toIndexedList rowArray 

fastDrawRow : Model -> (Float, Float) -> Int -> Array Int -> List Form
fastDrawRow model (originX, originY) j rowArray =
  let
    myRect = rect model.rectSize model.rectSize
    floatJ = toFloat j
    rekt (i, v) = (fastDrawRect model myRect (originX, originY)) (toFloat i) (floatJ) (v)
  in
    List.map rekt
    <| List.filter (\(_, x) -> x > 0) 
    <| Array.toIndexedList rowArray 


drawArray : Model -> List Form
drawArray model =
  (List.concat << Array.toList) 
    <| Array.indexedMap (drawRow model) model.board

fastDrawArray : Model -> List Form
fastDrawArray model =
  let
    originX = (model.rectSize / 2) - (toFloat model.width / 2) 
    originY = (toFloat model.height / 2) - model.rectSize / 2
  in
    (List.concat << Array.toList) 
      <| Array.indexedMap (fastDrawRow model (originX, originY)) model.board

background width height =
  rect (toFloat width) (toFloat height)
    |> filled black

--draw : Model -> Element
draw model = 
  collage model.width model.height
    <| (background model.width model.height) :: (fastDrawArray model)