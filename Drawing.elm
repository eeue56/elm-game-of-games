module Drawing where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red, black, toRgb, rgb)

import Array exposing (Array)
import Matrix
import Model exposing (..)

collageWidth = 8000
collageHeight = 8000

{-| 
Assume that the canvas has already been drawn as black
  -}
drawRect : Model -> Form -> (Float, Float) -> Float -> Float -> Int -> Form
drawRect model myRect (originX, originY) x y value = 
    myRect
      |> move (originX + (model.rectSize * x), originY - (model.rectSize * y))

drawArray : Model -> List Form
drawArray model =
  let
    myRect = rect model.rectSize model.rectSize |> filled red
    originX = (model.rectSize / 2) - (toFloat model.width / 2) 
    originY = (toFloat model.height / 2) - model.rectSize / 2
    drawRect' x y v = drawRect model myRect (originX, originY) (toFloat x) (toFloat y) v
  in
    --[drawRect' 62 29 1]
    List.map (\((i, j), v) -> drawRect' i j v)
      <| Array.toList
      <| Array.filter (\(_, v) -> v > 0) 
      <| Matrix.toIndexedArray model.board



background width height =
  rect (toFloat width) (toFloat height)
    |> filled black

--draw : Model -> Element
draw model = 
  collage model.width model.height
    <| (background model.width model.height) :: (drawArray model)