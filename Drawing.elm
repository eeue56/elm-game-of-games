module Drawing where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red, black, toRgb, rgb)

import Array exposing (Array)
import Matrix
import Model exposing (..)

collageWidth = 4000
collageHeight = 4000

{-| 
Assume that the canvas has already been drawn as black
  -}
drawRect : Model -> Form -> (Float, Float) -> Float -> Float -> Int -> Form
drawRect model myRect (originX, originY) x y value = 
    myRect
      |> move (originX + model.rectSize * x, originY - (model.rectSize * y))

drawArray : Model -> List Form
drawArray model =
  let
    myRect = rect model.rectSize model.rectSize |> filled red
    originX = (model.rectSize / 2) - (toFloat model.width / 2) 
    originY = (toFloat model.height / 2) - model.rectSize / 2
    grab xs = case Matrix.map2 (\v d -> (v, d)) model.board xs of Just v -> v
    drawRect' x y v = drawRect model myRect (originX, originY) (toFloat x) (toFloat y) v
  in
    Array.toList 
      <| Array.map (\t -> snd t)
      <| Matrix.filter (\(v,_) -> v > 0) 
      <| grab
      <| Matrix.indexedMap (drawRect') model.board

background width height =
  rect (toFloat width) (toFloat height)
    |> filled black

--draw : Model -> Element
draw model = 
  collage model.width model.height
    <| (background model.width model.height) :: (drawArray model)