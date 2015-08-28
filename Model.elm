module Model where

import Array exposing (Array)

type alias Board = Array (Array Int)

type alias Model = {
  rectSize : Float,
  initBoard : Board,
  board : Board, 
  clicks : Int, 
  autoplay: Bool, 
  debug : String,
  width : Int,
  height : Int }