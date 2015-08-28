module Model where

import Array exposing (Array)

type alias Board = Array (Array Int)

type alias Model = {
  initBoard : Board,
  board : Board, 

  width : Int,
  height : Int,
  rectSize : Float,
  
  iterations : Int, 
  autoplay: Bool, 
  mouseDown : Bool,
  
  debug : String }