module Model where

import Array exposing (Array)
import Matrix exposing (Matrix)

type alias Board = Matrix Int

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