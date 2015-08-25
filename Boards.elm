module Boards where

import Array

lonelyBoard = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0] ]

oscillator = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [0, 0, 0],
        [1, 1, 1],
        [0, 0, 0] ]

emptyBoard = Array.fromList <|  List.map (\x -> Array.repeat 5 0) [0..4] 

fullBoard = Array.fromList <|  List.map (\x -> Array.repeat 5 1) [0..4] 