module Boards where

import Array exposing (..)
type alias Board = Array (Array Int)

matrixGet i j m = case Array.get j m of
  Just x ->  Array.get i x
  Nothing -> Nothing

matrixSet : Int -> Int -> Int -> Board -> Board
matrixSet i j v m = 
    let
        row : Array Int
        row = case Array.get j m of Just r -> r
    in
        Array.set j (Array.set i v row) m


lonelyBoard = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0] ]

oscillatorRow = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [1, 1, 1] ]

oscillatorCol = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [1],
        [1],
        [1] ]

stillSquare = 
  Array.fromList
    <| List.map Array.fromList
    <|
      [ [1, 1],
        [1, 1] ]

rect width height =  List.concat <| List.map (\x-> List.map2 (,) [0..(width-1)] <| List.repeat width x) [0..(height-1)]

stampBoard : Int -> Int -> Board -> Board -> Board
stampBoard x y stamp board = 
    let
        transfer : (Int,Int) -> Board -> Board
        transfer (i,j) board = matrixSet (x+i) (y+j) (case matrixGet i j stamp of Just v -> v) board
        stampHeight = (Array.length stamp)
        stampWidth = case Array.get 0 stamp of Just v -> Array.length v
    in
        (List.foldl (>>) identity <| List.map transfer <| rect stampWidth stampHeight) board

emptyBoard width height = Array.fromList <|  List.map (\x -> Array.repeat width 0) [0..(height-1)] 

fullBoard width height = Array.fromList <|  List.map (\x -> Array.repeat width 1) [0..(height-1)] 