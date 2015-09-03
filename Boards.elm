module Boards where

import Model exposing (..)

import Matrix
import Convert
import Array exposing (..)

guaranteedStamp = Convert.defaultMaybe (Matrix.fromList) (Matrix.repeat 1 1 0)

sheepsBeautifulGithubAvatar = 
  guaranteedStamp
    <|
      [ [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 1, 0, 1, 0],
        [1, 1, 0, 1, 1] ]

noahsLessBeautifulGithubAvatar = 
  guaranteedStamp
    <|
      [ [0, 1, 1, 1, 0],
        [0, 1, 1, 1, 0],
        [0, 0, 1, 0, 0],
        [1, 1, 1, 1, 1],
        [0, 1, 1, 1, 0] ]

lonelyBoard = 
  guaranteedStamp
    <|
      [ [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0] ]

oscillatorRow = 
  guaranteedStamp
    <|
      [ [1, 1, 1] ]

oscillatorCol = 
  guaranteedStamp
    <|
      [ [1],
        [1],
        [1] ]

stillSquare = 
  guaranteedStamp
    <|
      [ [1, 1],
        [1, 1] ]

rect width height =  List.concat <| List.map (\x-> List.map2 (,) [0..(width-1)] <| List.repeat width x) [0..(height-1)]

stampBoard : Int -> Int -> Board -> Board -> Board
stampBoard x y stamp board = 
    let
        transfer : (Int,Int) -> Board -> Board
        transfer (i,j) board = Matrix.set (x+i) (y+j) (
          case Matrix.get i j stamp of 
            Just v -> v
            Nothing -> -1
          ) board
        stampHeight = snd stamp.size
        stampWidth = fst stamp.size
    in
        (List.foldl (>>) identity <| List.map transfer <| rect stampWidth stampHeight) board

emptyBoard width height = Matrix.repeat width height 0 

fullBoard width height = Matrix.repeat width height 1