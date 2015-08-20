import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (..)

main =
  show <| update board 2 2 (\_ -> 5)


update array i j f = 
  let 
    updatePart x = case Array.get j x of
      Just y -> Array.set i (Array.set j (f y) x) array
      Nothing -> array
  in
  case Array.get i array of
    Just x -> updatePart x
    Nothing -> array
    
board = Array.fromList <|  List.map (\x -> Array.repeat 5 0) [0..4] 
