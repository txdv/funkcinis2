module Encode where
  
import Helper
  
showPointStr:: [String] -> String
showPointStr (x:y:[]) = "[\"" ++ x ++ "\",\"" ++ y ++ "\"]"  
showPointStr [] = "[]"  

showFinalMoves:: Move -> String
showFinalMoves (MoveFirst point) = "[\"coord\","++ showPointStr (showPoint point)  ++ "]" 
showFinalMoves current@(MovePrev point status move) = "[\"coord\"," ++ showPointStr (showPoint point) ++ "," ++ "\"result\",\"" ++ show status ++ "\",\"prev\"," ++ showFinalMoves move ++ "]"