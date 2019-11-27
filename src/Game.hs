module Game where
  
import Parser
import Helper
import Encode
--import Remote
import System.Random.Shuffle

getMovePoint:: [Point] -> Move -> Either String (Maybe Point)
getMovePoint _ (NotMove e) = Right Nothing
getMovePoint (coord:rest) allMoves = if 
  (checkIfMoveExists (show (MoveFirst coord)) (map show (listMoves [] allMoves))) then (getMovePoint rest allMoves) else (Right (Just coord)) 
getMovePoint_ _ = Right Nothing

checkIfMoveExists move list = move `elem` list
checkIfOponentHit point list = point `elem` list 

getOponentsResult:: Move -> Status
getOponentsResult moves =
  if (checkIfOponentHit (getMoveCoord moves) shipPoints)
    then Hit
    else Miss

getPointResult :: String -> Either String Status
getPointResult [] = Left "getting result failed"
getPointResult json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Left "getting result failed"
    (Just moves) -> 
        Right (getOponentsResult moves) 

addMove:: Point -> Status -> Move -> Move
addMove point status allMoves = (MovePrev point status allMoves)

---------------------randomize
randomCoordinates :: IO [Point]
randomCoordinates = shuffleM possibleCoord

randomNextMove :: Move -> IO Point
randomNextMove moves = do
  i <- randomCoordinates
  let filtered = filter (`notElem` map getMoveCoord (listMoves [] moves)) i
  let coord = head filtered
  return coord

makeARandomMove :: String -> IO String
makeARandomMove json = do
  (list, _) <- liftEither pure (parseJList json)
  maybeMoves <-  liftEither pure (getAllMoves list)
  case maybeMoves of
    Nothing -> return "error"
    (Just moves) -> do
      scores <- liftEither pure $ getScore moves
      if(fst scores >= 20 || snd scores >=20)
        then return (showFinalMoves (addMove EmptyPoint (getOponentsResult moves) moves))
      else ( do
        correctMove <- randomNextMove moves
        return (showFinalMoves (addMove correctMove (getOponentsResult moves) moves)))

countHits acc (Hit:xs) = countHits (acc + 1) xs
countHits acc (Miss:xs) = countHits acc xs
countHits acc (Unknown:xs) = countHits acc xs
countHits acc [] = acc 

getScore :: Move -> Either String (Int, Int)
getScore moves = do
  let movesA = listMoves [] moves
  let movesB = listMoves' [] moves
  let statsA = map getMoveStatus movesA
  let statsB = map getMoveStatus movesB
  let hitsA = countHits 0 statsA
  let hitsB = countHits 0 statsB
  return (hitsA, hitsB)

main :: IO ()
main = do
  contents <- readFile "moves.txt"
  --result <- makeARandomMove contents
  movesStr <-  makeARandomMove contents
  moves <- liftEither pure $ parseMoves movesStr
  result <- liftEither pure $ getScore moves
  print $ result
  --print $ parseMoves contents
  --print $ getOponentsResult $ parseMoves contents
  --print $ getPointResult contents

