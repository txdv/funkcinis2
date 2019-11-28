module Game where

import Parser
import Helper
import Encode
import Remote
import System.Environment
import System.Random.Shuffle

getMovePoint:: [Point] -> Move -> Either String (Maybe Point)
getMovePoint _ NotMove = Right Nothing
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

getMovesFromJson :: String -> IO Move
getMovesFromJson json = do
  (list, _) <- liftEither pure (parseJList json)
  maybeMoves <-  liftEither pure (getAllMoves list)
  case maybeMoves of
    Nothing -> return NotMove
    (Just moves) -> do
      return moves

makeFirstMove :: IO String
makeFirstMove  = do
  i <- randomCoordinates
  let coord = head i
  return $ "[\"coord\","++ showPointStr (showPoint coord) ++ "]"

makeAMove :: Point -> String -> IO String
makeAMove point json = do
  (list, _) <- liftEither pure (parseJList json)
  maybeMoves <-  liftEither pure (getAllMoves list)
  case maybeMoves of
    Nothing -> return "error"
    (Just moves) -> do
      scores <- liftEither pure $ getScore moves
      if(fst scores >= 20 || snd scores >=20)
        then return (showFinalMoves (addMove EmptyPoint (getOponentsResult moves) moves))
      else ( do
        --correctMove <- randomNextMove moves
        return (showFinalMoves (addMove point (getOponentsResult moves) moves)))

getScoreFromJson :: String -> IO (Int, Int)
getScoreFromJson json = do
  moves <- getMovesFromJson json
  scores <- liftEither pure (getScore moves)
  return scores

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

step:: String -> String -> IO ()
step player gameId = do
  (responseCode, contents) <- getMessage player gameId
  moves <- getMovesFromJson contents
  point <- randomNextMove moves
  movesStr <- makeAMove point contents
  score <- getScoreFromJson contents
  let (myScore, enemyScore) = score
  if (myScore == 20) then do
    putStrLn("Game Over")
    sendMessage movesStr player gameId
    return $ ()
  else do
    putStrLn $ "Made a move to " ++ showPointStr (showPoint point) ++ " and score is " ++ show score
    sendMessage movesStr player gameId
    step player gameId

{-battleship :: String -> String -> IO ()
battleship player gameId = do
  --(responseCode, contents) <- getMessage player gameId
  if(player == "A")
    then do
      firstMove <- makeFirstMove
      _ <- sendMessage firstMove player gameId
      step player gameId
      return $ ()
    else do
      step player gameId-}

main :: IO ()
main = do
  args <- getArgs
  if length args == 2 then do
    let [player, gameId] = args
    _ <- if player == "A" then do
      firstMove <- makeFirstMove
      _ <- sendMessage firstMove player gameId
      step player gameId
    else do
      step player gameId
    return $ ()
  else do
    putStrLn ("Yo")
