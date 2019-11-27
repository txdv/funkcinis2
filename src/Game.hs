module Game where
  
import Encode
import System.Random.Shuffle
--import Map
--import Remote
  
data Player = PlayerA | PlayerB deriving (Eq)
data Game = Game {
  player :: Player,
  gameId :: String,
  --map :: Map,
  step :: Int
}

instance Show Player where
  show PlayerA = "A"
  show PlayerB = "B"
  
availableHits = 20  
mapSize = 10

shipPoints = [Point (1, 9), Point (2, 2), Point (3, 6), Point (3, 7), Point (3, 8), Point (4, 1), Point (4, 3), Point (5, 3), Point (6, 10), Point (7, 2), Point (7, 7), Point (7, 8), Point (7, 10), Point (8, 2), Point (8, 10), Point (9, 2), Point (9, 5), Point (10, 2), Point (10, 5), Point (10, 9)]

makeAMove:: [Point] -> Move -> Either String (Maybe [String])
makeAMove _ (NotMove e) = Right Nothing
makeAMove (coord:rest) allMoves = if 
  (checkIfMoveExists (show (MoveFirst coord)) (map show (listMoves [] allMoves))) then (makeAMove rest allMoves) else (Right (Just (showPoint coord))) 
makeAMove _ _ = Right Nothing

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

move :: String -> Either String (Maybe [String])
move json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Right Nothing
    (Just moves) -> 
        case makeAMove possibleCoord moves of
        Right Nothing -> Right Nothing
        Right correctMove -> Right correctMove
        
parseMoves:: String -> Either String Move
parseMoves json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Left "error"
    (Just moves) -> Right moves

getPointResult :: String -> Either String Status
getPointResult [] = Left "whatever"
getPointResult json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Left "error"
    (Just moves) -> 
        Right (getOponentsResult moves) 
        
showPointStr:: [String] -> String
showPointStr (x:y:[]) = "[\"" ++ x ++ "\",\"" ++ y ++ "\"]"  

showFinalMoves:: Move -> String
showFinalMoves (MoveFirst point) = "[\"coord\",\"" ++ showPointStr (showPoint point)  ++ "]" 
showFinalMoves current@(MovePrev point status move) = "[\"coord\"," ++ showPointStr (showPoint point) ++ "]," ++ "\"result\",\"" ++ show status ++ "\",\"prev\"," ++ showFinalMoves move ++ "]"

getFinalMoves :: String -> Either String String
getFinalMoves  json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Left "error"
    (Just moves) -> 
      case getMovePoint possibleCoord moves of
        Right Nothing -> Left "error"
        Right (Just correctMove) -> Right (showFinalMoves (addMove correctMove (getOponentsResult moves) moves))

addMove:: Point -> Status -> Move -> Move
addMove point status allMoves = (MovePrev point status allMoves)

---------------------randomize
liftEither :: Monad m => (t -> m a) -> Either String t -> m a
liftEither = either fail

randomCoordinates :: IO [Point]
randomCoordinates = shuffleM possibleCoord

randomNextMove :: Move -> IO Point
randomNextMove moves = do
  i <- randomCoordinates
  let filtered = filter (`notElem` map getMoveCoord (listMoves [] moves)) i
  let coord = head filtered
  --print coord
  return coord

makeARandomMove :: String -> IO String
makeARandomMove json = do
  (list, _) <- liftEither pure (parseJList json)
  maybeMoves <-  liftEither pure (getAllMoves list)
  case maybeMoves of
    Nothing -> return "error"
    (Just moves) -> do
      correctMove <- randomNextMove moves
      return (showFinalMoves (addMove correctMove (getOponentsResult moves) moves))

--getPlayerStatus:: Move -> [Status]
getPlayerStatus acc (MovePrev  _ _ move) = getPlayerStatus' acc move
getPlayerStatus acc (MoveFirst _) = acc
getPlayerStatus acc (NotMove e) = acc
getPlayerStatus' acc (MovePrev xy status move) = getPlayerStatus (acc ++ [status]) move
getPlayerStatus' acc (MoveFirst xy) = acc

-- countHits:: [Status] -> Int
countHits acc (Hit:xs) = countHits (acc + 1) xs
countHits acc (Miss:xs) = countHits acc xs
countHits acc [] = acc 

score :: Move -> Either String (Int, Int)
score moves = do
  let statsA = getPlayerStatus [] moves
  let statsB = getPlayerStatus' [] moves
  let hitsA = countHits 0 statsA
  let hitsB = countHits 0 statsB
  return (hitsA, hitsB)
  
  
--main :: IO ()
main :: IO ()
main = do
  contents <- readFile "moves.txt"
  --result <- makeARandomMove contents
  movesStr <-  makeARandomMove contents
  moves <- liftEither pure $ parseMoves movesStr
  result <- liftEither pure $ score moves
  print $ result
  --print $ parseMoves contents
  --print $ getOponentsResult $ parseMoves contents
  --print $ getPointResult contents
        
{--runGame:: Player -> String -> IO ()
runGame player gameId = runGameStep game
  where game = Game { player = player, gameId = gameId, step = 0}
--}
--toTuple :: Moves
getMoves :: IO ()
getMoves = do
  contents <- readFile "moves.txt"
  --parsedContents <- parseJList contents
  --parsedContents2 <- getWhole2 parsedContents
  let a = move contents 
  print $ a
