module Extra where
{-makeAMove:: [Point] -> Move -> Either String (Maybe [String])
makeAMove _ (NotMove e) = Right Nothing
makeAMove (coord:rest) allMoves = if 
  (checkIfMoveExists (show (MoveFirst coord)) (map show (listMoves [] allMoves))) then (makeAMove rest allMoves) else (Right (Just (showPoint coord))) 
makeAMove _ _ = Right Nothing-}

{-move :: String -> Either String (Maybe [String])
move json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Right Nothing
    (Just moves) -> 
        case makeAMove possibleCoord moves of
        Right Nothing -> Right Nothing
        Right correctMove -> Right correctMove-}
        
{-getFinalMoves :: String -> Either String String
getFinalMoves  json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Left "error"
    (Just moves) -> 
      case getMovePoint possibleCoord moves of
        Right Nothing -> Left "error"
        Right (Just correctMove) -> Right (showFinalMoves (addMove correctMove (getOponentsResult moves) moves))-}   

{-getPlayerStatus acc (MovePrev  _ _ move) = getPlayerStatus' acc move
getPlayerStatus acc (MoveFirst _) = acc
getPlayerStatus acc (NotMove e) = acc
getPlayerStatus' acc (MovePrev xy status move) = getPlayerStatus (acc ++ [status]) move
getPlayerStatus' acc (MoveFirst xy) = acc-}

--contents <- readFile "moves.txt"
--result <- makeARandomMove contents
--movesStr <-  makeARandomMove contents
--moves <- liftEither pure $ parseMoves movesStr
--result <- liftEither pure $ getScore moves
--print $ movesStr
--print $ parseMoves contents
--print $ getOponentsResult $ parseMoves contents
--print $ getPointResult contents


{--runGame:: Player -> String -> IO ()
runGame player gameId = runGameStep game
where game = Game { player = player, gameId = gameId, step = 0}
--}
--toTuple :: Moves
{-getMoves :: IO ()
getMoves = do
contents <- readFile "moves.txt"
--parsedContents <- parseJList contents
--parsedContents2 <- getWhole2 parsedContents
let a = move contents 
print $ a-}


{-getWhole :: Either String (JValue, String) -> JValue
getWhole (Right ((JList a), rest)) = (JList a)
getWhole (Left e) = (JString e)

getWhole2 :: (JValue, String) -> JValue
getWhole2 (JList a, _) = (JList a)-}
     