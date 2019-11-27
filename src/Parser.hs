module Parser where
  
import System.IO
import System.Environment
import Helper

parseMoves:: String -> Either String Move
parseMoves json = do 
  (list, _) <- parseJList json
  maybeMoves <- getAllMoves list
  case maybeMoves of
    Nothing -> Left "parsing moves failed"
    (Just moves) -> Right moves

parseJString :: String -> Either String (String, String)
parseJString ('"':str) = parseJString' "" str
parseJString' acc ('"':str) = Right (acc, str)
parseJString' acc (c:str) = parseJString' (acc ++ [c]) str
parseJString' _ _ = Left "string does not end with quotation marks"

parseJList :: String -> Either String (JValue, String)
parseJList ('[':str) = parseJList' [] str
parseJList' :: [JValue] -> String -> Either String (JValue, String)
parseJList' acc a@('"':_) =
    case parseJString a of
        Left e -> Left e
        Right (res, str) -> parseJList' (acc ++ [JString res]) str
parseJList' acc (']':str) = Right (JList acc, str)
parseJList' acc [] = Right (JList acc, [])
parseJList' acc (',':str) = parseJList' acc str
parseJList' acc ('[':']':str) = Left "game over"
parseJList' acc a@('[':str) =
    case parseJList' [] str of
        Left e -> Left e
        Right (res, str) -> parseJList' (acc ++ [res]) str
parseJList' _ str = Left str

getCoord :: JValue -> Either String Point
getCoord (JList (JString "coord":JList []:rest)) = Left "game is over"
getCoord (JList (JString "coord":(JList (JString a:JString b:[])):rest)) = Right (readTuple (a,b))
getCoord (JList (_:rest)) = getCoord (JList rest)
getCoord _ = Left "there are no coordinates in this list"

getMoveCoord :: Move -> Point
getMoveCoord (MoveFirst point) = point
getMoveCoord (MovePrev point status move) = point
getMoveCoord (MoveWithStatus point status) = point

getMoveStatus :: Move -> Status
getMoveStatus (MoveFirst point) = Unknown
getMoveStatus (MovePrev point status move) = status
getMoveStatus (MoveWithStatus point status) = status


getResult :: JValue -> Either String Status
getResult (JList (JString "result":JString "HIT":rest)) = Right Hit
getResult (JList (JString "result":JString "MISS":rest)) = Right Miss
getResult (JList (_:rest)) = getResult (JList rest)
getResult _ = Left "there are no results in this list"

getPrev :: JValue -> Either String JValue
getPrev (JList (JString "prev":b:rest)) = Right b
getPrev (JList (_:rest)) = getPrev (JList rest)
getPrev _ = Left "there is no previous in this list"

getAllMoves :: JValue -> Either String (Maybe Move)
getFirstMove :: JValue -> Either String Move
getAllMoves a = case getPrev a of
  Left e -> case getFirstMove a of
    Left e -> Right Nothing
    Right firstMove -> Right (Just firstMove)
  Right prev -> 
    case getCoord a of
      Left e -> Right Nothing
      Right coord -> 
        case getResult a of
          Left e -> Left e
          Right result -> 
            case getAllMoves prev of
              Left e -> case getFirstMove a of
                Left e -> Right Nothing
                Right firstMove -> Right (Just (MovePrev coord result firstMove))
              Right Nothing -> Right Nothing
              Right (Just move) -> Right (Just (MovePrev coord result move))

getFirstMove a = 
  case getCoord a of
    Left e -> Left e
    Right coord -> Right (MoveFirst coord)
    
listMoves acc (MovePrev  _ _ move) = listMoves' acc move
listMoves acc (MoveFirst _) = acc
listMoves acc (NotMove e) = []
listMoves' acc (MovePrev xy status move) = listMoves (acc ++ [MoveWithStatus xy status]) move
listMoves' acc (MoveFirst xy) = acc ++ [MoveFirst xy]


