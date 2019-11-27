module Encode where
  
import System.IO
import System.Environment

data Point = Point (Int, Int) | PointStatus (Int, Int) Status
  deriving (Show, Eq)


data Status = Hit | Miss | Unknown
  deriving Eq
  
instance Show Status where
  show Hit = "HIT"
  show Miss = "MISS"

data JValue = JString String | JList [JValue]
  deriving Show

data Move = MoveFirst Point | MovePrev Point Status Move | NotMove String
  deriving Show

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
parseJList' acc a@('[':str) =
    case parseJList' [] str of
        Left e -> Left e
        Right (res, str) -> parseJList' (acc ++ [res]) str
parseJList' _ str = Left "list is not in correct format"

readTuple ::  (String , String) -> Point
readTuple (x,y) 
  | x == "A" = Point (1, read y :: Int)
  | x == "B" = Point (2, read y :: Int)
  | x == "C" = Point (3, read y :: Int)
  | x == "D" = Point (4, read y :: Int)
  | x == "E" = Point (5, read y :: Int)
  | x == "F" = Point (6, read y :: Int)
  | x == "G" = Point (7, read y :: Int)
  | x == "H" = Point (8, read y :: Int)
  | x == "I" = Point (9, read y :: Int)
  | x == "J" = Point (10, read y :: Int)
  | otherwise = Point (0,0)
  
showPoint ::  Point  -> [String]
showPoint (Point (x,y)) 
    | x == 1 = ["A", show y]
    | x == 2 = ["B", show y]
    | x == 3 = ["C", show y]
    | x == 4 = ["D", show y]
    | x == 5 = ["E", show y]
    | x == 6 = ["F", show y]
    | x == 7 = ["G", show y]
    | x == 8 = ["H", show y]
    | x == 9 = ["I", show y]
    | x == 10 = ["J", show y]
    | otherwise = [""]

getCoord :: JValue -> Either String Point
getCoord (JList (JString "coord":JList []:rest)) = Left "game is over"
getCoord (JList (JString "coord":(JList (JString a:JString b:[])):rest)) = Right (readTuple (a,b))
getCoord (JList (_:rest)) = getCoord (JList rest)
getCoord _ = Left "there are no coordinates in this list"

getMoveCoord :: Move -> Point
getMoveCoord (MoveFirst point) = point
getMoveCoord (MovePrev point status move) = point


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
listMoves' acc (MovePrev xy _ move) = listMoves (acc ++ [MoveFirst xy]) move
listMoves' acc (MoveFirst xy) = acc ++ [MoveFirst xy]

getWhole :: Either String (JValue, String) -> JValue
getWhole (Right ((JList a), rest)) = (JList a)
getWhole (Left e) = (JString e)

getWhole2 :: (JValue, String) -> JValue
getWhole2 (JList a, _) = (JList a)

listOfCoordX = [1..10]
listOfCoordY = [1..10]
possibleCoord = [Point (x,y) | x <- listOfCoordX, y <- listOfCoordY]  


