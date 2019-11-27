module Helper where

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

data Point = Point (Int, Int) | PointStatus (Int, Int) Status | EmptyPoint
  deriving (Show, Eq)

data Status = Hit | Miss | Unknown
  deriving Eq
  
instance Show Status where
  show Hit = "HIT"
  show Miss = "MISS"

data JValue = JString String | JList [JValue]
  deriving Show

data Move = MoveFirst Point | MovePrev Point Status Move | MoveWithStatus Point Status | NotMove String
  deriving Show

listOfCoordX = [1..10]
listOfCoordY = [1..10]
possibleCoord = [Point (x,y) | x <- listOfCoordX, y <- listOfCoordY]  

liftEither :: Monad m => (t -> m a) -> Either String t -> m a
liftEither = either fail
  
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
    | otherwise = []