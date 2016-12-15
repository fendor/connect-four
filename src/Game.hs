module Game (
  newConnectFour
  , ConnectFour
  , throwIn
  , newMove
  , prettyPrint
  , hasWon
) where

import           Control.Applicative (ZipList (..), getZipList)
import           Data.Matrix
import           Data.Maybe          (fromJust, isNothing)
import           Move
import           Player
import           Stone

data ConnectFour = Four
  { gamefield :: Matrix Stone
  , lastMove  :: Maybe Move
  } deriving (Eq, Show)

newConnectFour :: Int -> Int -> ConnectFour
newConnectFour rows columns =
    Four
      { gamefield = fromList rows columns (repeat Empty)
      , lastMove = Nothing
      }

playerToStone :: Player -> Stone
playerToStone Jana = Yellow -- important, famous, stubborn player
playerToStone Hannes = Red

stoneToPlayer :: Stone -> Maybe Player
stoneToPlayer Red = Just Hannes
stoneToPlayer Yellow = Just Jana
stoneToPlayer Empty = Nothing

inBounds :: ConnectFour -> Pos -> Bool
inBounds c (a, b) = (0 < a && a < x) && (0 < b && b < y)
  where
    g = gamefield c
    x = nrows g
    y = ncols g

throwIn :: ConnectFour -> Move -> ConnectFour
throwIn connect m = connect

newMove :: ConnectFour -> Player -> Column -> Maybe Move
newMove four p column = update (highest, column) >>= move'
  where
    field = gamefield four
    highest = nrows field

    move' pos = Just
      Move
        { position  = pos
        , player    = p
        , stone     = playerToStone p
        }

    update :: Pos -> Maybe Pos
    update pos@(x,y)
      | not $ inBounds four pos = Nothing
      | Empty == field ! pos = Just pos
      | otherwise = update (y-1, x)


prettyPrint :: ConnectFour -> String
prettyPrint = prettyMatrix . gamefield


hasWon :: ConnectFour -> Maybe Player
hasWon c
    | isNothing move = Nothing
    | isNothing p = Nothing
    | hasSomeoneWon = p
    | otherwise = Nothing
    where
        field = gamefield c
        move = lastMove c
        pos = position $ fromJust move
        p = stoneToPlayer (field ! pos)
        s = playerToStone $ fromJust p
        candidates = getAllNeighbours pos

        hasSomeoneWon :: Bool
        hasSomeoneWon =
          any (all (==s))
          . map (map  (field !))
          $ filter (all (inBounds c)) candidates
--
getDiagonalNeighbours :: Pos -> [[Pos]]
getDiagonalNeighbours (x,y) =
  getZipList (zip <$> ZipList candidatesX <*> ZipList candidatesY)
  ++ getZipList
    (zip <$> ZipList candidatesX <*> ZipList (reverse $ map reverse candidatesY))
  where
    candidatesX = getDirectNeighbours 3 x
    candidatesY = getDirectNeighbours 3 y

getHorizontalNeighbours :: Pos -> [[Pos]]
getHorizontalNeighbours (x, y) =
    map (\coords -> zip coords (repeat y)) (getDirectNeighbours 3 x)

getVerticalNeighbours :: Pos -> [[Pos]]
getVerticalNeighbours (x, y) =
    map (zip (repeat x)) (getDirectNeighbours 3 y)

getDirectNeighbours :: Int -> Int -> [[Int]]
getDirectNeighbours interval n = [[a..a+interval] | a <- [n-interval..n]]

getAllNeighbours :: Pos -> [[Pos]]
getAllNeighbours pos =
    getVerticalNeighbours pos
    ++ getHorizontalNeighbours pos
    ++ getDiagonalNeighbours pos
