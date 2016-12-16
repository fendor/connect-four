module Game (
  newConnectFour
  , newGame
  , ConnectFour
  , Game
  , throwIn
  , newMove
  , prettyPrint
  , hasWon
  , undoMove
) where

import           Control.Applicative (ZipList (..), getZipList)
import           Data.Matrix
import           Data.Maybe          (fromJust, isNothing)
import           Data.Text           (pack, replace, unpack)
import           Move
import           Player
import           Stone

data Game = Game
  { current :: ConnectFour
  , history :: [ConnectFour]
  }

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

newGame :: ConnectFour -> Game
newGame c = Game { current = c, history = [] }

undoMove :: Game -> Maybe Game
undoMove game
  | null $ history game = Nothing
  | otherwise = Just game { current = head $ history game, history = tail $ history game  }

playerToStone :: Player -> Stone
playerToStone Jana = Yellow -- important, famous, stubborn player
playerToStone Hannes = Red

stoneToPlayer :: Stone -> Maybe Player
stoneToPlayer Red = Just Hannes
stoneToPlayer Yellow = Just Jana
stoneToPlayer Empty = Nothing

inBounds :: ConnectFour -> Pos -> Bool
inBounds c (a, b) = (0 < a && a <= x) && (0 < b && b <= y)
  where
    g = gamefield c
    x = nrows g
    y = ncols g

throwIn :: Game -> Move -> Game
throwIn connect m = newConnect
  where
    previousGame = current connect
    s = stone m
    pos = position m
    newField = setElem s pos (gamefield previousGame)
    newConnect = connect
      { current = previousGame
          { gamefield = newField
          , lastMove = Just m
          }
      , history =  previousGame : history connect
      }

newMove :: Game -> Player -> Column -> Maybe Move
newMove game p column = update (highest, column) >>= move'
  where
    field = gamefield $ current game
    highest = nrows field

    move' pos = Just
      Move
        { position  = pos
        , player    = p
        , stone     = playerToStone p
        }

    update :: Pos -> Maybe Pos
    update pos@(y, x)
      | not $ inBounds (current game) pos = Nothing
      | Empty == field ! pos = Just pos
      | otherwise = update (y-1, x)



hasWon :: Game -> Maybe Player
hasWon c
    | isNothing move = Nothing
    | isNothing p = Nothing
    | hasSomeoneWon = p
    | otherwise = Nothing
    where
        connect = current c
        field = gamefield connect
        move = lastMove connect
        pos = position $ fromJust move
        p = stoneToPlayer (field ! pos)
        s = playerToStone $ fromJust p
        candidates = getAllCandidates pos

        hasSomeoneWon :: Bool
        hasSomeoneWon =
          any (all (==s))
          . map (map  (field !))
          $ filter (all (inBounds connect)) candidates
--
getDiagonalNeighbours :: Pos -> [[Pos]]
getDiagonalNeighbours (x,y) =
  getZipList (zip <$> ZipList candidatesX <*> ZipList candidatesY)
  ++ getZipList
    (zip <$> ZipList candidatesX <*> ZipList (reverse $ map reverse candidatesY))
  where
    candidatesX = getDirectCandidates 3 x
    candidatesY = getDirectCandidates 3 y

getHorizontalCandidates :: Pos -> [[Pos]]
getHorizontalCandidates (x, y) =
    map (\coords -> zip coords (repeat y)) (getDirectCandidates 3 x)

getVerticalCandidates :: Pos -> [[Pos]]
getVerticalCandidates (x, y) =
    map (zip (repeat x)) (getDirectCandidates 3 y)

getDirectCandidates :: Int -> Int -> [[Int]]
getDirectCandidates interval n = [[a..a+interval] | a <- [n-interval..n]]

getAllCandidates :: Pos -> [[Pos]]
getAllCandidates pos =
    getVerticalCandidates pos
    ++ getHorizontalCandidates pos
    ++ getDiagonalNeighbours pos


prettyPrint :: Game -> String
prettyPrint = unpack . replace (pack "Empty") (pack "    -") . pack . prettyMatrix . gamefield . current
