module Game (Player(Hannes, Jana)
    , Stone(..)
    , newConnectFour
    , player
    , throwIn
    , prettyPrint
    ) where

import           Data.Matrix
import           Data.Maybe  (fromJust, isNothing)

data Player = Hannes | Jana deriving (Show, Eq, Ord, Read, Enum)
data Stone = Red | Yellow | Empty deriving (Show, Eq, Ord, Read, Enum)
type Pos = (Row, Column)
type Row = Int
type Column = Int

data ConnectFour = Four
  { gamefield :: Matrix Stone
  , lastMove  :: Maybe Pos
  } deriving (Eq, Show)

newConnectFour :: Int -> Int -> ConnectFour
newConnectFour rows columns =
    Four
      { gamefield = fromList rows columns (repeat Empty)
      , lastMove = Nothing
      }

player :: Player -> Stone
player Jana = Yellow -- important, famous, stubborn player
player Hannes = Red

stone :: Stone -> Maybe Player
stone Red = Just Hannes
stone Yellow = Just Jana
stone Empty = Nothing

hasWon :: ConnectFour -> Maybe Player
hasWon c
    | isNothing move = Nothing
    | isNothing player = Nothing
    | otherwise = checkVictory
    where
        field = gamefield c
        move = lastMove c
        pos = fromJust move
        player = stone (field ! pos)
        checkVictory = Nothing


throwIn :: ConnectFour -> Player -> Column -> Maybe ConnectFour
throwIn four p column = update highest
  where
    field = gamefield four
    highest = nrows field
    updatedField row = setElem (player p) (row, column) field
    newFour row = Four {gamefield = updatedField row, lastMove = Just (row, column)}

    update :: Row -> Maybe ConnectFour
    update n
      | n == 0 = Nothing
      | Empty == field ! (n, column) = Just (newFour n)
      | otherwise = update (n-1)

prettyPrint :: ConnectFour -> String
prettyPrint = prettyMatrix . gamefield
