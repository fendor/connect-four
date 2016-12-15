module Move (Move(..), Move, Pos, Row, Column) where

import           Player (Player)
import           Stone  (Stone)

type Pos = (Row, Column)
type Row = Int
type Column = Int

data Move = Move
  { position :: Pos
  , player   :: Player
  , stone    :: Stone
  } deriving (Eq, Show)

