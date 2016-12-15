module Player (Player, Player(..))where

data Player = Hannes | Jana deriving (Show, Eq, Ord, Read, Enum)
