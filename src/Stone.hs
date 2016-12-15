module Stone (Stone, Stone(..))where

data Stone = Red | Yellow | Empty deriving (Show, Eq, Ord, Read, Enum)
