module Main where

import           Data.Maybe (fromJust, isNothing)
import           Game


main :: IO ()
main = do
  let field = newConnectFour 6 8
  let game = zip [1, 1, 2, 2, 3, 3] $ cycle [Hannes, Jana]
  let Just played = foldl (\field' (m, p) -> field' >>= \f -> throwIn f p m ) (Just field) game
  let play field = foldl (\field' (m, p) -> field' >>= \f -> throwIn f p m ) (Just field)

  putStrLn . prettyPrint . fromJust $ play field game
