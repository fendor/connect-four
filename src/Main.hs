module Main where

import           Data.Maybe (isJust)
import           Game
import           Move       (Move)
import           Player
import           Text.Read  (readMaybe)

main :: IO ()
main = do
  let game = newConnectFour 6 7
  turn game Hannes
  return ()

turn :: ConnectFour -> Player -> IO ()
turn gamefield player = do
    putStrLn $ prettyPrint gamefield
    putStrLn "Enter a number: "
    maybeColumn <- fmap readMaybe getLine :: IO (Maybe Int)
    let game = maybeColumn >>= newMove gamefield player >>= playTurn gamefield

    case game of
      Just g ->
        if isJust $ hasWon g then
          putStrLn (prettyPrint g) >>
          putStrLn ("Player " ++ show player ++ " has won!")
        else
          turn g (otherPlayer player)
      Nothing -> putStrLn "Enter a correct number. (1-7)" >> turn gamefield player

    return ()

playTurn :: ConnectFour -> Move -> Maybe ConnectFour
playTurn c m = Just (throwIn c m)

otherPlayer :: Player -> Player
otherPlayer Hannes = Jana
otherPlayer Jana = Hannes
