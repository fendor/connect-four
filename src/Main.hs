module Main where

import           Data.Maybe (isJust)
import           Game
import           Move       (Move)
import           Player
import           Text.Read  (readMaybe)

main :: IO ()
main = do
  let game = newGame (newConnectFour 6 7)
  turn game Hannes
  return ()

turn :: Game -> Player -> IO ()
turn gamefield player = do
    putStrLn $ prettyPrint gamefield
    putStrLn "Enter a number: "
    line <- getLine

    let game = parseInput line

    case game of
      Just g ->
        if isJust $ hasWon g then
          putStrLn (prettyPrint g) >>
          putStrLn ("Player " ++ show player ++ " has won!")
        else
          turn g (otherPlayer player)
      Nothing -> putStrLn "Enter a correct number. (1-7)" >> turn gamefield player

    return ()

    where
      parseInput l = case l of
        "u" -> undo gamefield
        number -> readMaybe number >>= playMove gamefield player


playTurn :: Game -> Move -> Maybe Game
playTurn c m = Just (throwIn c m)

otherPlayer :: Player -> Player
otherPlayer Hannes = Jana
otherPlayer Jana = Hannes

undo :: Game -> Maybe Game
undo = undoMove

playMove :: Game -> Player -> Int ->  Maybe Game
playMove g p num = newMove g p num >>= playTurn g

