module Main where

import qualified Data.MCTS as Search
import qualified Data.Maybe as Maybe

data PlayerToken = X | O deriving (Eq, Show)

nextPlayer X = O
nextPlayer O = X

type Board = (Maybe PlayerToken, Maybe PlayerToken, Maybe PlayerToken)

type SearchState = (PlayerToken, Board)
data SearchMove  = First | Second | Third deriving (Eq, Show)

getMoves (move, (x, y, z)) = Maybe.catMaybes [x', y', z']
  where x' = replace First  x
        y' = replace Second y
        z' = replace Third  z
        replace a Nothing = Just a
        replace _ _       = Nothing

makeMove (move, (x, y, z)) First  = (nextPlayer move, (Just move, y, z))
makeMove (move, (x, y, z)) Second = (nextPlayer move, (x, Just move, z))
makeMove (move, (x, y, z)) Third  = (nextPlayer move, (x, y, Just move))

isWon (_, (x, y, z)) =
    let xy = x == y && x /= Nothing
        yz = y == z && y /= Nothing

        won  = (xy || yz) && (y == Just X)
        done = all (/= Nothing) [x, y, z]

    in if won
       then Search.PositiveOutcome
       else if done
               then Search.NegativeOutcome
               else Search.NotFinished

threeSeconds = 3
initialState = (X, (Nothing, Nothing, Nothing))

main :: IO ()
main = print =<< Search.search getMoves makeMove isWon threeSeconds initialState
