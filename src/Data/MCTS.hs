{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Data.MCTS
    ( search
    -- , searchResults
    , GetMoves
    , MakeMove
    , IsWon
    , Seconds
    , SearchEnd(..)
    ) where

import qualified Data.Time.Clock as Time
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Random as Random
import qualified Data.Foldable as Fold
import qualified Data.Ord as Ord
import Data.Ratio ((%))

-- | From state s, construct a container t of moves of type e
type GetMoves s t e = s -> t e

-- | Given state s and move e, create the next state s
type MakeMove s e   = s -> e -> s

-- | Does the state s satisfy a termination critierion?
type IsWon s        = s -> SearchEnd

{-| Represents termination status for a current state.

NotFinished means the current state is not a terminal state.
PositiveOutcome is a successful terminal state (e.g. player won).
NegativeOutcome is a failed terminal state (e.g. player lost).
-}
data SearchEnd = NotFinished
               | PositiveOutcome
               | NegativeOutcome

type Seconds = Integer

data Search' s t e = Search {
                       getMoves :: GetMoves s t e
                     , makeMove :: MakeMove s e
                     , isWon    :: IsWon s
                     }

{-| Uses a variant of monte carlo tree search to find the best "move."

The penultimate parameter (Seconds) is a soft bound on
how much time can be used for the search.

The final parameter is the initial state s from which to start the search.

Imagine the following game: you have three cells where players alternate turns
placing a token. The cells are adjacent and the winner is the one who places
two of their tokens next to each other. Clearly, the dominant strategy
is for the first player to go in the middle every time. Here's how you
could model the search for the first player (X).

@
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

@
-}
search :: Traversable t => GetMoves s t e -> MakeMove s e -> IsWon s -> Seconds -> s -> IO e
search getMoves makeMove isWon totalSecs start = choose <$> searcher totalSecs start moves
  where moves  = getMoves start
        searcher = searchResults getMoves makeMove isWon
        choose = fst . Fold.maximumBy (Ord.comparing snd)

searchResults :: Traversable t => GetMoves s t e -> MakeMove s e -> IsWon s -> Seconds -> s -> t e -> IO (t (e, Rational))
searchResults getMoves makeMove isWon totalSecs start moves =
    let s       = Search getMoves makeMove isWon
        addTime = Time.addUTCTime (fromIntegral totalSecs)
    in do stopTime <- addTime <$> Time.getCurrentTime
          Async.forConcurrently moves $ worker s stopTime start

worker :: Traversable t => Search' s t e -> Time.UTCTime -> s -> e -> IO (e, Rational)
worker s@Search{..} stopTime oldStart edge =
    let start = makeMove oldStart edge

        go !numWins !numTrials = do now <- Time.getCurrentTime
                                    if now > stopTime
                                    then return (edge, numWins % numTrials)
                                    else do won <- finishTree s start
                                            if won
                                            then go (numWins+1) (numTrials+1)
                                            else go numWins     (numTrials+1)
    in go 0 1


finishTree :: (Traversable t, Random.MonadRandom m) => Search' s t e -> s -> m Bool
finishTree s@Search{..} start =
    case isWon start of
        PositiveOutcome -> return True
        NegativeOutcome -> return False
        NotFinished     -> do randMove <- uniformT $ getMoves start
                              finishTree s $ makeMove start randMove

uniformT :: (Traversable t, Random.MonadRandom m) => t a -> m a
uniformT = Random.uniform . Fold.toList
