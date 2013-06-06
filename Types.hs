module Types where

import qualified Data.Map as Map

-- directly on the board
data Piece = X | O deriving (Eq, Show, Read)

type Position = (Int, Int)

type Board = Map.Map Position Piece

showBoard :: Board -> String
showBoard board = let showCell :: Maybe Piece -> String
                      showCell (Just X) = "X"
                      showCell (Just O) = "O"
                      showCell Nothing  = " "

                      spot :: Position -> String
                      spot = showCell . flip Map.lookup board
  in "\n   1   2   3 "                                                      ++
     "\n 1 " ++ spot (1, 1) ++ " | " ++ spot (2, 1) ++ " | " ++ spot (3, 1) ++
     "\n  ---|---|---"                                                      ++
     "\n 2 " ++ spot (1, 2) ++ " | " ++ spot (2, 2) ++ " | " ++ spot (3, 2) ++
     "\n  ---|---|---"                                                      ++
     "\n 3 " ++ spot (1, 3) ++ " | " ++ spot (2, 3) ++ " | " ++ spot (3, 3)

-- client-server logic
data GameOver =  Win | Loss | Draw
instance Show GameOver where
  show Win  = "You won."
  show Loss = "You lost."
  show Draw = "Cat's game."

data YourError = NonEmptySquare | OutOfBounds | BadInput
instance Show YourError where
  show NonEmptySquare = "That square is taken. Choose another move."
  show OutOfBounds    = "Only select coordinates from 1 to 3."
  show BadInput       = "Couldn't parse that input. Enter your move as (x, y)."

data OtherPlayerError = OtherPlayerError
instance Show OtherPlayerError where
  show OtherPlayerError = "The other player made a mistake. Keep waiting."

data OneOf a b c = First a | Second b | Third c deriving Show

type BoardDisplay = String

type YourTurnResponse = OneOf GameOver YourError BoardDisplay

type OtherTurnResponse = Either GameOver BoardDisplay
