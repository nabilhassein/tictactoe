module Types where

import qualified Data.Map as Map

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
  in concatMap spot [(1, 1), (2, 1), (3, 1),
                     (1, 2), (2, 2), (3, 2),
                     (1, 3), (2, 3), (3, 3)]

data GameOver =  Win | Loss | Draw deriving (Show, Read)

data YourError = NonEmptySquare | OutOfBounds | BadInput deriving (Show, Read)

data OtherPlayerError = OtherPlayerError deriving Read
instance Show OtherPlayerError where
  show OtherPlayerError = "The other player made a mistake. Keep waiting."

data OneOf a b c = First a | Second b | Third c deriving Show

type BoardDisplay = String
type YourTurnResponse = OneOf GameOver YourError BoardDisplay
type OtherTurnResponse = Either GameOver BoardDisplay
