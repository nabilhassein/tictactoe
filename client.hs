import Types
import qualified Data.Map as Map
import Network (withSocketsDo, connectTo, PortID(..))
import System.IO (hSetBuffering, hGetLine, hGetContents, hPutStrLn, BufferMode(..), Handle)
import System.Environment (getArgs)

emptyBoard :: Board
emptyBoard = Map.empty

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = fromInteger $ read $ head args
  hOut <- connectTo "0.0.0.0" $ PortNumber port
  hSetBuffering hOut NoBuffering
  piece <- getPiece hOut -- from the server
  case piece of
    X -> print emptyBoard >> loop hOut X
    O -> do
      putStrLn "X moves first."
      -- no way for game to be over after X has moved just once
      Right boardDisplay <- await hOut
      putStrLn "O's first await has returned"
      print boardDisplay
      putStrLn "board should have been printed"
      loop hOut O

loop :: Handle -> Piece -> IO ()
loop hOut piece = do
  putStrLn $ "\nPlayer " ++ show piece ++ ", move in the form (x, y)."
  move <- getLine
  response <- request hOut move
  case response of
    First gameOver       -> print gameOver
    Second yourError     -> print yourError >> loop hOut piece
    Third newBoard -> do
      putStrLn newBoard -- after your move
      nextResponse <- await hOut
      case nextResponse of
        Left result -> print result -- game over
        Right board -> print board >> loop hOut piece -- after their move

getPiece :: Handle -> IO Piece
getPiece hOut = do
  str <- hGetLine hOut
  let piece = read str :: Piece
  putStrLn $ "You are playing piece " ++ show piece
  return piece

request :: Handle -> String -> IO YourTurnResponse
request h req = do
  hPutStrLn h req
  response <- hGetContents h
  return $ case response of
    "Win"            -> First Win
    "Draw"           -> First Draw
    "NonEmptySquare" -> Second NonEmptySquare
    "OutOfBounds"    -> Second OutOfBounds
    "BadInput"       -> Second BadInput
    board            -> Third board

await :: Handle -> IO OtherTurnResponse
await h = do
  putStrLn "inside await"
  response <- hGetContents h
--  putStrLn $ "got response " ++ response
  case response of
    "OtherPlayerError" -> print OtherPlayerError >> await h
    "Loss"             -> return $ Left Loss
    "Draw"             -> return $ Left Draw
    board              -> putStrLn "got board" >> return (Right board)
