import Types
import Network (withSocketsDo, connectTo, PortID(..))
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import System.Environment (getArgs)

-- don't read this function as code; instead look at it as a picture
prettyPrintBoard :: String -> String
prettyPrintBoard [tl, tm, tr,
                  ml, mm, mr,
                  bl, bm, br] =
  "\n   1   2   3 "                                 ++
  "\n 1 " ++ [tl] ++ " | " ++ [tm] ++ " | " ++ [tr] ++
  "\n  ---|---|---"                                  ++
  "\n 2 " ++ [ml] ++ " | " ++ [mm] ++ " | " ++ [mr] ++
  "\n  ---|---|---"                                  ++
  "\n 3 " ++ [bl] ++ " | " ++ [bm] ++ " | " ++ [br]

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = fromInteger $ read $ head args
  hOut <- connectTo "0.0.0.0" $ PortNumber port
  hSetBuffering hOut NoBuffering
  piece <- getPiece hOut -- from the server
  case piece of
    X -> move hOut X
    O -> wait hOut O

getPiece :: Handle -> IO Piece
getPiece hOut = do
  str <- hGetLine hOut
  let piece = read str :: Piece
  putStrLn $ "You are playing piece " ++ show piece
  return piece

wait :: Handle -> Piece -> IO ()
wait h piece = do
  response <- hGetLine h
  case response of
    "Draw"             -> print Draw
    "Loss"             -> print Loss
    "OtherPlayerError" -> print OtherPlayerError >> wait h piece
    board              -> putStrLn (prettyPrintBoard board) >> move h piece

move :: Handle -> Piece -> IO ()
move h piece = do
  putStrLn $ "\nPlayer " ++ show piece ++ ", move in the form (x, y)."
  input <- getLine
  response <- request h input
  case response of
    First gameOver   -> print gameOver
    Second yourError -> print yourError                      >> move h piece
    Third newBoard   -> putStrLn (prettyPrintBoard newBoard) >> wait h piece

request :: Handle -> String -> IO YourTurnResponse
request h req = do
  hPutStrLn h req
  response <- hGetLine h
  return $ case response of
    "Win"            -> First Win
    "Draw"           -> First Draw
    "NonEmptySquare" -> Second NonEmptySquare
    "OutOfBounds"    -> Second OutOfBounds
    "BadInput"       -> Second BadInput
    board            -> Third board
