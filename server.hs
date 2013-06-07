import Types
import TicTacToe
import Safe (readMay)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hPutStrLn, hGetLine, BufferMode(..), Handle)
import Control.Monad (forever)
import Control.Concurrent (forkIO)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = case args of
          [] -> 443
          _ -> fromInteger $ read $ head args
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ head args
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = forever $ do
  (player1, _, _) <- accept sock
  (player2, _, _) <- accept sock
  hSetBuffering player1 NoBuffering >> hSetBuffering player2 NoBuffering
  hPutStrLn player1 "X"             >> hPutStrLn player2 "O"
  forkIO $ loop emptyBoard X player1 player2

loop :: Board -> Piece -> Handle -> Handle -> IO ()
loop board piece player1 player2 = do
  let (mover, waiter) = case piece of
        X -> (player1, player2)
        O -> (player2, player1)
  input <- hGetLine mover
  let move = readMay input :: Maybe Position
  case makeMove board piece move of
    Left err       -> hPutStrLn mover (show err :: String) >>
                      hPutStrLn waiter "OtherPlayerError"  >>
                      loop board piece player1 player2
    Right newBoard -> case gameOver newBoard of
      (True, Nothing) -> hPutStrLn player1 "Draw" >> hPutStrLn player2 "Draw"
      (True, Just X)  -> hPutStrLn player1 "Win"  >> hPutStrLn player2 "Loss"
      (True, Just O)  -> hPutStrLn player1 "Loss" >> hPutStrLn player2 "Win"
      (False, _)      -> hPutStrLn mover  (showBoard newBoard) >>
                         hPutStrLn waiter (showBoard newBoard) >>
                         loop newBoard (other piece) player1 player2
