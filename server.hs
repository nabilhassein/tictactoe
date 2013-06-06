import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hPutStrLn, BufferMode(..))

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromInteger $ read $ head args
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ head args
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (player1, _, _) <- accept sock
  hSetBuffering player1 NoBuffering
  (player2, _, _) <- accept sock
  hSetBuffering player2 NoBuffering
  -- TODO: send actual type instead of string typing
  hPutStrLn player1 "X"
  hPutStrLn player2 "O"
  putStrLn "sent piece types"
