{-# LANGUAGE NoMonomorphismRestriction #-}

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import TicTacToe

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral $ read $ head args
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ show port
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
