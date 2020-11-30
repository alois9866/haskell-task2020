module Main where

import System.Environment

import Lib
import Text.Read (readMaybe)

defaultPort :: Int
defaultPort = 8080

main :: IO ()
main = do
  portStr <- lookupEnv "TASK_PORT"
  let port = portStr >>= readMaybe
  maybe (startApp defaultPort) startApp port
