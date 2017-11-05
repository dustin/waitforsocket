module Main where

import Waitforsocket

import System.IO (hClose)
import Network (connectTo)
import Network.HTTP (simpleHTTP, getRequest)
import Control.Monad (when)
import Control.Exception.Safe (catchIO)
import Control.Concurrent.Async
import Control.Concurrent.Timeout

import System.Log.Logger (rootLoggerName, updateGlobalLogger,
                          Priority(INFO), setLevel, infoM)

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options { optAbsTimeout :: Integer
                       , optRequired :: Integer
                       , optTimeout :: Integer
                       , targets :: [Target]
                       }

loginfo :: String -> IO ()
loginfo = infoM rootLoggerName

options :: Parser Options
options = Options
  <$> option auto (long "absTimeout" <> showDefault <> value 0 <> help "absolute timeout")
  <*> option auto (long "required" <> showDefault <> value 0 <> help "how many connections required (0 = all)")
  <*> option auto (long "timeout" <> showDefault <> value 5000 <> help "connect/retry timeout (ms)")
  <*> some (argument parseTarget (metavar "targets..."))

attemptIO :: Target -> IO Bool -> IO Bool
attemptIO t f = do
  loginfo $ "Connecting to " ++ show t
  catchIO f (\e -> loginfo ("Error connecting to " ++ show t ++ ": " ++ show e) >> return False)

tryConnect :: Target -> IO Bool
tryConnect targ@(TCP h p) = do
  attemptIO targ $ connectTo h p >>= hClose >> pure True
tryConnect targ@(HTTP u) = do
  attemptIO targ $ simpleHTTP (getRequest u) >> pure True

connect :: Target -> IO Bool
connect targ = do
  (t,b) <- timedFun (tryConnect targ)
  when b $ loginfo $ "Connected to " ++ show targ ++ " in " ++ show t
  return b

waitforsocket :: Options -> IO ()
waitforsocket (Options _ req to things) = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  let lth = fromIntegral $ length things
  let todo = if req == 0 || req > lth then lth else req
  let asyncs = mapM (async . waitfor) things
  _ <- waitN todo asyncs
  return ()

  where millis = (* 1000)
        waitfor :: Target -> IO (Maybe Bool)
        waitfor u = while $ timeout (millis to) (connect u)

main :: IO ()
main = waitforsocket =<< execParser opts
  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Wait for network things.")
