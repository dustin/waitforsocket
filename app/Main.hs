{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Waitforsocket

import Data.Either (isLeft)
import System.IO (hClose)
import Network (connectTo)
import Network.HTTP.Conduit (simpleHttp, HttpException(..), HttpExceptionContent(..), responseStatus)
import Control.Monad (when, forever)
import Control.Exception.Safe (catches, Handler(..), SomeException)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race)
import Control.Concurrent.Timeout (timeout)
import System.Exit (die)

import System.Log.Logger (rootLoggerName, updateGlobalLogger,
                          Priority(INFO), setLevel, infoM)

import Options.Applicative (option, auto, long, showDefault, value, help, some, helper, fullDesc,
                            progDesc, argument, eitherReader, metavar, execParser, info,
                            (<**>), Parser)
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
  <*> some (argument (eitherReader parseTarget) (metavar "targets..."))

attemptIO :: Target -> IO Bool -> IO Bool
attemptIO t f = do
  loginfo $ "Connecting to " ++ show t
  catches f [
    Handler (\(e :: HttpException) -> False <$ printHTTPEx e),
    Handler (\(e :: SomeException) -> False <$ loginfo ("Error connecting to " ++ show t ++ ": " ++ show e))
            ]

  where
    printHTTPEx (HttpExceptionRequest _ (StatusCodeException res _)) =
      loginfo $ show t ++ " " ++ (show.responseStatus) res
    printHTTPEx (HttpExceptionRequest _ he) = loginfo $ show t ++ " " ++ show he
    printHTTPEx e = loginfo $ "http exception: " ++ show e

tryConnect :: Target -> IO Bool
tryConnect targ@(TCP h p) = attemptIO targ $ connectTo h p >>= hClose >> pure True
tryConnect targ@(HTTP u) = attemptIO targ $ simpleHttp u >> pure True

connect :: Target -> IO Bool
connect targ = do
  (t,b) <- timedFun (tryConnect targ)
  when b $ loginfo $ "Connected to " ++ show targ ++ " in " ++ show t
  return b

waitforsockets :: Options -> IO ()
waitforsockets (Options _ req to things) = do
  let lth = fromIntegral $ length things
  let todo = if req == 0 || req > lth then lth else req
  let asyncs = mapM (async . waitfor) things
  _ <- waitN todo asyncs
  return ()

  where millis = (* 1000)
        waitfor :: Target -> IO (Maybe Bool)
        waitfor u = while $ timeout (millis to) (connect u)

waitAbsolutely :: Options -> IO ()
waitAbsolutely (Options 0 _ _ _) = forever (threadDelay 10000000)
waitAbsolutely (Options to _ _ _) = threadDelay (fromIntegral $ 1000 * to)

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  o <- execParser opts
  r <- race (waitAbsolutely o) (waitforsockets o)
  when (isLeft r) $ die "reached absolute timeout waiting for completion"

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Wait for network things.")
