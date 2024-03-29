{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Waitforsocket

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, race)
import           Control.Concurrent.Timeout   (timeout)
import           Control.Exception.Safe       (Handler (..), SomeException, catches)
import           Control.Monad                (forever, when)
import           Data.Either                  (isLeft)
import qualified Data.List.NonEmpty           as NE
import           Network.HTTP.Conduit         (HttpException (..), HttpExceptionContent (..), responseStatus,
                                               simpleHttp)
import           Network.Socket               (AddrInfo (..), SocketType (..), close, connect, defaultHints,
                                               getAddrInfo, socket)
import           Numeric.Natural
import           Options.Applicative          (Parser, argument, auto, eitherReader, execParser, fullDesc, help, helper,
                                               info, long, metavar, option, progDesc, showDefault, value, (<**>))
import           Options.Applicative.NonEmpty (some1)
import           System.Exit                  (die)

data Options = Options
    { optAbsTimeout :: Natural
    , optRequired   :: Natural
    , optTimeout    :: Natural
    , failDelay     :: Natural
    , targets       :: NE.NonEmpty Target
    }

loginfo :: String -> IO ()
loginfo = putStrLn

options :: Parser Options
options = Options
  <$> option auto (long "absTimeout" <> showDefault <> value 0 <> help "absolute timeout")
  <*> option auto (long "required" <> showDefault <> value 0 <> help "how many connections required (0 = all)")
  <*> option auto (long "timeout" <> showDefault <> value 5000 <> help "connect/retry timeout (ms)")
  <*> option auto (long "faildelay" <> showDefault <> value 1 <> help "seconds to delay before retrying")
  <*> some1 (argument (eitherReader parseTarget) (metavar "targets..."))

attemptIO :: Target -> IO Bool -> IO Bool
attemptIO t f = do
  loginfo $ "Connecting to " <> show t
  catches f [
    Handler (\(e :: HttpException) -> False <$ printHTTPEx e),
    Handler (\(e :: SomeException) -> False <$ loginfo ("Error connecting to " <> show t <> ": " <> show e))
            ]

  where
    printHTTPEx (HttpExceptionRequest _ (StatusCodeException res _)) =
      loginfo $ show t <> " " <> (show.responseStatus) res
    printHTTPEx (HttpExceptionRequest _ he) = loginfo $ show t <> " " <> show he
    printHTTPEx e = loginfo $ "http exception: " <> show e

tryConnect :: Target -> IO Bool
tryConnect targ@(HTTP u) = attemptIO targ $ simpleHttp u >> pure True
tryConnect targ@(TCP h p) = attemptIO targ $ connectTo >>= close >> pure True
  where connectTo = open =<< resolve
        resolve = do
          let hints = defaultHints { addrSocketType = Stream }
          head <$> getAddrInfo (Just hints) (Just h) (Just p)
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          pure sock

contact :: Target -> IO Bool
contact targ = do
  (t,b) <- timedFun (tryConnect targ)
  when b $ loginfo $ "Connected to " <> show targ <> " in " <> show t
  pure b

waitforsockets :: Options -> IO ()
waitforsockets (Options _ req to fd things) = do
  let lth = fromIntegral $ length things
      todo = if req == 0 || req > lth then lth else req
      asyncs = traverse (async . waitfor) things
  (t, _) <- timedFun $ waitN todo (NE.toList <$> asyncs)
  loginfo $ "Finished in " <> show t

  where millis =  (* 1000)
        seconds = (* 1000) . millis
        waitfor u = while (seconds fd) $ timeout (fromIntegral (millis to)) (contact u)

waitAbsolutely :: Options -> IO ()
waitAbsolutely (Options 0 _ _ _ _)  = forever (threadDelay 10000000)
waitAbsolutely (Options to _ _ _ _) = threadDelay (fromIntegral $ 1000 * to)

main :: IO ()
main = do
  o <- execParser opts
  r <- race (waitAbsolutely o) (waitforsockets o)
  when (isLeft r) $ die "reached absolute timeout waiting for completion"

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Wait for network things.")
