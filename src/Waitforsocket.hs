module Waitforsocket
    ( waitN
    , while
    , timedFun
    , Target(..)
    , parseTarget
    ) where

import Control.Concurrent.Async (waitAny, Async)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Network (PortID(..))
import Options.Applicative.Types (ReadM, readerAsk)

data Target = TCP String PortID

instance Show Target where
  show (TCP s (Service p)) = s ++ ":" ++ p
  show _ = undefined

parseTarget :: ReadM Target
parseTarget = do
  s <- readerAsk
  let h = takeWhile (/= ':') s
  let s' = drop (length h + 1) s
  return $ TCP h (Service s')

while :: IO (Maybe Bool) -> IO (Maybe Bool)
while f = do
  v <- f
  if fromMaybe False v then return v
    else threadDelay 1000000 >> while f

waitN :: (Integral n) => n -> IO [Async a] -> IO [Async a]
waitN 0 asyncs = asyncs
waitN n asyncs = do
  as <- asyncs
  (done, _) <- waitAny as
  waitN (pred n) $ return $ filter (/= done) as

timedFun :: IO a -> IO (NominalDiffTime, a)
timedFun f = do
  start <- getCurrentTime
  a <- f
  end <- getCurrentTime
  return (diffUTCTime end start, a)
