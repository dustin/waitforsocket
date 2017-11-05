module Waitforsocket
    ( waitN
    , while
    , timedFun
    , Target(..)
    , parseTarget
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (waitAny, Async)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.List (isSubsequenceOf)
import Network (PortID(..))
import Options.Applicative (eitherReader)
import Options.Applicative.Types (ReadM)

data Target = TCP String PortID
            | HTTP String

instance Show Target where
  show (TCP s (Service p)) = "tcp@" ++ s ++ ":" ++ p
  show (HTTP s) = "web@" ++ s
  show _ = undefined

parseTarget :: ReadM Target
parseTarget = do
  eitherReader http <|> eitherReader tcp

  where tcp s = let h = takeWhile (/= ':') s
                    s' = drop (length h + 1) s in
                  Right $ TCP h (Service s')
        http s
          | isSubsequenceOf "://" s = Right $ HTTP s
          | otherwise = Left "not a URL"

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
