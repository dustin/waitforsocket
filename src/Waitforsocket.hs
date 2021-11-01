{-# LANGUAGE OverloadedStrings #-}

module Waitforsocket
    ( waitN
    , while
    , timedFun
    , Target(..)
    , parseTarget
    , URL(..)
    , hostnameParser
    , urlParser
    , hostPortParser
    ) where

import           Control.Applicative      ((<|>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, waitAny)
import           Control.Monad            (guard, unless)
import qualified Data.Attoparsec.Text     as A
import           Data.Foldable            (fold)
import           Data.String              (fromString)
import qualified Data.Text                as T
import           Data.Time.Clock          (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Network.Socket           (HostName, ServiceName)
import           Numeric.Natural

data Target = TCP HostName ServiceName
    | HTTP String

instance Show Target where
  show (TCP s p) = fold ["tcp@", s, ":", p]
  show (HTTP s)  = fold ["web@", s]

hostnameParser :: A.Parser T.Text
hostnameParser = T.intercalate "." <$> hpart `A.sepBy1` A.char '.'
  where hpart :: A.Parser T.Text
        hpart = do
          t <- A.takeWhile1 (A.inClass "A-z0-9-")
          guard $ '-' `notElem` [T.head t, T.last t]
          pure t

hostPortParser :: A.Parser (T.Text, T.Text)
hostPortParser = (,) <$> (hostnameParser <* ":") <*> A.takeWhile (A.inClass "A-z0-9")

newtype URL = URL T.Text deriving (Show)

urlParser :: A.Parser URL
urlParser = do
  prot <- A.string "http://" <|> A.string "https://"
  host <- hostnameParser
  rest <- A.takeText
  pure $ URL $ fold [prot, host, rest]

parseTarget :: String -> Either String Target
parseTarget = A.parseOnly (http <|> tcp) . fromString
  where
    http = urlParser >>= \(URL u) -> pure $ HTTP (T.unpack u)
    tcp = hostPortParser >>= \(h,p) -> pure $ TCP (T.unpack h) (T.unpack p)

while :: Natural -> IO (Maybe Bool) -> IO ()
while d f = f >>= \v -> unless (v == Just True) $ threadDelay (fromIntegral d) *> while d f

waitN :: Natural -> IO [Async a] -> IO [Async a]
waitN 0 asyncs = asyncs
waitN n asyncs = do
  as <- asyncs
  (done, _) <- waitAny as
  waitN (pred n) $ pure $ filter (/= done) as

timedFun :: IO a -> IO (NominalDiffTime, a)
timedFun f = do
  start <- getCurrentTime
  a <- f
  end <- getCurrentTime
  pure (diffUTCTime end start, a)
