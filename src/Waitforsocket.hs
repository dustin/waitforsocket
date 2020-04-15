{-# LANGUAGE OverloadedStrings #-}

module Waitforsocket
    ( waitN
    , while
    , timedFun
    , Target(..)
    , parseTarget
    , Hostname(..)
    , URL(..)
    , hostnameParser
    , urlParser
    , hostPortParser
    ) where

import           Control.Applicative      ((<|>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, waitAny)
import           Control.Monad            (guard)
import qualified Data.Attoparsec.Text     as A
import           Data.Maybe               (fromMaybe)
import           Data.String              (fromString)
import qualified Data.Text                as T
import           Data.Time.Clock          (NominalDiffTime, diffUTCTime,
                                           getCurrentTime)
import           Network.Socket           (HostName, ServiceName)

data Target = TCP HostName ServiceName
    | HTTP String

instance Show Target where
  show (TCP s p) = "tcp@" ++ s ++ ":" ++ p
  show (HTTP s)  = "web@" ++ s

newtype Hostname = Hostname T.Text deriving (Show)

hostnameParser :: A.Parser Hostname
hostnameParser = do
  parts <- hpart `A.sepBy1` A.char '.'
  pure $ Hostname (T.intercalate "." parts)
  where hpart :: A.Parser T.Text
        hpart = do
          t <- A.takeWhile1 (A.inClass "A-z0-9-")
          guard $ '-' `notElem` [T.head t, T.last t]
          pure t

hostPortParser :: A.Parser (T.Text, T.Text)
hostPortParser = do
  (Hostname host) <- hostnameParser
  _ <- A.string ":"
  port <- A.takeWhile (A.inClass "A-z0-9")
  pure (host, port)

newtype URL = URL T.Text deriving (Show)

urlParser :: A.Parser URL
urlParser = do
  prot <- A.string "http://" <|> A.string "https://"
  (Hostname host) <- hostnameParser
  rest <- A.takeText
  pure $ URL $ T.concat [prot, host, rest]

parseTarget :: String -> Either String Target
parseTarget =
  A.parseOnly (http <|> tcp) . fromString
  where
    http = urlParser >>= \(URL u) -> pure $ HTTP (T.unpack u)
    tcp = hostPortParser >>= \(h,p) -> pure $ TCP (T.unpack h) (T.unpack p)

while :: Int -> IO (Maybe Bool) -> IO (Maybe Bool)
while d f = do
  v <- f
  if fromMaybe False v then pure v
    else threadDelay d >> while d f

waitN :: (Integral n) => n -> IO [Async a] -> IO [Async a]
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
