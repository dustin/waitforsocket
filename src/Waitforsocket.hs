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

import Control.Applicative ((<|>))
import Control.Concurrent.Async (waitAny, Async)
import Control.Concurrent (threadDelay)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.String (fromString)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Network (PortID(..))

data Target = TCP String PortID
            | HTTP String

instance Show Target where
  show (TCP s (Service p)) = "tcp@" ++ s ++ ":" ++ p
  show (HTTP s) = "web@" ++ s
  show _ = undefined

newtype Hostname = Hostname T.Text deriving (Show)

hostnameParser :: A.Parser Hostname
hostnameParser = do
  parts <- hpart `A.sepBy1` A.char '.'
  return $ Hostname (T.intercalate "." parts)
  where hpart :: A.Parser T.Text
        hpart = do
          t <- A.takeWhile1 (A.inClass "A-z0-9-")
          guard $ '-' `notElem` [T.head t, T.last t]
          return t

hostPortParser :: A.Parser (T.Text, T.Text)
hostPortParser = do
  (Hostname host) <- hostnameParser
  _ <- A.string ":"
  port <- A.takeWhile (A.inClass "A-z0-9")
  return (host, port)

newtype URL = URL T.Text deriving (Show)

urlParser :: A.Parser URL
urlParser = do
  prot <- A.string "http://" <|> A.string "https://"
  (Hostname host) <- hostnameParser
  rest <- A.takeText
  return $ URL $ T.concat [prot, host, rest]

parseTarget :: String -> Either String Target
parseTarget =
  A.parseOnly (http <|> tcp) . fromString
  where
    http = urlParser >>= \(URL u) -> return $ HTTP (T.unpack u)
    tcp = hostPortParser >>= \(h,p) -> return $ TCP (T.unpack h) (Service (T.unpack p))

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
