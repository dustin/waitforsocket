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
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.List (isSubsequenceOf)
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
hostnameParser = pure . Hostname =<< A.takeWhile (`elem` ('.':['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']))

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
  return $ URL $ prot `T.append` host `T.append` rest

parseTarget :: String -> Either String Target
parseTarget s =
  http <|> tcp

  where
    tcp = case A.parseOnly hostPortParser (T.pack s) of
            Right (h,p) -> Right (TCP (T.unpack h) (Service (T.unpack p)))
            _ -> Left "can't parse service"
    http
      | isSubsequenceOf "://" s = Right $ HTTP s
      | otherwise = Left "can't parse URL"

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
