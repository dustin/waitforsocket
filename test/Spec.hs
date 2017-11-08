import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Network (PortID(..))
import Data.List (intercalate, tails)

import Waitforsocket

newtype ArbitraryHostname = ArbitraryHostname String deriving Show

instance Arbitrary ArbitraryHostname where
  arbitrary = do
    c <- choose (1, 4)
    parts <- vectorOf c seg
    return $ ArbitraryHostname $ intercalate "." parts

      where seg :: Gen String
            seg = do
              let valid = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
              first <- elements valid
              n <- choose (0,11)
              more <- vectorOf n $ elements ('-':valid)
              n' <- choose (0 + if n > 0 then 1 else 0, 1)
              evenmore <- vectorOf n' $ elements valid
              return (first:more ++ evenmore)

  shrink (ArbitraryHostname h)
    | length h == 1 = []
    | '.' `elem` h = map ArbitraryHostname (map (intercalate ".") $ shortn $ split h)
    | otherwise = map ArbitraryHostname (filter (\xs -> length xs > 0 && '-' `notElem` [head xs, last xs])
                                         $ shortn h)
    where split :: String -> [String]
          split h = foldr (\x (w:ws) -> if x == '.' then []:w:ws else (x:w):ws) [[]] h
          shortn :: [a] -> [[a]]
          shortn a = go (length a - 1) a
            where go _ [] = []
                  go n l@(_:ls)
                    | length l < n = []
                    | otherwise = take n l : go n ls

propTCPTargetParsing :: ArbitraryHostname -> Positive Int -> Bool
propTCPTargetParsing (ArbitraryHostname h) (Positive p) =
  case parseTarget (h ++ ":" ++ show p) of
    Right (TCP h (Service p)) -> True
    _ -> False

propHTTPTargetParsing :: ArbitraryHostname -> Bool
propHTTPTargetParsing (ArbitraryHostname h) =
  let u = "http://" ++ h ++ "/stuff" in
    case parseTarget u of
      Right (HTTP u) -> True
      _ -> False

tests :: [TestTree]
tests = [
  testProperty "target TCP parsing" propTCPTargetParsing,
  testProperty "target HTTP parsing" propHTTPTargetParsing
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
