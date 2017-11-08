import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Network (PortID(..))
import Data.List (intercalate)

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
              n <- choose (0,12)
              more <- vectorOf n $ elements ('-':valid)
              return (first:more)

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
