import Test.Tasty
import Test.Tasty.HUnit
import qualified Markov as M
import System.Random.Stateful as R

import Data.Map as Map

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [unitTests]

unitTests = testGroup "1D FreqTable" 
    [ 
      basic1DFreqTable,
      basic1DFreqTable_NoFinalState,
      worksWithAnyOrd,
      markovGenerateGolden
    ]

basic1DFreqTable = testCase "1D String, 1 Final State" $
    assertEqual "actual == expected" actual expected
    where
        actual   = M.buildFreqTable "001012"
        expected = Map.fromList [ ('0', Map.fromList [('0', 1), ('1', 2)]), 
                                  ('1', Map.fromList [('0', 1), ('2', 1)]) 
                                ]
basic1DFreqTable_NoFinalState = testCase "1D String, No Final State" $
    assertEqual "actual == expected" actual expected
    where
        actual   = M.buildFreqTable "0010120"
        expected = Map.fromList [ ('0', Map.fromList [('0', 1), ('1', 2)]), 
                                  ('1', Map.fromList [('0', 1), ('2', 1)]),
                                  ('2', Map.fromList [('0', 1)])
                                ]

markovGenerateGolden = testCase "Markov golden test 1" $ do
    let freq = M.buildFreqTable "00101011100012"
    gen <- R.newIOGenM (R.mkStdGen 23)
    samples <- M.markovGenerate gen freq 16 
    assertEqual "actual == expected" samples "0001000010011112"

data EnumFoo = Eins | Zwei | Drei | Vier
  deriving (Eq, Show, Enum, Bounded, Ord)

worksWithAnyOrd = testCase "Works with any Ord type" $
    assertEqual "actual == expected" (actual == expected) True
    where
        expected = Map.fromList [ (Drei, Map.fromList [(Eins, 1), (Vier, 1)]), 
                                  (Eins, Map.fromList [(Zwei, 1), (Vier, 1)]), 
                                  (Zwei, Map.fromList [(Drei, 1)]), 
                                  (Vier, Map.fromList [(Eins, 2)])
                                ]
        actual   = M.buildFreqTable [ Drei, Eins, Zwei, Drei, Vier, Eins, Vier, Eins ]
