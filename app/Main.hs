module Main (main) where

import Markov
import System.Random.Stateful as R
import qualified Data.Map as Map

data EnumFoo = Eins | Zwei | Drei | Vier
  deriving (Eq, Show, Enum, Bounded, Ord)


-- TODO: Display the grid, grid to csv
-- TODO: Input decoder
--      Bitmaps
--      Paragraphs

main :: IO ()
main = do

    let expected = Map.fromList [ (Drei, Map.fromList [(Eins, 1), (Vier, 1)]), 
                                  (Eins, Map.fromList [(Zwei, 1), (Vier, 1)]), 
                                  (Zwei, Map.fromList [(Drei, 1)]), 
                                  (Vier, Map.fromList [(Eins, 2)])
                                ]
    let actual   = buildFreqTable [ Drei, Eins, Zwei, Drei, Vier, Eins, Vier, Eins ]
    gen <- R.newIOGenM (R.mkStdGen 23)
    samples <- markovGenerate gen actual 1024
    print $ samples
