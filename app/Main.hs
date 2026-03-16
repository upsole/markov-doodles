module Main (main) where

import qualified Markov as M
import System.Random.Stateful as R

main :: IO ()
main = do
    fortune <- readFile "samples/8ball.txt"
    let table = M.buildFreqTable (lines fortune)
    gen <- R.newIOGenM (R.mkStdGen 3)
    generated <- M.markovGenerate gen table 1
    print generated
