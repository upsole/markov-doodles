{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import qualified Markov as M
import qualified Image as I
import System.Random.Stateful as R
import Data.List (intercalate)
import System.Environment
import System.Exit
import System.IO

run8Ball :: (R.StatefulGen g IO) => g -> IO ()
run8Ball gen = do
    contents <- readFile "samples/8ball.txt"
    let table = M.buildFreqTable (lines contents)
    generatedLines <- M.markovGenerate gen table 5
    print $ unwords generatedLines 

runGenImage :: (R.StatefulGen g IO) => g -> String -> String -> IO ()
runGenImage gen input output = do
    contents <- readFile input
    let table = M.buildFreqTable (lines contents)
    generated <- M.markovGenerate gen table 50
    I.writeToFile output generated

runGenText :: (R.StatefulGen g IO) => g -> String -> String -> IO ()
runGenText gen input output = do
    contents <- readFile input
    let table = M.buildFreqTable (lines contents)
    generated <- M.markovGenerate gen table 50
    writeFile output $ unwords generated

main :: IO ()
main = do
    args <- getArgs
    gen  <- R.newStdGen >>= R.newIOGenM
    case args of 
        ["image", input, output] -> runGenImage gen input output
        ["text", input, output]  -> runGenText gen input output
        ["8ball"] -> run8Ball gen
        _        -> do 
            hPutStrLn stderr "usage: [image|text] input_filename output_filename or 8ball"
            exitWith (ExitFailure 1)
