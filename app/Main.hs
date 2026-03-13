module Main (main) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Random.Stateful as R

data EnumFoo = Eins | Zwei | Drei | Vier
  deriving (Eq, Show, Enum, Bounded, Ord)

type FreqTable k = Map.Map k (Map.Map k Int)

weightedSampleM :: (StatefulGen g m) => g -> [(b, Int)] -> m (Maybe b)
weightedSampleM gen weights = do
  let totalW = sum $ fmap snd weights
  if totalW <= 0
    then return Nothing
    else do
      r <- R.uniformRM (0, totalW - 1) gen
      return (pick r weights)
  where
    pick remainder ((b, w) : ws)
      | remainder < w = Just b
      | otherwise = pick (remainder - w) ws
    pick _ [] = error "This is a bug"

takeUntilM :: (Monad m) => (a -> Bool) -> m a -> Int -> m [a]
takeUntilM p m n = go 0 id
  where
    go counter acc = do
      x <- m
      if p x || counter >= n
        then return $ acc []
        else go (counter + 1) (acc . (x :))

markovStep ::
  (Ord k, StatefulGen g m) =>
  g ->
  FreqTable k ->
  StateT (Maybe k) m (Maybe k)
markovStep gen table = do
  mk <- get
  case mk of
    Nothing -> return Nothing
    Just k -> do
      -- let weights = Map.toList (table Map.! k)
      let weights = Map.toList $ Map.findWithDefault Map.empty k table 
      nxt <- lift $ weightedSampleM gen weights
      put nxt
      return nxt

markovGenerate :: (Ord k, StatefulGen g m) => 
    g -> FreqTable k -> Int -> m [k]
markovGenerate gen grid maxStates = do
    let start = (fst . Map.elemAt 0) grid
    samples <- evalStateT (takeUntilM (== Nothing) (markovStep gen grid) maxStates) (Just start)
    return (catMaybes samples)

pairWithNext :: [a] -> [(a,a)]
pairWithNext lst = go lst
    where go (a:b:xs) = (a,b) : go (b:xs)
          go _ = []

{-
    From a list of pairs (beg, end)
    Rows are indexed by beg
    Cols are a map indexed by end with a count
-}
buildFreqTable :: (Ord a) => [a] -> FreqTable a
buildFreqTable lst = cols
    where
        pairs = pairWithNext lst
        begs = fmap fst pairs
        rows = foldl toRow Map.empty begs
        cols = foldl toCol rows pairs

        toRow :: (Ord a) => FreqTable a -> a -> FreqTable a 
        toRow t x = Map.insert x Map.empty t
        toCol :: (Ord a) => FreqTable a -> (a,a) -> FreqTable a
        toCol t (beg, end) = Map.insert beg row' t
            where 
                row  = Map.findWithDefault Map.empty beg t
                row' = Map.insertWith bump end startVal row
                bump a b = a + b
                startVal = 1

-- TODO: Display the grid, grid to csv
-- TODO: Move these to test + doc strings
-- TODO: Input decoder
--      Bitmaps
--      Paragraphs

main :: IO ()
main = do

    -- let expected = Map.fromList [ ('0', Map.fromList [('0', 1), ('1', 2)]), 
    --                               ('1', Map.fromList [('0', 1), ('2', 1)]) 
    --                             ]
    -- let actual   = buildFreqTable "001012"

    -- let expected = Map.fromList [ ('0', Map.fromList [('0', 1), ('1', 2)]), 
    --                               ('1', Map.fromList [('0', 1), ('2', 1)]) 
    --                             ]
    -- let actual   = buildFreqTable "0010120"

    let expected = Map.fromList [ ('0', Map.fromList [('0', 1), ('1', 2)]), 
                                  ('1', Map.fromList [('0', 1), ('2', 1)]) 
                                ]
    let actual   = buildFreqTable [ Drei, Eins, Zwei, Drei, Vier, Eins ]
    gen <- R.newIOGenM (R.mkStdGen 23)
    samples <- markovGenerate gen actual 1024
    print $ samples
