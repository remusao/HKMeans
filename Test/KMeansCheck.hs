

module Main where

import Math.HKMeans

import Control.Monad
import System.Environment
import System.Random
import Data.Packed.Vector
import Numeric.LinearAlgebra

-- Cosine distance used by the KMeans algorithm
cosine :: Vector Double -> Vector Double -> Double
cosine d1 d2 = (d1 <.> d2) / (norm1 d1) * (norm1 d2)

-- Euclidian distance
edistance :: Vector Double -> Vector Double -> Double
edistance d1 d2 = norm1 (d1 - d2)


{-
main :: IO ()
main = let datas = (map fromList [[1], [3], [5], [42], [43], [44], [100], [105]]) in
    do
        test 1 datas
        test 2 datas
        test 3 datas
        test 4 datas
        test 5 datas
    where
        test k datas = print $ kmeans edistance k datas
-}
-- Benchmarking my 'kmeans-vector' package


main :: IO ()
main = do
  [dimension, nb, k] <- getArgs
  go dimension nb k 10
  where go :: String -> String -> String -> Int -> IO ()
        go _ _ _ 0 = return ()
        go d nb k j = do
          vectors <- generateVecs (read nb :: Int) (read d :: Int)
          let cls = kmeans edistance (read k :: Int) vectors
          putStrLn . show $ cls
          go d nb k (j - 1)

generateVecs :: Int -> Int -> IO [Vector Double]
generateVecs nb d = do
  replicateM nb (fromList `fmap` replicateM d (randomRIO (-10, 10)))
