
module Main where

import Math.HKMeans.KMeans

import Data.List
import Data.Ord
import Control.Monad
import System.Environment
import System.Random
import Data.Packed.Vector
import Numeric.LinearAlgebra
import qualified Data.String.Utils as S


-- Displays the usage
usage :: IO ()
usage = putStrLn $
       "KMeansCheck usage :\n"
    ++ "--perf dim n k (runs a performance test with n random n-dimentional vectors and k clusters)\n"
    ++ "--normal (runs a custom test to check the clustering)"


-- Cosine distance
cosine :: Vector Double -> Vector Double -> Double
cosine d1 d2 = (d1 <.> d2) / (norm1 d1) * (norm1 d2)


-- Euclidian distance
edistance :: Vector Double -> Vector Double -> Double
edistance d1 d2 = norm1 (d1 - d2)


-- Generates random vectors
generateVecs :: Int -> Int -> IO [Vector Double]
generateVecs nb d = replicateM nb (fromList `fmap` replicateM d (randomRIO (-10, 10)))


printClustering :: ([Int], [Vector Double]) -> IO ()
printClustering (clusterIds, vectors) =
    let items = zip clusterIds vectors
        sorted = sortBy (comparing fst) items -- Sort by cluster id
        grouped = groupBy (\a b -> fst a == fst b) sorted -- Group by cluster id
        strings = map (map (show . toList . snd)) grouped
        joined = map (S.join " ") strings
    in putStrLn $ S.join "\n" joined


-- Run a custom test to check clustering
runNormal :: IO ()
runNormal =
    let datas = (map fromList [[1], [3], [5], [42], [43], [44], [100], [105]]) in
    do
        test 1 datas
        test 2 datas
        test 3 datas
        test 4 datas
        test 5 datas
    where
        test k datas =
            do
                putStrLn $ "New Check (k = " ++ (show k) ++ ")"
                printClustering $ kmeans edistance k datas


-- Generate n random d-dimentional vectors and then run the clustering with k cluster
runPerf :: [String] ->  IO ()
runPerf (_dim:_nb:_k:[]) =
    let (d, n, k) = (read _dim :: Int, read _nb :: Int, read _k :: Int) in
    do
        vectors <- generateVecs n d
        let cls = kmeans edistance k vectors
        putStrLn . show $ cls
runPerf _ = error "Incorrect arguments"


main :: IO ()
main = do
  args <- getArgs
  let (demo:t) = args
  case demo of
    "--perf" -> runPerf t
    "--normal" -> runNormal
    _ -> usage
