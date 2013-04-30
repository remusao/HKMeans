module Math.HKMeans(kmeans) where

import Prelude hiding
    (all,
    cycle,
    head,
    iterate,
    last,
    length,
    map,
    sum,
    take,
    zip,
    (++))
import Numeric.LinearAlgebra
import Data.List.Stream
import Data.Ord

type Distance = (Vector Double -> Vector Double -> Double)

-- Cluster the datas into k parts
initClustering :: Int -> [Int]
initClustering k = cycle [1..k]
{-# INLINE initClustering #-}


-- Compute the centroids of each cluster
getCentroids :: Int -> ([Int], [Vector Double]) -> [Vector Double]
getCentroids k (ids, vectors) = let items = zip ids vectors in map (getCentroid items) [1..k]
    where
        -- Sum the vectors that have the same cluster id and divide the sum to get the mean
        getCentroid ::  [(Int, Vector Double)] -> Int -> Vector Double
        getCentroid items cid = let (acc, s) = sumVectors items cid
            in s / fromIntegral acc
        -- Sum the vectors that are in the cluster `cid`
        sumVectors :: [(Int, Vector Double)] -> Int -> (Int, Vector Double)
        sumVectors [] _ = (0, 0)
        sumVectors ((c, vec):t) cid
            | c == cid   = let (acc, v) = sumVectors t cid in (acc + 1, v + vec)
            | otherwise = sumVectors t cid


-- Return the closest Centroid from the given vector
closest :: Distance -> [(Int, Vector Double)] -> Vector Double -> Int
closest d centroids vec = fst $ minimumBy (comparing (d vec . snd)) centroids
{-# INLINE closest #-}


-- Cluster items
clusterize :: Distance -> [Vector Double] -> [Vector Double] -> [Int]
clusterize d datas centroids = [c | c <- map (closest d $ zip [1..] centroids) datas]
{-# INLINE clusterize #-}


-- KMeans naive algorithm
kmeans :: Distance -> Int -> [Vector Double] -> ([Int], [Vector Double])
kmeans distance k datas
    | k == 0        = error "Clustering with 0 cluster is impossible"
    | datas == []   = ([], [])
    | k == 1        = (take (length datas) $ initClustering 1, datas)
    | otherwise     =
        let clusters = initClustering k -- Init the items with random cluster id
            centroids = getCentroids k (clusters, datas) -- Init the centroids
        in (kmeans' centroids clusters, datas)
        where
            kmeans' :: [Vector Double] -> [Int] -> [Int]
            kmeans' centroids ids
                | ids == newIds = ids
                | otherwise     = kmeans' (getCentroids k (ids, datas)) newIds
                where newIds = clusterize distance datas centroids
