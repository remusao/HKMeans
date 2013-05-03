module Math.HKMeans.KMeans(kmeans) where

import Data.List
import Data.Ord

type Distance a = (a -> a -> Double)

-- Cluster the datas into k parts
initClustering :: Int -> [Int]
initClustering k = cycle [1..k]
{-# INLINE initClustering #-}


-- Compute the centroids of each cluster
getCentroids :: (Num a, Fractional a) => ([Int], [a]) -> [a]
getCentroids (clusterIds, vectors) =
    map (\l -> (sum . map snd $ l) / (fromIntegral $ length l))
    . groupBy (\a b -> fst a == fst b) -- Group by cluster id
    . sortBy (comparing fst) -- Sort by cluster id
    . zip clusterIds $ vectors -- Convert from ([], []) to [(,)]
{-# INLINE getCentroids #-}


-- Cluster items
clusterize :: Distance a -> [a] -> [(Int, a)] -> [Int]
clusterize distance datas centroids = [c | c <- map closest datas]
    where
        closest vector = fst $ minimumBy (comparing (distance vector . snd)) centroids
{-# INLINE clusterize #-}


-- KMeans naive algorithm
kmeans :: (Eq a, Fractional a, Num a) => Distance a -> Int -> [a] -> ([Int], [a])
kmeans distance k datas
    | k == 0        = error "Clustering with 0 cluster is impossible"
    | datas == []   = ([], [])
    | k == 1        = (take (length datas) $ initClustering 1, datas)
    | otherwise     =
        let initialClusters = initClustering k -- Init the items with random cluster id
            centroids = getCentroids (initialClusters, datas) -- Init the centroids
        in (kmeans' centroids initialClusters, datas)
        where
            kmeans' centroids clusterIds
                | clusterIds == newIds = clusterIds
                | otherwise     = kmeans' (getCentroids (clusterIds, datas)) newIds
                where newIds = clusterize distance datas $ zip [1..] centroids
