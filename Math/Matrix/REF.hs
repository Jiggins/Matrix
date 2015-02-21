--module Math.Matrix.ReducedRowEchelon where
module Math.Matrix.REF where

import Data.List

import Math.Matrix
import Math.Vector

import qualified Data.Vector as V

type RowOperation a = (Vector a -> Vector a -> Vector a)

rowOperation :: RowOperation a -> Int -> Int -> Matrix a -> Matrix a
rowOperation op a b m = fromVectors . V.generate (rows m) $
	\i -> if i == a 
			then (row a m) `op` (row b m)
			else row i m

addRow :: (Num a) => Int -> Int -> Matrix a -> Matrix a
addRow = rowOperation (+)
subRow = rowOperation (-)

multilplyRowBy :: Num a => a -> Int -> Matrix a -> Matrix a
multilplyRowBy n = mapRow (* n)

sample :: Matrix Integer
sample  = fromLists [[1,2,1], [2,2,2], [1,0,1]]
sample' = fromLists [[0,3,-6,6,4,-5], [3,-7,8,-5,8,9], [3,-9,12,-9,6,15]]

main = do
	--print $ fromLists [[1,2,3], [2,3,4]]
	--putStrLn ""
	print . multilplyRowBy 3 0 . fromLists $ [[1,2,1], [2,2,2], [1,0,1]]