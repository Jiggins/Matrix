--module Math.Matrix.ReducedRowEchelon where
module Math.Matrix.REF where

import Math.Matrix
import Math.Vector

import qualified Data.Vector as V

type RowOperation a = (Vector a -> Vector a -> Vector a)

rowOperation :: RowOperation a -> Int -> Int -> Matrix a -> Matrix a
rowOperation op a b m = fromVectors . V.generate (rows m) $
	\i -> if i == a
			then row a m `op` row b m
			else row i m

addRow, subRow :: (Num a) => Int -> Int -> Matrix a -> Matrix a
addRow = rowOperation (+)
subRow = rowOperation (-)

multilplyRowBy :: Num a => a -> Int -> Matrix a -> Matrix a
multilplyRowBy n = mapRow (* n)
