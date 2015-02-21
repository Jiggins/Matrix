module Math.Matrix.Lazy where

import Math.Matrix (matrix, Matrix)

data LazyMatrix a = LazyMatrix { rows     :: Int -- ^ Number of rows
                               , columns  :: Int -- ^ Number of Columns
                               , function :: (Int, Int) -> a -- ^ Generator
                              }                              -- function

instance Show a => Show (LazyMatrix a) where
	show = show . generate

instance Functor LazyMatrix where
	fmap f m = LazyMatrix (rows m) (columns m) (f . function m)

lmatrix :: Int -> Int -> ((Int,Int) -> a) -> LazyMatrix a
lmatrix = LazyMatrix

generate :: LazyMatrix a -> Matrix a
generate m = matrix (rows m) (columns m) (function m)

fromList :: Int -> Int -> [a] -> LazyMatrix a
fromList r c xs = lmatrix r c $ \(i,j) -> xs !! (i*c+j)
