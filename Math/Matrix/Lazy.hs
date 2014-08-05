module Math.Matrix.Lazy where

import Math.Util
import Math.Matrix (matrix, Matrix)

data GMatrix a = GMatrix { rows     :: Int -- ^ Number of rows
                         , columns  :: Int -- ^ Number of Columns
                         , function :: (Int, Int) -> a -- ^ Generator function
                        }

instance Show a => Show (GMatrix a) where
	show = show . generate

instance Functor GMatrix where
	fmap f m = GMatrix (rows m) (columns m) (f . (function m))

lmatrix :: Int -> Int -> ((Int,Int) -> a) -> GMatrix a
lmatrix n m f = GMatrix n m f

generate :: GMatrix a -> Matrix a
generate m = matrix (rows m) (columns m) (function m) 