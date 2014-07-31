module Math.Matrix where 

import Math.Vector
import Data.Vector ((!))
import Data.List hiding (transpose)
import Data.Ord
import qualified Data.Vector as V

data Matrix a = Matrix { rows    :: Int -- ^ Number of rows
                       , columns :: Int -- ^ Number of Columns
                       , vectors :: (Vector (Vector a))
                      } deriving (Eq)


instance (Num a) => Num (Matrix a) where
    (+) = mZipWith (+)
    (*) = multiplyMatrix
    abs = undefined
    negate = undefined
    signum = undefined
    fromInteger = undefined

instance Functor Matrix where
    fmap f (Matrix row col vec) = Matrix row col (fmap (fmap f) vec)

instance Show a => Show (Matrix a) where
    show = intercalate "\n" . pad . toLists . fmap show
      where bars = ("|"++) . (++" |")
            pad v = map (bars . concat . map ((' ':) . padTo (maxLength v))) v
              where maxLength = length . maximumBy (comparing length) . concat
                    padTo n s | length s == n = s
                              | otherwise = padTo n $ ' ' : s

-- * Matrix Creation

-- | Creates an m x n matrix with the given generator function.
matrix :: Int               -- ^ Number of rows
       -> Int               -- ^ Number of columns
       -> ((Int, Int) -> a) -- ^ Generator function
       -> Matrix a
matrix row col f = Matrix row col newMatrix
    where newMatrix = V.generate row (\i -> V.generate col $ (curry f) i)

{- | Creates an n x n matrix where the main diagonal = 0.
    >>> identity 3
    | 1 0 0 |
    | 0 1 0 |
    | 0 0 1 |
-}
identity :: Num a => Int -> Matrix a
identity n = matrix n n (\(i,j) -> if i == j then 1 else 0)

-- | Creates an m x n matrix of 0's
zeroMatrix :: Num a => Int -> Int -> Matrix a
zeroMatrix m n = matrix m n (\_ -> 0)

{- | Creates an n x m matrix of Ints in ascending order.
>>> inOrderMatrix 4 4
|  1  2  3  4 |
|  5  6  7  8 |
|  9 10 11 12 |
| 13 14 15 16 |
-}
inOrderMatrix :: Int -> Int -> Matrix Int
inOrderMatrix n m = matrix n m $ \(i,j) -> i*m + j+1

-- * Accessing

-- ** Accessing elements

-- | returns the element at position (i,j)
getElem :: (Int, Int) -> Matrix a -> a
getElem (i, j) m 
    | i >= rows m = error $ "Row Index out of bounds " ++ show (i,j)
    | j >= columns m = error $ "Column Index out of bounds " ++ show (i,j)
    | otherwise = (vectors m ! i) ! j

-- | returns the row at the the given index.
--   Note: index starts at 0.
row :: Int -> Matrix a -> Vector a
row i m | i < 0      = error $ "Negative index " ++ show i
        | i > rows m = error $ "Index out of bounds " ++ show i
        | otherwise  = vectors m ! i

-- | returns the row at the the given index.
--   Note: index starts at 0.
column :: Int -> Matrix a -> Vector a
column i m | i < 0         = error $ "Negative index " ++ show i
           | i > columns m = error $ "Index out of bounds " ++ show i
           | otherwise     = fmap (!i) . vectors $ m

-- | Returns the main diagonal
mainDiagonal :: Matrix a -> Vector a
mainDiagonal m = V.generate ((columns m) `min` (rows m)) (\i -> getElem (i,i) m)

-- ** Properties

-- | Returns the size of the matrix as a pair (rows, columns)
size :: Matrix a -> (Int,Int)
size m = (rows m, columns m)

-- | returns True if matrix is square
square :: Matrix a -> Bool
square m = (rows m) == (columns m)

-- *  Matrix Manipulation

-- ** Single Matrix Operations

-- | Returns the transpose of a matrix
--   Swaps the rows and columns of a vector
transpose :: Matrix a -> Matrix a
transpose m = matrix (columns m) (rows m) (\(i,j) -> getElem (j,i) m)

-- | Synonym for (*)
--   Multiplies the two given matrices.
--   Creates a new matrix from the dot product of the rows of m
--   and the columns of n
multiplyMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
multiplyMatrix m n | (columns n) /= (rows m) = error "Invalid sizes"
                   | otherwise = matrix (rows m) (columns n)
                        (\(i,j) -> (row i m) <.> (column j n))

-- | zipWith function defined for matrices
mZipWith :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
mZipWith f m n = matrix row col (\(i,j) -> getElem (i,j) m `f` getElem (i,j) n)
    where row = min (rows m) (rows n)
          col = min (columns m) (columns n)

-- ** Row and Column Operations

mapRow :: (a -> a) -> Int -> Matrix a -> Matrix a
mapRow f r m = matrix (rows m) (columns m) $ 
    \(i,j) -> if i == r then f $ getElem (i,j) m else getElem (i,j) m

mapColumn :: (a -> a) -> Int -> Matrix a -> Matrix a
mapColumn f c m = matrix (rows m) (columns m) $
    \(i,j) -> if j == c then f $ getElem (i,j) m else getElem (i,j) m

swapRow :: Int -> Int -> Matrix a -> Matrix a
swapRow a b m = matrix (rows m) (columns m) swap 
    where swap (i,j) | i == a = getElem (b,j) m
                     | i == b = getElem (a,j) m
                     | otherwise = getElem (i,j) m

swapColumn :: Int -> Int -> Matrix a -> Matrix a
swapColumn a b m = matrix (rows m) (columns m) swap
    where swap (i,j) | j == a = getElem (b,j) m
                     | j == b = getElem (a,j) m
                     | otherwise = getElem (i,j) m

-- ** Joining Matrices

(<|>) :: Matrix a -> Matrix a -> Matrix a
m <|> n = matrix (rows m `min` rows n) (columns m + columns n) generate
    where generate (i,j) 
            | j < columns m = getElem (i,j) m
            | otherwise     = getElem (i, j - columns m) n

(<->) :: Matrix a -> Matrix a -> Matrix a
m <-> n = matrix (rows m + rows n) (columns m `min` columns n) generate
    where generate (i,j) 
            | i < rows m = getElem (i,j) m
            | otherwise  = getElem (i - rows m, j) n

-- * Conversions

toLists :: Matrix a -> [[a]]
toLists = V.toList . fmap V.toList . vectors

{- | Converts lists to Matrix

>>> fromLists [[1,2,3], [4,5,6]]
| 1 2 3 |
| 4 5 6 |

-}
fromLists :: [[a]] -> Matrix a
fromLists = fromVectors . V.fromList . map V.fromList

fromVectors :: Vector (Vector a) -> Matrix a
fromVectors vs = Matrix row col vs
    where row = V.length vs
          col = V.minimum $ fmap V.length vs

-- | Creates a vector from a matrix
--   In the order (0,0), (0,1), .. (0,n), (1,0), (1,1), .. (n,n)
toVector :: Matrix a -> Vector a
toVector = V.concat . V.toList . vectors