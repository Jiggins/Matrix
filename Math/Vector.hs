--------------------------------------------------------------------------------
{- |
Module      :  Math.Vector
Description :  A Haskell implementation of Numeric Vectors and Matrices

License     :  GPL-3
Maintainer  :  jackhiggins07@gmail.com
Stability   :  Stable
Portability :  Portable

A Haskell implementation of Numeric Vectors, Matrices and Lines.

This class defines an extension of 'Data.Vector' to introduce numeric operation
for vectors.  It begins by adding an instance for Num Vector,  allowing for
numeric vectors to be treated as numbers.

@
example: let u = |1,2,3|
         let v = |3,4,5|
    then u + v = |4,6,8|
@

It also implements common vector functions such as dot and (eventually) cross
products.
-}
--------------------------------------------------------------------------------
module Math.Vector where

import Data.List.Split
import Math.Util
import Data.Vector ((!))
import qualified Data.Vector as V

-- | Points are expressed as 'Data.Vector's.
--   Points can be of any dimension.  P ∈ Rⁿ
type Point  = V.Vector

-- | Vectors are expressed as 'Data.Vector's.
--   Vectors can be of any dimension.  V ∈ Rⁿ
type Vector = V.Vector

instance Num a => Num (V.Vector a) where
  (+) = V.zipWith (+)
  (*) = V.zipWith (*)
  abs = fmap abs
  negate      = fmap negate
  signum      = undefined
  fromInteger = undefined

-- * Operators

-- | Infix operator for the 'dot' product of two Vectors
(<.>) :: (Num a) => Vector a -> Vector a -> a
(<.>) = dotProduct

-- | Infix operator for the 'dot' product of two Vectors
(·) :: (Num a) => Vector a -> Vector a -> a
(·) = dotProduct

-- | Infix operator for 'perpendicular'
(-|) :: (Eq a, Num a) => Vector a -> Vector a -> Bool
(-|) = perpendicular

-- | Infix operator for 'perpendicular'
(⟂) :: (Eq a, Num a) => Vector a -> Vector a -> Bool
(⟂) = perpendicular

-- | Infix operator for 'parallel'
(-||) :: (Eq a, Floating a) => Vector a -> Vector a -> Bool
(-||) = parallel

-- | Multiply a functor (Matrix or Vector) by a scalar
(·>) :: (Functor f, Num a) => f a -> a -> f a
vector ·> scalar = fmap (*scalar) vector

-- | Multiply a functor (Matrix or Vector) by a scalar
(<·) :: (Functor f, Num a) => a -> f a -> f a
scalar <· vector = vector ·> scalar

-- * Vector Creation

-- | Vector a relative to b
newVector :: (Num a) => Point a -> Point a -> Vector a
newVector a b = pointToVector $ b - a

-- | Reads a vector from a String in the form
--   <x0, x1, .. xn>
readVector :: Read a => String -> Vector a
readVector  = fmap read . f
  where f (x:xs) | x /= '<'  = error "Parse error"
                 | last xs  /= '>' = error "Parse error"
                 | otherwise = V.fromList . splitOn "," . init $ xs

-- * Vector Operations

-- ** Angles

-- | Get the angle between two vectors
-- Probably wrong
angle :: (Floating a) => Vector a -> Vector a -> a
angle u v = toDegrees . acos $ u <.> v / (magnitude u * magnitude v)

-- | True when two vectors are parallel
--   May have trouble with rounding errors
parallel :: (Eq a, Floating a) => Vector a -> Vector a -> Bool
parallel u v = unitVector u == unitVector v

-- | True when two vectors are parallel
perpendicular :: (Eq a, Num a) => Vector a -> Vector a -> Bool
perpendicular u v = u · v == 0

-- ** Dot and cross products

-- | Short form of dotProduct
dot :: (Num a) => Vector a -> Vector a -> a
dot = dotProduct

-- | Dot product of two Vectors
dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct = V.sum .: (*)

-- | Projection of u onto v
projection :: (Floating a) => Vector a -> Vector a -> Vector a
projection u v = (u <.> v / u <.> u) <· u

-- ** Single vector operations

-- | Returns the magnitude / vector norm of a vector
magnitude :: (Floating a) => Vector a -> a
magnitude = sqrt . V.sum . fmap (^2)

-- | returns the unit vector of u
unitVector :: (Floating a) => Vector a -> Vector a
unitVector u = fmap (/ magnitude u) u

-- | Converts a Point to a Vector
pointToVector :: Point a -> Vector a
pointToVector x = x

crossProduct :: Num a => Vector a -> Vector a -> Vector a
crossProduct u v | V.length v /= 3 = error "Cross product is defned for 3D."
                  | otherwise = V.fromList [(u!1 * v!2) - (u!2 * v!1)
                                          ,(u!2 * v!0) - (u!0 * v!2)
                                          ,(u!0 * v!1) - (u!1 * v!0)]
