module Math.Vector where

	import qualified Data.Vector as V

	type Point = V.Vector
	type Vector = V.Vector

	instance Num a => Num (V.Vector a) where
		(+) = V.zipWith (+)
		(*) = V.zipWith (*)
		abs = fmap abs
		negate      = fmap negate
		signum      = undefined
		fromInteger = undefined

	(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
	(.:) = (.) . (.)

	(<.>) :: (Num a) => Vector a -> Vector a -> a
	(<.>) = dotProduct

	-- * Scalar multiplication
	(*>) :: (Functor f, Num a) => f a -> a -> f a
	(<*) :: (Functor f, Num a) => a -> f a -> f a
	vector *> scalar = fmap (*scalar) vector
	scalar <* vector = vector *> scalar

	angle :: (Floating a) => Vector a -> Vector a -> a
	angle u v = toDegrees . acos $ u <.> v / (magnitude u * magnitude v)

	dot :: (Num a) => Vector a -> Vector a -> a
	dot = dotProduct

	dotProduct :: (Num a) => Vector a -> Vector a -> a
	dotProduct = V.sum .: (*)

	magnitude :: (Floating a) => Vector a -> a
	magnitude = sqrt . V.sum . fmap (^2)

	-- @param u v	- Vectors
	-- @return 		- True if vectors are parallel
	-- 				- May have trouble with rounding errors
	parallel :: (Eq a, Floating a) => Vector a -> Vector a -> Bool
	parallel u v = unitVector u == unitVector v

	-- @param u v	- Vectors
	-- @return 		- True if vectors are perpendicular
	perpendicular :: (Eq a, Num a) => Vector a -> Vector a -> Bool
	perpendicular u v = u <.> v == 0

	pointToVector :: Point a -> Vector a
	pointToVector x = x  

	-- @param u v	- Vectors
	-- @return 		- Projection u onto v
	projection :: (Floating a) => Vector a -> Vector a -> Vector a
	projection u v = (u <.> v / u <.> u) <* u

	toDegrees :: Floating a => a -> a
	toDegrees rad = rad * (180 / pi)

	toRadians :: Floating a => a -> a
	toRadians deg = deg * (pi / 180)

	-- @return 		- returns the unit vector of u
	unitVector :: (Floating a) => Vector a -> Vector a
	unitVector u = fmap (/ magnitude u) u

	-- @param a b	- Point
	-- @return 		- Vector a relative to b
	vector :: (Num a) => Point a -> Point a -> Vector a
	vector a b = pointToVector $ b - a