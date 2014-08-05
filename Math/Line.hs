module Math.Line where

import Data.List
import Math.Vector
import Math.Util
import qualified Data.Vector as V

{- | Line in Vector form.  A line is a geometrical object in Rⁿ that contains a
	point P and all points that lie in one direction and its inverse direction 
	from point P.

	Every line l in Rⁿ can be parametrized by a vector equation of the form:

		>l: v➝(t) = v₀➝ + t.d➝	for t ∈ R.

	where v₀➝ corresponds to some point on the line and d➝ gives the direction 
	of the line.
-}
data Line a = Line { point :: Point a 		-- ^ Any point on the line
				   , vector :: Vector a	-- ^ Direction vector
				  } deriving (Eq)

instance Show a => Show (Line a) where
	show l = concat ["Line (", intercalate "," . map show . V.toList $ point l
		, ") + t(", intercalate "," . map show . V.toList $ vector l, ")"]

-- | Creates a line from a point and a Vector
line :: Point a -> Vector a -> Line a 
line p v = Line p v

-- | Distance between two points
distance :: (Num a, Floating a) => Point a -> Point a -> a
distance = (sqrt . V.sum . fmap (^2)) .: (-)

--Needs Testing
-- | Gets the perpendicular distance from a point to a line.
distPointToLine :: (Num a, Floating a) => Point a -> Line a -> a
distPointToLine p l = abs (p0p `dot` d) / (magnitude d)
	where p0p = newVector p (point l)
	      d   = vector l

-- | True if two lines are parallel.
parallelLine :: (Eq a, Floating a) => Line a -> Line a -> Bool
parallelLine a b = (vector a) -|| (vector b)

-- | True if two lines are perpendicular
perpendicularLine :: (Eq a, Num a) => Line a -> Line a -> Bool
perpendicularLine a b = (vector a) -| (vector b)