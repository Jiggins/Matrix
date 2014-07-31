module Math.Line where

import Data.List
import Math.Vector
import Math.Util
import qualified Data.Vector as V

data Line a = Line { point :: Point a 		-- Any point on the line
				   , getVector :: Vector a	-- Direction vector
				  } deriving (Eq)

instance Show a => Show (Line a) where
	show l = concat ["Line (", intercalate "," . map show . V.toList $ point l
		, ") + t(", intercalate "," . map show . V.toList $ getVector l, ")"]

line :: Point a -> Vector a -> Line a 
line p v = Line p v

distance :: (Num a, Floating a) => Point a -> Point a -> a
distance = (sqrt . V.sum . fmap (^2)) .: (-)

--Needs Testing
-- | Gets the perpendicular distance from a point to a line.
distPointToLine :: (Num a, Floating a) => Point a -> Line a -> a
distPointToLine p l = abs (p0p `dot` d) / (magnitude d)
	where p0p = vector p (point l)
	      d   = getVector l

-- | True if two lines are parallel.
parallelLine :: (Eq a, Floating a) => Line a -> Line a -> Bool
parallelLine a b = (getVector a) -|| (getVector b)

-- | True if two lines are perpendicular
perpendicularLine :: (Eq a, Num a) => Line a -> Line a -> Bool
perpendicularLine a b = (getVector a) -| (getVector b)