module Math.Util where

	(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
	(.:) = (.) . (.)

	-- * Angle conversions

	-- | Converts Radians to Degrees
	toDegrees :: Floating a => a -> a
	toDegrees rad = rad * (180 / pi)

	-- | Converts Degrees to Radians
	toRadians :: Floating a => a -> a
	toRadians deg = deg * (pi / 180)