--------------------------------------------------------------------------------
{- |
Module      :  Math.Util
Description :  Utility functions for the matrix package.

License     :  GPL-3
Maintainer  :  jackhiggins07@gmail.com
Stability   :  Stable
Portability :  Portable

-}
--------------------------------------------------------------------------------
module Math.Util where

	{- | Function composition composition.  Composes an arity 1 function with an
	arity 2 function.

	> dotProduct = V.sum .: (*)

	Is equivalent to:

	> dotProduct u v = V.sum (u * v)

	-}
	(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
	(.:) = (.) . (.)

	-- * Angle conversions

	-- | Converts Radians to Degrees
	toDegrees :: Floating a => a -> a
	toDegrees rad = rad * (180 / pi)

	-- | Converts Degrees to Radians
	toRadians :: Floating a => a -> a
	toRadians deg = deg * (pi / 180)
