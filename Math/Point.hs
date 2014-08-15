--------------------------------------------------------------------------------
{- |
Module      :  Math.Line
Description :  A Haskell implementation of Lines in Vector form

License     :  GPL-3
Maintainer  :  jackhiggins07@gmail.com
Stability   :  Stable
Portability :  Portable

This class defines functions to operate on pomts  
-}
--------------------------------------------------------------------------------
module Math.Point where

import Math.Util
import Math.Vector 
import qualified Data.Vector as V

-- | Distance between two points
distance :: (Num a, Floating a) => Point a -> Point a -> a
distance = (sqrt . V.sum . fmap (^2)) .: (-)

-- | Midpoint of two points
midpoint :: Floating a => Point a -> Point a -> Point a
midpoint = V.map (/ 2) .: V.zipWith (+)