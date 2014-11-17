module Vector2D where

import Data.Function
import Math.Vector
import qualified Data.Vector as V
import Data.Vector ((!))

data Slope a = Vertical | Slope a
	deriving (Eq, Show)

fromPair :: (a, a) -> Point a
fromPair (x,y) = V.fromList [x,y]

fromPairs :: Num a => (a, a) -> (a, a) -> Vector a
fromPairs = newVector `on` fromPair

getX :: Vector a -> a
getX = (!0)

getY :: Vector a -> a
getY = (!1)

slope :: (Eq a, Fractional a) => Vector a -> Slope a
slope v = s (getY v) (getX v)
	where s y x | x == 0    = Vertical
	            | otherwise = Slope (y / x)

main :: IO ()
main = do
	print . slope $ fromPairs (0,0) (1,2)
	print . slope $ fromPairs (0,0) (0,4)
