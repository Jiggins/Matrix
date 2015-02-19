{-#LANGUAGE OverloadedStrings#-}
module Math.Matrix.IO where

--import Data.List
import Data.Text hiding (map)
import Data.Text.IO
import Math.Matrix
import Prelude hiding (lines, words, putStrLn, getContents, getLine, readFile)
import System.Environment
import System.IO (openFile, IOMode (ReadMode))

-- | Reads from a file
-- Returns single Text of contents
open :: FilePath -> IO Text
open = readFile

readCSV :: FilePath -> IO [[Text]]
readCSV file = open file >>= return . map (splitOn ",") . lines

-- | Converts a Matrix to a list of comma seperated lines.
toCSV :: Show a => Matrix a -> [Text]
toCSV = map (intercalate ",") . toLists . fmap (pack . show)

-- | Prints a matrix in CSV format.
printCSV :: Show a => Matrix a -> IO ()
printCSV = mapM_ putStrLn . toCSV
