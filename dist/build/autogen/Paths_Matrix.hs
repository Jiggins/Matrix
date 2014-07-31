module Paths_Matrix (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,2,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Jack/Library/Haskell/ghc-7.6.3/lib/Matrix-0.1.2.0/bin"
libdir     = "/Users/Jack/Library/Haskell/ghc-7.6.3/lib/Matrix-0.1.2.0/lib"
datadir    = "/Users/Jack/Library/Haskell/ghc-7.6.3/lib/Matrix-0.1.2.0/share"
libexecdir = "/Users/Jack/Library/Haskell/ghc-7.6.3/lib/Matrix-0.1.2.0/libexec"
sysconfdir = "/Users/Jack/Library/Haskell/ghc-7.6.3/lib/Matrix-0.1.2.0/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Matrix_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Matrix_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Matrix_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Matrix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Matrix_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
