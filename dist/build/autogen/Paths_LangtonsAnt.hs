module Paths_LangtonsAnt (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Mac/Desktop/comp1100-2016-assignment1/.cabal-sandbox/bin"
libdir     = "/Users/Mac/Desktop/comp1100-2016-assignment1/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.3/LangtonsAnt-0.1.0.0-ExOeCtg8ofZLCGq9n9LxQj"
datadir    = "/Users/Mac/Desktop/comp1100-2016-assignment1/.cabal-sandbox/share/x86_64-osx-ghc-7.10.3/LangtonsAnt-0.1.0.0"
libexecdir = "/Users/Mac/Desktop/comp1100-2016-assignment1/.cabal-sandbox/libexec"
sysconfdir = "/Users/Mac/Desktop/comp1100-2016-assignment1/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LangtonsAnt_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LangtonsAnt_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "LangtonsAnt_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LangtonsAnt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LangtonsAnt_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
