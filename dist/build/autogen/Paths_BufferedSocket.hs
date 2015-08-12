module Paths_BufferedSocket (
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
version = Version [0,2,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tomasmore/Desktop/Haskell fun/BufferedSocket/.cabal-sandbox/bin"
libdir     = "/Users/tomasmore/Desktop/Haskell fun/BufferedSocket/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/BufferedSocket-0.2.1.0"
datadir    = "/Users/tomasmore/Desktop/Haskell fun/BufferedSocket/.cabal-sandbox/share/x86_64-osx-ghc-7.10.2/BufferedSocket-0.2.1.0"
libexecdir = "/Users/tomasmore/Desktop/Haskell fun/BufferedSocket/.cabal-sandbox/libexec"
sysconfdir = "/Users/tomasmore/Desktop/Haskell fun/BufferedSocket/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BufferedSocket_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BufferedSocket_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "BufferedSocket_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BufferedSocket_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BufferedSocket_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
