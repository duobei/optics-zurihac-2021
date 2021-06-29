{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_optics_exercises (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/duobei/.cabal/bin"
libdir     = "/Users/duobei/.cabal/lib/x86_64-osx-ghc-8.10.4/optics-exercises-0.1.0.0-inplace"
dynlibdir  = "/Users/duobei/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/duobei/.cabal/share/x86_64-osx-ghc-8.10.4/optics-exercises-0.1.0.0"
libexecdir = "/Users/duobei/.cabal/libexec/x86_64-osx-ghc-8.10.4/optics-exercises-0.1.0.0"
sysconfdir = "/Users/duobei/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "optics_exercises_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "optics_exercises_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "optics_exercises_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "optics_exercises_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "optics_exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "optics_exercises_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
