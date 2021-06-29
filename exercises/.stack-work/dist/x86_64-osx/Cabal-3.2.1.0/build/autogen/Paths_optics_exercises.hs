{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
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

bindir     = "/Users/duobei/Coding/haskell/zurihac/optics-zurihac-2021/exercises/.stack-work/install/x86_64-osx/956e8891faf1b110a2dc57a83bd4691e94f47dfe7eef83184f53231f45ddc377/8.10.4/bin"
libdir     = "/Users/duobei/Coding/haskell/zurihac/optics-zurihac-2021/exercises/.stack-work/install/x86_64-osx/956e8891faf1b110a2dc57a83bd4691e94f47dfe7eef83184f53231f45ddc377/8.10.4/lib/x86_64-osx-ghc-8.10.4/optics-exercises-0.1.0.0-Ch7EATUmHRRIJ0CXQbvkYi"
dynlibdir  = "/Users/duobei/Coding/haskell/zurihac/optics-zurihac-2021/exercises/.stack-work/install/x86_64-osx/956e8891faf1b110a2dc57a83bd4691e94f47dfe7eef83184f53231f45ddc377/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/duobei/Coding/haskell/zurihac/optics-zurihac-2021/exercises/.stack-work/install/x86_64-osx/956e8891faf1b110a2dc57a83bd4691e94f47dfe7eef83184f53231f45ddc377/8.10.4/share/x86_64-osx-ghc-8.10.4/optics-exercises-0.1.0.0"
libexecdir = "/Users/duobei/Coding/haskell/zurihac/optics-zurihac-2021/exercises/.stack-work/install/x86_64-osx/956e8891faf1b110a2dc57a83bd4691e94f47dfe7eef83184f53231f45ddc377/8.10.4/libexec/x86_64-osx-ghc-8.10.4/optics-exercises-0.1.0.0"
sysconfdir = "/Users/duobei/Coding/haskell/zurihac/optics-zurihac-2021/exercises/.stack-work/install/x86_64-osx/956e8891faf1b110a2dc57a83bd4691e94f47dfe7eef83184f53231f45ddc377/8.10.4/etc"

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
