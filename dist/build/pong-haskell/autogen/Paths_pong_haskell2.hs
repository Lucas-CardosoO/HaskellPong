{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pong_haskell2 (
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

bindir     = "/Users/lucascardoso/.cabal/bin"
libdir     = "/Users/lucascardoso/.cabal/lib/x86_64-osx-ghc-8.6.3/pong-haskell2-0.1.0.0-L2pg9Wr78DR5V5U6NxLq0R"
dynlibdir  = "/Users/lucascardoso/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/lucascardoso/.cabal/share/x86_64-osx-ghc-8.6.3/pong-haskell2-0.1.0.0"
libexecdir = "/Users/lucascardoso/.cabal/libexec/x86_64-osx-ghc-8.6.3/pong-haskell2-0.1.0.0"
sysconfdir = "/Users/lucascardoso/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pong_haskell2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pong_haskell2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pong_haskell2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pong_haskell2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pong_haskell2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pong_haskell2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
