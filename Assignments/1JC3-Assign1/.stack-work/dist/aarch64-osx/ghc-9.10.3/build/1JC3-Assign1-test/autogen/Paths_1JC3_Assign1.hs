{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_1JC3_Assign1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/aaronsit/Developer/1JC3/Assignments/1JC3-Assign1/.stack-work/install/aarch64-osx/e5b34ae13a213da6551e932399196b2f349902ce340d8ee0b60351bb5a27c04c/9.10.3/bin"
libdir     = "/Users/aaronsit/Developer/1JC3/Assignments/1JC3-Assign1/.stack-work/install/aarch64-osx/e5b34ae13a213da6551e932399196b2f349902ce340d8ee0b60351bb5a27c04c/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c/1JC3-Assign1-0.1.0.0-Iy8eU4qilDl62WZRtcbxT2-1JC3-Assign1-test"
dynlibdir  = "/Users/aaronsit/Developer/1JC3/Assignments/1JC3-Assign1/.stack-work/install/aarch64-osx/e5b34ae13a213da6551e932399196b2f349902ce340d8ee0b60351bb5a27c04c/9.10.3/lib/aarch64-osx-ghc-9.10.3-fe9c"
datadir    = "/Users/aaronsit/Developer/1JC3/Assignments/1JC3-Assign1/.stack-work/install/aarch64-osx/e5b34ae13a213da6551e932399196b2f349902ce340d8ee0b60351bb5a27c04c/9.10.3/share/aarch64-osx-ghc-9.10.3-fe9c/1JC3-Assign1-0.1.0.0"
libexecdir = "/Users/aaronsit/Developer/1JC3/Assignments/1JC3-Assign1/.stack-work/install/aarch64-osx/e5b34ae13a213da6551e932399196b2f349902ce340d8ee0b60351bb5a27c04c/9.10.3/libexec/aarch64-osx-ghc-9.10.3-fe9c/1JC3-Assign1-0.1.0.0"
sysconfdir = "/Users/aaronsit/Developer/1JC3/Assignments/1JC3-Assign1/.stack-work/install/aarch64-osx/e5b34ae13a213da6551e932399196b2f349902ce340d8ee0b60351bb5a27c04c/9.10.3/etc"

getBinDir     = catchIO (getEnv "1JC3_Assign1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "1JC3_Assign1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "1JC3_Assign1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "1JC3_Assign1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1JC3_Assign1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1JC3_Assign1_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
