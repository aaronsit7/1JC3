{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_HaskellExercises02 (
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
bindir     = "/Users/aaronsit/Developer/1JC3/Exercises/HaskellExercises02/.stack-work/install/aarch64-osx/62323f3c617815fdb3b2faeaa0b97beff1f4885951a26ea776f84b6830bacc1e/9.8.4/bin"
libdir     = "/Users/aaronsit/Developer/1JC3/Exercises/HaskellExercises02/.stack-work/install/aarch64-osx/62323f3c617815fdb3b2faeaa0b97beff1f4885951a26ea776f84b6830bacc1e/9.8.4/lib/aarch64-osx-ghc-9.8.4/HaskellExercises02-0.1.0.0-2j3kejJOuux7Y7lsfrIxvI"
dynlibdir  = "/Users/aaronsit/Developer/1JC3/Exercises/HaskellExercises02/.stack-work/install/aarch64-osx/62323f3c617815fdb3b2faeaa0b97beff1f4885951a26ea776f84b6830bacc1e/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/aaronsit/Developer/1JC3/Exercises/HaskellExercises02/.stack-work/install/aarch64-osx/62323f3c617815fdb3b2faeaa0b97beff1f4885951a26ea776f84b6830bacc1e/9.8.4/share/aarch64-osx-ghc-9.8.4/HaskellExercises02-0.1.0.0"
libexecdir = "/Users/aaronsit/Developer/1JC3/Exercises/HaskellExercises02/.stack-work/install/aarch64-osx/62323f3c617815fdb3b2faeaa0b97beff1f4885951a26ea776f84b6830bacc1e/9.8.4/libexec/aarch64-osx-ghc-9.8.4/HaskellExercises02-0.1.0.0"
sysconfdir = "/Users/aaronsit/Developer/1JC3/Exercises/HaskellExercises02/.stack-work/install/aarch64-osx/62323f3c617815fdb3b2faeaa0b97beff1f4885951a26ea776f84b6830bacc1e/9.8.4/etc"

getBinDir     = catchIO (getEnv "HaskellExercises02_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "HaskellExercises02_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "HaskellExercises02_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "HaskellExercises02_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellExercises02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellExercises02_sysconfdir") (\_ -> return sysconfdir)



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
