{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_brick (
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
version = Version [1,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shreyachoure/.cabal/store/ghc-9.2.7/brck-1.9-60bb6f23/bin"
libdir     = "/Users/shreyachoure/.cabal/store/ghc-9.2.7/brck-1.9-60bb6f23/lib"
dynlibdir  = "/Users/shreyachoure/.cabal/store/ghc-9.2.7/lib"
datadir    = "/Users/shreyachoure/.cabal/store/ghc-9.2.7/brck-1.9-60bb6f23/share"
libexecdir = "/Users/shreyachoure/.cabal/store/ghc-9.2.7/brck-1.9-60bb6f23/libexec"
sysconfdir = "/Users/shreyachoure/.cabal/store/ghc-9.2.7/brck-1.9-60bb6f23/etc"

getBinDir     = catchIO (getEnv "brick_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "brick_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "brick_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "brick_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "brick_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "brick_sysconfdir") (\_ -> return sysconfdir)




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
