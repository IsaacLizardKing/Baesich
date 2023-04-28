{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Baesich (
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
version = Version [1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/scrumble/Documents/2023-1 Spring/ProgLang/project_4/Baesich/.stack-work/install/x86_64-linux/29d23d8a95681cb5cf793cf26989b280653ba28ffc2892bcdfffe26238670b66/9.2.6/bin"
libdir     = "/home/scrumble/Documents/2023-1 Spring/ProgLang/project_4/Baesich/.stack-work/install/x86_64-linux/29d23d8a95681cb5cf793cf26989b280653ba28ffc2892bcdfffe26238670b66/9.2.6/lib/x86_64-linux-ghc-9.2.6/Baesich-1.0-1UysXSiRHFk42vYcSzB8JZ-Baesich"
dynlibdir  = "/home/scrumble/Documents/2023-1 Spring/ProgLang/project_4/Baesich/.stack-work/install/x86_64-linux/29d23d8a95681cb5cf793cf26989b280653ba28ffc2892bcdfffe26238670b66/9.2.6/lib/x86_64-linux-ghc-9.2.6"
datadir    = "/home/scrumble/Documents/2023-1 Spring/ProgLang/project_4/Baesich/.stack-work/install/x86_64-linux/29d23d8a95681cb5cf793cf26989b280653ba28ffc2892bcdfffe26238670b66/9.2.6/share/x86_64-linux-ghc-9.2.6/Baesich-1.0"
libexecdir = "/home/scrumble/Documents/2023-1 Spring/ProgLang/project_4/Baesich/.stack-work/install/x86_64-linux/29d23d8a95681cb5cf793cf26989b280653ba28ffc2892bcdfffe26238670b66/9.2.6/libexec/x86_64-linux-ghc-9.2.6/Baesich-1.0"
sysconfdir = "/home/scrumble/Documents/2023-1 Spring/ProgLang/project_4/Baesich/.stack-work/install/x86_64-linux/29d23d8a95681cb5cf793cf26989b280653ba28ffc2892bcdfffe26238670b66/9.2.6/etc"

getBinDir     = catchIO (getEnv "Baesich_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Baesich_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Baesich_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Baesich_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Baesich_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Baesich_sysconfdir") (\_ -> return sysconfdir)




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
