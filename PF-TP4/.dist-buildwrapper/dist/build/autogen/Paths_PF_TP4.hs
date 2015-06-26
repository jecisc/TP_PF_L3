module Paths_PF_TP4 (
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
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Cyril\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Cyril\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\PF-TP4-0.1"
datadir    = "C:\\Users\\Cyril\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\PF-TP4-0.1"
libexecdir = "C:\\Users\\Cyril\\AppData\\Roaming\\cabal\\PF-TP4-0.1"
sysconfdir = "C:\\Users\\Cyril\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PF_TP4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PF_TP4_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PF_TP4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PF_TP4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PF_TP4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
