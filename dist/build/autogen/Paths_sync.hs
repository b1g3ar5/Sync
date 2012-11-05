module Paths_sync (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "c:/Documents and Settings/Robin Seeley/workspace/bin\\bin"
libdir     = "c:/Documents and Settings/Robin Seeley/workspace/bin\\sync-0.0\\ghc-7.4.1"
datadir    = "c:/Documents and Settings/Robin Seeley/workspace/bin\\sync-0.0"
libexecdir = "c:/Documents and Settings/Robin Seeley/workspace/bin\\sync-0.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "sync_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sync_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "sync_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sync_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
