{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_interpreter (
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
version = Version [0,3,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Ling/MyPrograms/Python_Interpreter/.stack-work/install/x86_64-osx/8b162e55ba3b7dc9603c2a4142e2e8a5101e98feea663950da63afda738436bc/8.10.4/bin"
libdir     = "/Users/Ling/MyPrograms/Python_Interpreter/.stack-work/install/x86_64-osx/8b162e55ba3b7dc9603c2a4142e2e8a5101e98feea663950da63afda738436bc/8.10.4/lib/x86_64-osx-ghc-8.10.4/interpreter-0.3.0.0-HHenCTbLemN1wuJRJ6m8cd-friendly-test"
dynlibdir  = "/Users/Ling/MyPrograms/Python_Interpreter/.stack-work/install/x86_64-osx/8b162e55ba3b7dc9603c2a4142e2e8a5101e98feea663950da63afda738436bc/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/Ling/MyPrograms/Python_Interpreter/.stack-work/install/x86_64-osx/8b162e55ba3b7dc9603c2a4142e2e8a5101e98feea663950da63afda738436bc/8.10.4/share/x86_64-osx-ghc-8.10.4/interpreter-0.3.0.0"
libexecdir = "/Users/Ling/MyPrograms/Python_Interpreter/.stack-work/install/x86_64-osx/8b162e55ba3b7dc9603c2a4142e2e8a5101e98feea663950da63afda738436bc/8.10.4/libexec/x86_64-osx-ghc-8.10.4/interpreter-0.3.0.0"
sysconfdir = "/Users/Ling/MyPrograms/Python_Interpreter/.stack-work/install/x86_64-osx/8b162e55ba3b7dc9603c2a4142e2e8a5101e98feea663950da63afda738436bc/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "interpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "interpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
