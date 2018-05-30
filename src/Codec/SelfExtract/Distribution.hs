{-|
Module      :  Codec.SelfExtract.Distribution
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines functions that should be used in the @Setup.hs@ file.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Codec.SelfExtract.Distribution
  ( bundle
  , bundle'
  ) where

import Control.Monad.Extra (unlessM)
import Data.Binary (Word32, encode)
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.FileEmbed (injectFileWith)
import Distribution.Simple.LocalBuildInfo (InstallDirs(..), LocalBuildInfo(..), fromPathTemplate)
import Distribution.Simple.Setup (ConfigFlags(..), fromFlag)
import Path
    ( Abs
    , Dir
    , File
    , Path
    , fromAbsFile
    , parseAbsDir
    , parseRelFile
    , relfile
    , toFilePath
    , (</>)
    )
import Path.IO (doesFileExist, renameFile, resolveDir', withSystemTempDir)
import qualified System.PosixCompat.Files as Posix

import Codec.SelfExtract.Tar (tar)

-- | Bundle the given directory into the executable with the given name.
--
-- For example, to bundle the @static/@ directory in the executable named @install-files@:
--
-- @
-- main = defaultMainWithHooks simpleUserHooks
--   { postCopy = \args cf pd lbi -> do
--       postCopy simpleUserHooks args cf pd lbi
--       bundle "install-files" ".\/static\/" lbi
--   }
-- @
bundle :: String -> FilePath -> LocalBuildInfo -> IO ()
bundle exe dir lbi = do
  dir' <- resolveDir' dir
  bundle' exe dir' lbi

-- | Same as 'bundle', except using the 'Path' library.
--
-- @
-- main = defaultMainWithHooks simpleUserHooks
--   { postCopy = \args cf pd lbi -> do
--       postCopy simpleUserHooks args cf pd lbi
--       bundle' "install-files" [reldir|.\/static\/|] lbi
--   }
-- @
bundle' :: String -> Path b Dir -> LocalBuildInfo -> IO ()
bundle' exeName dir lbi = do
  exe <- getExe lbi exeName
  unlessM (doesFileExist exe) $ error $ "Executable does not exist: " ++ exeName

  size <- getFileSize exe

  withSystemTempDir "self-extract" $ \tempDir -> do
    let exeWithSize = tempDir </> [relfile|exe_with_size|]
    injectFileWith "self-extract"
      (LBS.toStrict $ encode size)
      (fromAbsFile exe)
      (fromAbsFile exeWithSize)

    let zippedDir = tempDir </> [relfile|bundle.tar.gz|]
    tar dir zippedDir

    let combined = tempDir </> [relfile|exe_and_bundle|]
    cat [exeWithSize, zippedDir] combined

    renameFile combined exe

  Posix.setFileMode (fromAbsFile exe) executeMode
  where
    -- 755 permissions
    executeMode = Posix.unionFileModes Posix.stdFileMode Posix.ownerExecuteMode

-- | Get the executable to be made self-extracting.
getExe :: LocalBuildInfo -> String -> IO (Path Abs File)
getExe LocalBuildInfo{configFlags} exeName = do
  binDir <- parseAbsDir $ fromPathTemplate $ fromFlag $ bindir $ configInstallDirs configFlags
  exe <- parseRelFile exeName
  return $ binDir </> exe

-- | Get the size of the given file.
getFileSize :: Path b File -> IO Word32
getFileSize = fmap getSize . Posix.getFileStatus . toFilePath
  where
    getSize = fromIntegral . Posix.fileSize

-- | Concatenate the given files and write to the given file.
cat :: [Path b File] -> Path b File -> IO ()
cat srcs dest = do
  contents <- BS.concat <$> mapM (BS.readFile . toFilePath) srcs
  BS.writeFile (toFilePath dest) contents
