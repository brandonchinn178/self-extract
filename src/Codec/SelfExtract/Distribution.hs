{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Codec.SelfExtract.Distribution
  ( bundle
  , bundle'
  ) where

import Data.Binary (Word32, encode)
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.FileEmbed (injectFileWith)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Path (Dir, File, Path, fromAbsFile, parseRelDir, parseRelFile, relfile, toFilePath, (</>))
import Path.IO (renameFile, resolveDir', withSystemTempDir)
import System.PosixCompat.Files (fileSize, getFileStatus)

import Codec.SelfExtract.Tar (tar)

-- | Bundle the given directory into the executable with the given name.
--
-- To be used as part of the Setup.hs file.
bundle :: String -> FilePath -> LocalBuildInfo -> IO ()
bundle exe dir lbi = do
  dir' <- resolveDir' dir
  bundle' exe dir' lbi

-- | Bundle the given directory into the executable with the given name.
--
-- To be used as part of the Setup.hs file.
bundle' :: String -> Path b Dir -> LocalBuildInfo -> IO ()
bundle' exeName dir LocalBuildInfo{buildDir} = do
  exeDir <- resolveDir' buildDir
  exeNameDir <- parseRelDir exeName
  exeNameFile <- parseRelFile exeName
  let exe = exeDir </> exeNameDir </> exeNameFile

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

-- | Get the size of the given file.
getFileSize :: Path b File -> IO Word32
getFileSize = fmap getSize . getFileStatus . toFilePath
  where
    getSize = fromIntegral . fileSize

-- | Concatenate the given files and write to the given file.
cat :: [Path b File] -> Path b File -> IO ()
cat srcs dest = do
  contents <- BS.concat <$> mapM (BS.readFile . toFilePath) srcs
  BS.writeFile (toFilePath dest) contents
