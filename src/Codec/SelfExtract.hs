{-|
Module      :  Codec.SelfExtract
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

Defines functions that should be used in a self-extractable executable.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Codec.SelfExtract
  ( extractTo
  , withExtractToTemp
  , bundle
  , extractTo'
  , withExtractToTemp'
  , bundle'
  ) where

import Codec.Archive.ZTar (Compression(..), create, extract)
import Control.Monad ((>=>))
import Control.Monad.Extra (unlessM)
import Data.Binary (Word32, decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (dummySpaceWith, injectFileWith)
import Path
    ( Abs
    , Dir
    , File
    , Path
    , fromAbsDir
    , fromAbsFile
    , parent
    , parseAbsFile
    , relfile
    , toFilePath
    , (</>)
    )
import Path.IO
    ( doesFileExist
    , renameFile
    , resolveDir'
    , resolveFile'
    , withSystemTempDir
    , withSystemTempFile
    , withTempDir
    )
import System.Environment (getExecutablePath)
import System.IO (IOMode(..), SeekMode(..), hClose, hSeek, withFile)
import qualified System.PosixCompat.Files as Posix

{- With FilePaths -}

-- | Extract the self-bundled executable to the given path.
--
-- @
-- extractTo "dir"  -- will extract to $CWD/dir
-- extractTo "\/usr\/local\/lib"
-- @
extractTo :: FilePath -> IO ()
extractTo = resolveDir' >=> extractTo'

-- | Extract the self-bundled executable to a temporary path.
withExtractToTemp :: (FilePath -> IO ()) -> IO ()
withExtractToTemp action = withExtractToTemp' (action . fromAbsDir)

-- | Bundle the given directory into the executable with the given name.
--
-- For example, to bundle the @static/@ directory in the executable named @install-files@:
--
-- @
-- bundle "./install-files" ".\/static\/"
-- @
bundle :: FilePath -> FilePath -> IO ()
bundle exe dir = do
  exe' <- resolveFile' exe
  dir' <- resolveDir' dir
  bundle' exe' dir'

{- With Paths -}

-- | Same as 'extractTo', except using the 'Path' library.
--
-- @
-- extractTo' [reldir|dir|]  -- will extract to $CWD/dir
-- extractTo' [absdir|\/usr\/local\/lib|]
-- @
extractTo' :: Path b Dir -> IO ()
extractTo' dir = do
  self <- getExecutablePath >>= parseAbsFile
  withSystemTempFile "" $ \archive hTemp -> do
    withFile (fromAbsFile self) ReadMode $ \hSelf -> do
      hSeek hSelf AbsoluteSeek $ fromIntegral exeSize
      BS.hGetContents hSelf >>= BS.hPut hTemp

    hClose hTemp
    extract (toFilePath dir) $ fromAbsFile archive

-- | Same as 'withExtractToTemp', except using the 'Path' library.
withExtractToTemp' :: (Path Abs Dir -> IO ()) -> IO ()
withExtractToTemp' action = withSystemTempDir "" $ \dir -> extractTo' dir >> action dir

-- | Same as 'bundle', except using the 'Path' library.
--
-- @
-- bundle' [relfile|install-files|] [reldir|static|]
-- @
bundle' :: Path b File -> Path b Dir -> IO ()
bundle' exe dir = do
  unlessM (doesFileExist exe) $ error $ "Executable does not exist: " ++ toFilePath exe

  size <- getFileSize exe

  withTempDir (parent exe) "self-extract" $ \tempDir -> do
    let exeWithSize = tempDir </> [relfile|exe_with_size|]
    injectFileWith "self-extract"
      (LBS.toStrict $ encode size)
      (toFilePath exe)
      (fromAbsFile exeWithSize)

    let archive = tempDir </> [relfile|bundle.tar.gz|]
    create GZip (fromAbsFile archive) $ toFilePath dir

    let combined = tempDir </> [relfile|exe_and_bundle|]
    cat [exeWithSize, archive] combined

    renameFile combined exe

  Posix.setFileMode (toFilePath exe) executeMode
  where
    -- 755 permissions
    executeMode = Posix.unionFileModes Posix.stdFileMode Posix.ownerExecuteMode

{- Helpers -}

-- | The size of executable that will be rewritten by `bundle`.
exeSize :: Word32
exeSize = decode $ LBS.fromStrict $(dummySpaceWith "self-extract" 32)

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
