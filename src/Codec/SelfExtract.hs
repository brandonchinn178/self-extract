{-|
Module      :  Codec.SelfExtract
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions that should be used in a self-extractable executable.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Codec.SelfExtract
  ( extractTo
  , withExtractToTemp
  , extractTo'
  , withExtractToTemp'
  ) where

import Control.Monad ((>=>))
import Data.Binary (Word32, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (dummySpaceWith)
import Path (Abs, Dir, Path, fromAbsDir, fromAbsFile, parseAbsFile)
import Path.IO (resolveDir', withSystemTempDir, withSystemTempFile)
import System.Environment (getExecutablePath)
import System.IO (IOMode(..), SeekMode(..), hClose, hSeek, withFile)

import Codec.SelfExtract.Tar (untar)

-- | Extract the self-bundled executable to the given path.
--
-- @
-- extractTo "dir"  -- will extract to $CWD/dir
-- extractTo "\/usr\/local\/lib"
-- @
extractTo :: FilePath -> IO ()
extractTo = resolveDir' >=> extractTo'

-- | Extract the self-bundled executable to a temporary path.
--
-- @
-- withExtractToTemp $ \tmp -> do
--   ...
-- @
withExtractToTemp :: (FilePath -> IO ()) -> IO ()
withExtractToTemp action = withExtractToTemp' (action . fromAbsDir)

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
    untar archive dir

-- | Same as 'withExtractToTemp', except using the 'Path' library.
withExtractToTemp' :: (Path Abs Dir -> IO ()) -> IO ()
withExtractToTemp' action = withSystemTempDir "" $ \dir -> extractTo' dir >> action dir

-- | The size of executable that will be rewritten by `bundle`.
exeSize :: Word32
exeSize = decode $ LBS.fromStrict $(dummySpaceWith "self-extract" 32)
