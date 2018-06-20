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
  ( getExe
  , getExe'
  ) where

import Distribution.Simple.LocalBuildInfo (InstallDirs(..), LocalBuildInfo(..), fromPathTemplate)
import Distribution.Simple.Setup (ConfigFlags(..), fromFlag)
import Path (Abs, File, Path, parseAbsDir, parseRelFile, toFilePath, (</>))

-- | Get the executable with the given name with the given LocalBuildInfo.
--
-- @
-- main = defaultMainWithHooks simpleUserHooks
--   { postCopy = \args cf pd lbi -> do
--       postCopy simpleUserHooks args cf pd lbi
--       exe <- getExe lbi "name-of-executable"
--       bundle exe ".\/static\/"
--   }
-- @
getExe :: LocalBuildInfo -> String -> IO FilePath
getExe lbi exeName = toFilePath <$> getExe' lbi exeName

-- | Same as 'getExe', except using the 'Path' library.
--
-- @
-- main = defaultMainWithHooks simpleUserHooks
--   { postCopy = \args cf pd lbi -> do
--       postCopy simpleUserHooks args cf pd lbi
--       exe <- getExe' lbi "name-of-executable"
--       bundle' exe [reldir|.\/static\/|]
--   }
-- @
getExe' :: LocalBuildInfo -> String -> IO (Path Abs File)
getExe' LocalBuildInfo{configFlags} exeName = do
  binDir <- parseAbsDir $ fromPathTemplate $ fromFlag $ bindir $ configInstallDirs configFlags
  exe <- parseRelFile exeName
  return $ binDir </> exe
