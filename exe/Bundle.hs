{-# LANGUAGE LambdaCase #-}

import Codec.SelfExtract (bundle)
import System.Environment (getArgs)

-- | Given an executable and a directory, bundle the directory into the executable to be extracted
-- when the executable is run.
--
-- The executable should import `extractTo` from `Codec.SelfExtract` to extract the directory
-- bundled with it.
main :: IO ()
main = getArgs >>= \case
  [exe, dir] -> bundle exe dir
  _ -> fail "Usage: self-bundle EXE DIR"
