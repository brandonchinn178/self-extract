module Codec.SelfExtract.Tar
  ( tar
  , untar
  ) where

import Path (Dir, File, Path, toFilePath)
import Path.IO (ensureDir)
import System.Process (callProcess)

-- | Zip the given directory into the given archive.
--
-- Shelling out to `tar` because Haskell Tar packages would need the C header files, which we don't
-- require on the client end.
tar :: Path b0 Dir -> Path b1 File -> IO ()
tar src archive = callProcess "tar" ["-czf", toFilePath archive, "-C", toFilePath src, "."]

-- | Extract the given archive to the given directory.
untar :: Path b0 File -> Path b1 Dir -> IO ()
untar archive dest = do
  ensureDir dest
  callProcess "tar" ["-xzf", toFilePath archive, "-C", toFilePath dest]
