{-# LANGUAGE LambdaCase #-}

import Codec.SelfExtract (extractTo)
import System.Environment (getArgs)

main :: IO ()
main = do
  dir <- getArgs >>= \case
    [dir] -> return dir
    _ -> fail "Usage: run-example DIR"

  putStrLn $ "Extracting to: " ++ dir
  extractTo dir
  let file = dir ++ "/hello-world.txt"
  putStrLn $ "Contents of " ++ file ++ ":"
  readFile file >>= putStrLn
