import System.IO (hFlush, stdout)

import Codec.SelfExtract (extractTo)

main :: IO ()
main = do
  putStrLn "Enter the directory to extract contents:"
  hFlush stdout
  target <- getLine
  extractTo target
  contents <- readFile $ target ++ "/hello-world.txt"
  putStrLn "Contents of hello-world.txt: "
  putStrLn contents
