import Codec.SelfExtract (bundle)
import Codec.SelfExtract.Distribution (getExe)
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postCopy = \args flags pd lbi -> do
      postCopy simpleUserHooks args flags pd lbi
      exe <- getExe lbi "self-extract-cabal"
      bundle exe "dist/"
  }
