import Codec.SelfExtract.Distribution (bundle)
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postCopy = \args flags pd lbi -> do
      postCopy simpleUserHooks args flags pd lbi
      bundle "self-extract-basic" "dist" lbi
  }
