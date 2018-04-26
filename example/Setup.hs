import Codec.SelfExtract.Distribution (bundle)
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postCopy = \args cf pd lbi -> do
      postCopy simpleUserHooks args cf pd lbi
      bundle "foo" "dist" lbi
  }
