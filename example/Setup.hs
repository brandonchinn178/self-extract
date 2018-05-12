import Codec.SelfExtract.Distribution (bundle)
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postBuild = \args bf pd lbi -> do
      postBuild simpleUserHooks args bf pd lbi
      bundle "foo" "dist" lbi
  }
