# self-extract

A Haskell library that can make an executable self-extracting.

## Usage

### Basic

```
-- Build.hs
import Codec.SelfExtract (bundle)

main :: IO ()
main = bundle "./Foo" "./artifacts"

-- Foo.hs
import Codec.SelfExtract (extractTo)
import System.Environment (getArgs)

main :: IO ()
main = do
  dir <- head <$> getArgs
  extractTo dir
```

```
$ stack ghc Foo.hs
$ mkdir artifacts && touch artifacts/foo.txt artifacts/bar.txt
$ stack Build.hs
$ ./Foo hello
$ ls hello
foo.txt
bar.txt
```

### With Cabal hooks

* Add `self-extract` to the Cabal file

```
custom-setup
  setup-depends: base, Cabal, self-extract

executable name-of-executable
  build-depends: self-extract
```

* Call `bundle` in `Setup.hs`

```
import Codec.SelfExtract (bundle)
import Codec.SelfExtract.Distribution (getExe)
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postCopy = \args cf pd lbi -> do
      postCopy simpleUserHooks args cf pd lbi
      exe <- getExe lbi "name-of-executable"
      bundle exe "dir-to-bundle"
  }
```

* Call `extractTo` in the executable

```
import Codec.SelfExtract

main = do
  -- will extract to $CWD/dir
  extractTo "dir"

  -- will extract to /usr/local/lib
  extractTo "/usr/local/lib"

  -- will extract to a temporary directory
  withExtractToTemp $ \dir -> ...
```

### Details

The above instructions should be a black box, but here is an explanation of the implementation
if you need to know the details of how it works.

When the executable containing `extractTo` is built, some space will be allocated to contain the
size of the binary.

`bundle` will take the directory specified and run `tar` on it. It will also get the size of the
given executable and write the size into the space allocated by `extractTo`. Then `bundle` will
replace the executable with the executable itself concatenated with the tar archive.

When `extractTo` is called, it will read the size of the executable that was written with `bundle`.
After seeking to the size of the binary, the tar archive can be extracted to the desired directory.
