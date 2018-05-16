# self-extract

A Haskell library that can make an executable self-extracting.

## Usage

### .cabal file

```
...
build-type: Custom
...

custom-setup
  setup-depends: base, Cabal, self-extract

...

executable name-of-executable
  build-depends: self-extract, ...
  ...
```

### Setup.hs

Basic:

```
import Codec.SelfExtract.Distribution (bundle)
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postCopy = \args cf pd lbi -> do
      postCopy simpleUserHooks args cf pd lbi
      bundle "name-of-executable" "dir-to-bundle" lbi
  }
```

Using the [Path](https://hackage.haskell.org/package/path-0.6.1) library:

```
import Codec.SelfExtract.Distribution (bundle')
import Distribution.Simple
import Path (reldir)

main = defaultMainWithHooks simpleUserHooks
  { postCopy = \args cf pd lbi -> do
      postCopy simpleUserHooks args cf pd lbi
      bundle' "name-of-executable" [reldir|dir-to-bundle|] lbi
  }
```

### Executable file

Basic:

```
import Codec.SelfExtract (extractTo)

main = do
  extractTo "dir" -- will extract to $CWD/dir
  extractTo "/usr/local/lib"
  ...
```

Extract to a temporary directory:

```
import Codec.SelfExtract (withExtractToTemp)
import System.Directory (removeDirectory)

main = do
  withExtractToTemp $ \tmp -> do
    ...
```

Using the [Path](https://hackage.haskell.org/package/path-0.6.1) library:

```
import Codec.SelfExtract (extractTo', withExtractToTemp')
import Path (absdir, reldir)
import Path.IO (removeDir)

main = do
  extractTo' $ [reldir|dir|] -- will extract to $CWD/dir
  extractTo' $ [absdir|/usr/local/lib|]
  withExtractToTemp' $ \tmp -> do
    ...
```

### Details

The above instructions should be a black box, but here is an explanation of the implementation
if you need to know the details of how it works.

When the executable containing `extractTo` is built, some space will be allocated to contain the
size of the binary.

`bundle` will find the executable from `LocalBuildInfo`'s `buildDir`. It will take the directory
specified and run `tar` on it. It will also get the size of the executable and write the size into
the space allocated by `extractTo`. Then `bundle` will replace the executable with the executable
itself concatenated with the tar archive.

When `extractTo` is called, it will read the size of the executable that was written in `bundle`.
After seeking to the size of the binary, the tar archive can be extracted to the desired directory.
