## self-extract 0.3.4

Fixes:
* Better error message if no archive was bundled

## self-extract 0.3.3

Fixes:
* Make bundling occur in same directory as exe
* Use typed paths for `ztar`

## self-extract 0.3.2

Fixes:
* Use `GZip` instead of `Zip` (#14), thanks to ztar-0.1.1

## self-extract 0.3.1

Other fixes:
* Uses `Zip` instead of `GZip`, due to filename limits in tar (#14)

## self-extract 0.3.0

Breaking fixes:
* `bundle` now works standalone, without needing to be in a Cabal hook
* Use `getExe` in Cabal hooks instead

Other fixes:
* Add tests
* Add dev flag
* Upgrade to ztar-0.1.0

## self-extract 0.2.0.0

Fixed bug due to stripping the exes after bundling, which is only a problem when bundling multiple
executables in a package.

Major fixes:
* Packages should use `bundle` in the `postCopy` hook, rather than the `postBuild` hook.

Other fixes:
* Updated documentation

## self-extract 0.1.0.0

* Initial implementation
