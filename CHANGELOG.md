## self-extract 0.2.0.0

Fixed bug due to stripping the exes after bundling, which is only a problem when bundling multiple
executables in a package.

Major fixes:
* Packages should use `bundle` in the `postCopy` hook, rather than the `postBuild` hook.

Other fixes:
* Updated documentation

## self-extract 0.1.0.0

* Initial implementation
