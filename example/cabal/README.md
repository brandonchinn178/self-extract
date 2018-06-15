# self-extract-cabal

A basic example package that bundles the `dist/` directory and extracts it elsewhere.

```
$ stack build self-extract-cabal

$ stack exec self-extract-cabal
Enter the directory to extract contents:
foo/
Contents of hello-world.txt:
Hello world!

$ ls foo/
hello-world.txt
```
