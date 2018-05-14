# self-extract-basic

A basic example package that bundles the `dist/` directory and extracts it elsewhere.

```
$ stack build self-extract-basic

$ stack exec self-extract-basic
Enter the directory to extract contents:
foo/
Contents of hello-world.txt:
Hello world!

$ ls foo/
hello-world.txt
```
