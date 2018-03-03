# language-ats

This is a package similar to
[language-c](http://hackage.haskell.org/package/language-c) or
[haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts) that
provides a parser and pretty-printer for [ATS](http://ats-lang.org/).

The parser is slightly buggy but it can handle almost all of the language; see
the `test/data` directory for examples of what it can handle.

The pretty-printer works quite well; you can safely use it for code generation.
