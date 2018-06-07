# ATS

This is a collection of several Haskell packages for use with the
[ATS](http://ats-lang.org/) language.

## Goals

The following are goals of the libraries and tools contained herein:

  * Make handling ATS code in Haskell easy

  * Make building ATS easy

  * Make dependency resolution for ATS packages easy

  * Make building Haskell with ATS dependencies easy

  * Make building ATS with Haskell dependencies easy
  
As of writing, building ATS code in Haskell is quite smooth, package management
is unfortunately maladroit, and handling ATS code in Haskell is mostly
manageable.

## Tools

* [ats-pkg](ats-pkg/README.md): package management
* [ats-format](ats-format/README.md): automated formatter
* [hs2ats](hs2ats/README.md): convert Haskell types to ATS types
* [language-ats](language-ats/README.md): Haskell parser & pretty-printer for ATS

More to come!

## Contents

```
-------------------------------------------------------------------------------
 Language             Files       Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Alex                     2         762          654           16           92
 Bash                     1           8            6            0            2
 Cabal                   10         865          816            0           49
 Cabal Project            1          32           29            0            3
 Dhall                    4         569          500            0           69
 Happy                    1         976          831           34          111
 Haskell                 54        5094         4113          233          748
 Justfile                 1          74           59            0           15
 Markdown                23         834          605            0          229
 TeX                      1          65           45            0           20
 TOML                     1           3            3            0            0
 YAML                     6         227          211            0           16
-------------------------------------------------------------------------------
 Total                  105        9509         7872          283         1354
-------------------------------------------------------------------------------
```
