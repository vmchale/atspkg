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
 Alex                     2         766          658           16           92
 Bash                     1           8            6            0            2
 Cabal                   10         854          806            0           48
 Cabal Project            1          37           33            0            4
 Dhall                    4         462          402            0           60
 Happy                    1         979          834           34          111
 Haskell                 53        5251         4273          224          754
 Justfile                 1          59           47            0           12
 Markdown                23         835          606            0          229
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     6         245          229            0           16
-------------------------------------------------------------------------------
 Total                  104        9565         7943          274         1348
-------------------------------------------------------------------------------
```
