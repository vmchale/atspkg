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
 Alex                     2         765          657           16           92
 Bash                     1           8            6            0            2
 Cabal                   10         861          811            0           50
 Cabal Project            1          33           30            0            3
 Dhall                    4         462          402            0           60
 Happy                    1         979          834           34          111
 Haskell                 53        5201         4228          223          750
 Justfile                 1          59           47            0           12
 Markdown                23         839          610            0          229
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     6         238          223            0           15
-------------------------------------------------------------------------------
 Total                  104        9514         7897          273         1344
-------------------------------------------------------------------------------
```
