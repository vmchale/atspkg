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
 Alex                     2         784          676           16           92
 Bash                     1           8            6            0            2
 Cabal                   10         835          791            0           44
 Cabal Project            1          35           30            0            5
 Dash                     1          55           38            0           17
 Dhall                    4         518          452            0           66
 Happy                    1        1014          868           34          112
 Haskell                 53        5441         4447          212          782
 Justfile                 1          57           45            0           12
 Markdown                26         870          629            0          241
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     4         197          180            0           17
-------------------------------------------------------------------------------
 Total                  106        9883         8211          262         1410
-------------------------------------------------------------------------------
```
