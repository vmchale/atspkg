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
 Alex                     1         641          567           12           62
 Bash                     1           8            6            0            2
 Cabal                    8         704          650            0           54
 Cabal Project            1          34           26            1            7
 Dash                     1          54           38            0           16
 Dhall                    4         574          499            3           72
 Happy                    1        1039          886           34          119
 Haskell                 45        5170         4269          185          716
 Justfile                 1          62           49            0           13
 Markdown                28         982          694            0          288
 Nix                      2          24           23            0            1
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     4         198          181            0           17
-------------------------------------------------------------------------------
 Total                   99        9559         7937          235         1387
-------------------------------------------------------------------------------
```
