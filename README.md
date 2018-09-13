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
 Cabal                    8         691          655            0           36
 Cabal Project            1          30           25            0            5
 Dash                     1          54           38            0           16
 Dhall                    4         574          499            3           72
 Happy                    1        1039          886           34          119
 Haskell                 45        5173         4269          186          718
 Justfile                 1          62           46            3           13
 Markdown                26         940          669            0          271
 Nix                      1           5            4            0            1
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     4         191          171            3           17
-------------------------------------------------------------------------------
 Total                   96        9477         7884          241         1352
-------------------------------------------------------------------------------
```
