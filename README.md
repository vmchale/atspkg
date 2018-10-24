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
 Cabal                    8         693          641            0           52
 Cabal Project            1          37           28            1            8
 Dash                     1          54           38            0           16
 Dhall                    4         574          499            3           72
 Happy                    1        1039          886           34          119
 Haskell                 45        5160         4259          186          715
 Justfile                 1          62           46            3           13
 Markdown                28         968          686            0          282
 Nix                      4          60           58            0            2
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     4         202          184            0           18
-------------------------------------------------------------------------------
 Total                  101        9567         7947          239         1381
-------------------------------------------------------------------------------
```
