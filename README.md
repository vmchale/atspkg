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
 Cabal                   10         834          790            0           44
 Cabal Project            1          34           30            0            4
 Dhall                    4         478          416            0           62
 Happy                    1         979          834           34          111
 Haskell                 52        5232         4257          224          751
 Justfile                 1          56           44            0           12
 Markdown                23         837          607            0          230
 TeX                      1          66           46            0           20
 TOML                     1           3            3            0            0
 YAML                     4         182          166            0           16
-------------------------------------------------------------------------------
 Total                  101        9475         7857          274         1344
-------------------------------------------------------------------------------
```
