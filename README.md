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
 Cabal                   13        1104         1023            0           81
 Cabal Project            1          33           30            0            3
 Dhall                    4         462          402            0           60
 Happy                    1         979          834           34          111
 Haskell                 66        8315         6505          669         1141
 Justfile                 1          62           49            0           13
 Markdown                27         958          691            0          267
 Plaintext                1          22           19            0            3
 TeX                      1          65           45            0           20
 TOML                     1           3            3            0            0
 YAML                     7         384          352            0           32
-------------------------------------------------------------------------------
 Total                  126       13160        10616          719         1825
-------------------------------------------------------------------------------
```
