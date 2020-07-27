# ats-pkg

## 3.5.0.1

  * Bump config files

## 3.5.0.0

  * Remove `packageCompiler`

## 3.4.0.8

  * Drop `.bz2` decompression; support GHC 8.8.1

## 3.4.0.5

  * Pin Dhall package
  * More sensible verbosity flag

## 3.4.0.3

  * Support shake 0.18.4

## 3.4.0.1

  * Install manpages to `/usr/local/share/man/man1` on Mac
  * Disable optimizations when running `--debug`

## 3.4.0.0

  * Add `--debug` flag to `test` and `build` subcommands

## 3.3.0.6

  * Only run `./autogen.sh` when installing the compiler if
    `automake`/`autoconf` exist

## 3.3.0.5

  * Only run `cmake` when it exists

## 3.3.0.4

  * Fixes for Mac

## 3.3.0.3

  * Update manpages

## 3.3.0.1

  * Add `bench` subcommand to CLI

## 3.3.0.0

  * Add `bench` field to `Pkg` type

## 3.2.6.4

  * Upgrade to latest `dhall`

## 3.2.6.3

  * Better `clean` command

## 3.2.6.1

  * Display `language-ats` version when `--version` flag is passed

## 3.2.6.0

  * Add `license` and `changelog` field to `Debian` type
  * `lintian` doesn't object to debianizations anymore

## 3.2.5.14

  * Update for new Dhall library

## 3.2.5.13

  * Use new compiler in `atslib.dhall`

## 3.2.5.12

  * Use old Dhall prelude

## 3.2.5.11

  * Set UTF8 encoding in all cases

## 3.2.5.10

  * Use better URL

## 3.2.5.6

  * Use `libarchive` instead of `tar`
  * Change URL for compilers
  * Remove hidden `pack` command
  * Remove `packageCompiler` function
  * Update latest Dhall libraries

## 3.2.4.6

  * Bump Dhall libraries again

## 3.2.4.5

  * Bump Dhall libraries for new default compiler

## 3.2.4.4

  * Fix bug where `gc` version could not be constrained
  * Slightly improved behavior around C package versioning (allow package
    reinstalls)

## 3.2.4.2

  * Update `.dhall` files

## 3.2.4.0

  * Update to use `cpphs` again

## 3.2.3.0

  * Update to not use `cpphs`

## 3.2.2.4

  * Fix `dhall/atslib.dhall` file that is embedded into the binary

## 3.3.2.0

  * Update `Debian` type and adjust prelude accordingly
  * Allow Debian packages built to include header files and libraries.
  * Bug fixes related to Debian packaging.

## 3.2.1.8

  * Update Dhall prelude

## 3.2.1.2

Bug Fixes:

  * Don't default to 4 processors
  * Use `getAppUserDirectory` for better portability

Breaking Changes:

  * Only build C sources when sensible to do so
