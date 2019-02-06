# ats-pkg

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
