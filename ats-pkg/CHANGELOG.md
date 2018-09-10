# ats-pkg

## 3.3.2.0

  * Update `Debian` type and adjust prelude accordingly
  * Allow debian packages built to include header files and libraries.

## 3.2.1.8

  * Update Dhall prelude

## 3.2.1.2

Bug Fixes:

  * Don't default to 4 processors
  * Use `getAppUserDirectory` for better portability

Breaking Changes:

  * Only build C sources when sensible to do so
