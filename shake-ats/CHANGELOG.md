# shake-ats

## 1.10.4.0

  * Track C dependencies as well.

## 1.10.3.0

  * Don't copy files to `$PATSHOME`

## 1.10.2.2

  * `genLinks` calls `traced` for better profiling

## 1.10.2.1

  * Use oracle to track `HsCompiler` in rules

## 1.10.2.0

  * `genATS` takes `cpphs` parameter again

## 1.10.1.0

  * `genATS` no longer takes `cpphs` parameter

## 1.10.0.0

  * Make `cabalForeign` take a `HsCompiler` instead of a `CCompiler`.

## 1.9.0.5

  * Bugfix for recent version of `shake-cabal`

## 1.9.0.4

  * Remove timestamp from generated `.c` files.

## 1.9.0.3

  * Use `getAppUserDirectory` for better portability
