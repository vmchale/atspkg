# language-ats

# 1.7.10.0

  * Add support for left/right shift operators in expressions
  * Add support for array literals
  * Fix bug in `absvt@ype` and `abst@ype` pretty-printing

# 1.7.9.0

  * Support float literals as something other than double literals

# 1.7.8.0

  * Add support for unsigned hexadecimal literals

## 1.7.7.2

  * Fix error when parsing `absvt@ype` declarations

## 1.7.7.1

  * Use `these-skinny`

## 1.7.7.0

  * Improved parse errors
  * Add `getDependenciesC`

## 1.7.6.2

  * Fix bug in handling of char literals

## 1.7.6.1

  * Bugfix in pretty-printer/lexer

## 1.7.6.0

  * Use `Natural`/`Integer` for literals

## 1.7.5.0

  * Fix `StaticExpression` to allow calls with dynamic components

## 1.7.4.1

  * Improve pretty-printer

## 1.7.4.0

  * Add `languageATSVersion`

## 1.7.3.1

  * Fix bug in parse order for `symintr` declarations

## 1.7.3.0

  * Update `PrVal` to include a field for universal quantifiers

## 1.7.2.0

  * Update `termetric` field type to allow empty termetrics

## 1.7.1.2

  * Add `cross` flag to cabal file

## 1.7.1.1

  * Bugfix in how `val`s were handled

## 1.7.1.0

  * Fix a bug in how `val`s were handled in `SATS` files

## 1.7.0.6

  * Fix bug by introducing immorality

## 1.7.0.5

  * Faster build times

## 1.7.0.4

  * Allow `datatype`, `datavtype`, and `dataview` to work with `and`.
  * Remove spurious dependency on `cpphs`
  * Prettier errors when parsing records

## 1.7.0.3

  * Bump `recursion` version bounds

## 1.7.0.2

Bug fixes:

  * Support `llam@` keyword

Enhancements:

  * Use `recursion` library to incur fewer dependencies

## 1.7.0.0

Bug Fixes:

  * Now accepts proof-level lambdas.
  * Include all test data

Breaking Changes:

  * `PrVar` and `PrVal` now take a `StaticExpression`
  * `PrFun`, `PrFn`, and `Praxi` now take a `StaticExpression`
  * Add a rewrite phase for `StaticExpression`s

## 1.6.0.0

Breaking Changes:

  * Remove types for `RecordValues` and instead rely on typed expressions.
  * Remove `Wildcard` constructor and instead treat `_` as a name
  * Remove `ParenType` and instead use tuples

Enhancements:

  * Better Error messages
  * Add support for boxed records
  * Add support for proof expressions introducing witnesses (`[ m | () ]`)

Bug Fixes:

  * Fix bug with formatting for type arguments
  * Fix formatting for `val ... and ...` declarations
  * Fix parse error on expressions like `list_vt_cons(x, _)`
  * Add support for patterns using binary operators.
