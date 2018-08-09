# language-ats 

## 1.6.0.0

  * Remove types for `RecordValues` and instead rely on typed expressions.
  * Better Error messages
  * Add support for boxed records
  * Fix formatting for `val ... and ...` declarations
  * Fix parse error on expressions like `list_vt_cons(x, _)`
  * Add support for proof expressions introducing witnesses (`[ m | [] ]`)
  * Remove `Wildcard` constructor and instead treat `_` as a name

## 1.5.0.0

Bug Fixes:

  * Add support for patterns using binary operators.
