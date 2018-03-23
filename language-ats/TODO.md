# Bugs
- [ ] Fix comments/annotations
  - [ ] I don't really have a good plan for this aside from parsing position and
    comments by default (instead of just position)
- [ ] Preserve `and` val declarations
- [ ] `primplmnt false_elim() = case+ 0 of _ =/=> ()`
- [ ] `stadef mpz = mpz_vt0ype: vt@ype`
# Deficiencies
- [ ] Error messages
  - [ ] Add test suite for messages
# Performance
- [ ] Get rid of `identifierSpace`
- [ ] Handle `stadef mytkind = $extkind"atslib_linmap_avltree"`
- [ ] Macros with `#else`
- [ ] Boxed tuples/records
- [ ] Literals, etc. should only be lexed when in the right state
