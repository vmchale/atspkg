⅋ ⊸
# Features
- [ ] `fetching...` should include version number.
- [ ] shell completions should list targets (?)
- [ ] custom build scripts in ATS (`setup.dats`)
- [ ] Set number of threads manually
- [ ] Initialize projects
# Libraries
- [ ] `shake-ats` should allow builds with `patscc` some other way
- [ ] Set target triple in `shake-ext`?
# Deficiencies
- [ ] Installations should be installed locally based on configuration
  - [ ] calculus of compatibility? algebra of compatibility?
- [ ] `atspkg` should be able to build `atslib` and thus work for
  cross-compilation.
- [ ] `triple` should be a configuration option
- [ ] Make cross builds work for `Distribution.ATS`
# Generalizations
- [ ] Make a `generic-package` library for `.a`, `.so`, and binary builds.
# Bugs
- [ ] Don't try to build test suite dependencies when doing a cross build
- [ ] We should be able to build `.so` and `.a` files from one source file.
- [ ] make paths portable for windows
- [ ] We shouldn't include `ccomp/lib` when doing a cross build.
- [ ] Always try to build `libats` with the right compiler version
- [ ] Generated code causes lint check failure?
# Documentation
- [ ] Tutorial
- [ ] User manual
- [ ] Document how to pin Dhall versions
# Code Quality
- [ ] Benchmark/test suite
- [ ] Make a `Dhall` subdirectory for "library" functions
- [ ] Instead of calling `atspkg install`, use the function.
