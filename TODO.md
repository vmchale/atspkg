# Features
- [ ] `fetching...` should include version number.
- [ ] shell completions should list targets (?)
- [ ] custom build scripts in ATS (`setup.dats`)
- [ ] Set number of threads manually
# Libraries
- [ ] `shake-ats` should allow builds with `patscc` some other way
- [ ] Set target triple in `shake-ext`?
# Deficiencies
- [ ] Build for files not in the current directory?
- [ ] Installations should be installed locally based on configuration
  - [ ] calculus of compatibility? algebra of compatibility?
- [ ] `atspkg` should be able to build `atslib` and thus work for
  cross-compilation.
- [ ] `triple` should be a configuration option
# Generalizations
- [ ] Make a `generic-package` library for `.a`, `.so`, and binary builds.
- [ ] Install GHC/Cabal? (see:
  `https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-src.tar.xz`)
- [ ] `ftp://sourceware.org/pub/valgrind/valgrind-3.13.0.tar.bz2`
# Bugs
- [ ] We should be able to build `.so` and `.a` files from one source file.
- [ ] make paths portable for windows
# Documentation
- [ ] Tutorial
- [ ] User manual
- [ ] Document how to pin Dhall versions
# Code Quality
- [ ] Benchmark/test suite
- [ ] Make a `Dhall` subdirectory for "library" functions
