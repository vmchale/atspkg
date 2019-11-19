% atspkg (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

atspkg - a build tool for ATS

# DESCRIPTION

**atspkg** is a build tool for the ATS2 language, written in Haskell.

# SYNOPSIS

  atspkg build

  atspkg install

  atspkg clean

  atspkg test

# SUBCOMMANDS

**build** - Build all binary targets listed in atspkg.dhall

**test** - Run all tests listed in atspkg.dhall

**bench** - Rull all benchmarks listed in atspkg.dhall

**clean** - Clean current project directory

**nuke** - Remove all local files installed by **atspkg**

**remote** - Download a tarball from the given URL and try to build its binary
targets

**install** - Install binary targets to $HOME/.local/bin and relevant manpages
to $HOME/.local/share/man/man1

**upgrade** - Download the latest binary release of **atspkg**, if available

**valgrind** - Run **valgrind** on the generated binary

**run** - Run the generated binary

**check** - Check a pkg.dhall file to make sure it is well-typed.

**check-set** - Check a package set to make sure it is well-typed.

**list** - List all available packages in current package set.

**pack** - Create a tarball suitable for packaging the compiler. Takes as an
argument a directory containing the unpacked compiler.

**setup** - Set up manpages and shell completions.

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-\-pkg-args**
:   Arguments to be passed to atspkg.dhall

**-c** **-\-no-cache**
:   Ignore cached configuration file

**-r**, **-\-rebuild**
:   Rebuild all binary targets.

**-l**, **-\-no-lint**
:   Disable the build system linter

**-t**, **-\-target**
:   Set the compilation target using its triple.

**-v**, **-\-verbose**
:   Turn up the verbosity

**-d**, **-\-detailed**
:   Enable detailed error messages when checking configuration files

**-\-debug**
:   Disable binary stripping and pass -g to the C compiler

# CONFIGURATION

**atspkg** is configured with Dhall, in an atspkg.dhall file. **atspkg** can be
configured to produce binary targets, static library targets, shared object
files, or plain C targets. Artifacts can be linked against Haskell libraries if
desired.

There is also a file $HOME/.config/atspkg/config.dhall which can be used to
configure all builds.

## TEMPLATES

There are several templates available for **pi** (see
https://crates.io/crates/project_init for more details). Several examples:

```
pi new ats project
```

```
pi git vmchale/haskell-ats ambitious-project
```

```
pi git vmchale/ats-haskell weird-project
```

# BUGS

Please report any bugs you may come across to
https://github.com/vmchale/atspkg/issues (for issues in atspkg proper) or
https://hub.darcs.net/vmchale/ats/issues (for issues in supporting libraries).

# COPYRIGHT

Copyright 2018-2019. Vanessa McHale. All Rights Reserved.
