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

## OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-c** **-\-no-cache**
:   Ignore cached configuration file

**-r**, **-\-rebuild**
:   Rebuild all binary targets.

**-l**, **-\-no-lint**
:   Disable the shake linter

**-t**, **-\-target**
:   Set the compilation target using its triple.

**-v**, **-\-verbose**
:   Turn up the verbosity

**-d**, **-\-detailed**
:   Enable detailed error messages

# CONFIGURATION

**atspkg** is configured with Dhall, in an atspkg.dhall file. **atspkg** can be
configured to produce binary targets (possibly linked against Haskell
libraries), as well as plain C targets.

## TEMPLATES

There are several template avaiable for **pi** as well (see
https://crates.io/crates/project_init for more details). A couple examples:

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

Copyright 2018. Vanessa McHale. All Rights Reserved.
