% atsfmt (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

atsfmt - a source code formatter for ATS

# SYNOPSIS

  atsfmt \<file\>

  atsfmt -i \<file\>

  atsfmt -\-default-config

  cat file.dats | atsfmt

  atsfmt --default-config

# DESCRIPTION

**atsfmt** is an opinionated formatter for that ATS2 language.

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-o** **-\-no-config**
:   Ignore configuration files in scope

**-i**
:   Modify a file in-place.

**-\-default-config**
:   Generate a default configuration file in the current directory

# CONFIGURATION

**atsfmt** is configured using a TOML file, by default .atsfmt.toml. You can
generate a default configuration with

```
atsfmt --default-config
```

To make **atsfmt** call clang-format on embedded C code, add the following to
your .atsfmt.toml

```
clang-format = true
```

You can also set ribbon width and line width in the file, viz.

```
ribbon = 0.6
width = 120
```

Ribbon width is the width of a line excluding indentation.
In this example, the maximum column number will be 120 and the maximum ribbon
width will be 0.6 * 120 = 72 characters.

# EDITOR INTEGRATION

Editor integration is available with the ATS vim plugin at:

https://github.com/vmchale/ats-vim

# COPYRIGHT

Copyright 2017-2018. Vanessa McHale. All Rights Reserved.
