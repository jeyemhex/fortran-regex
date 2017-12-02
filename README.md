# REGEX
## A Fortran 2003 implementation of regular expressions

## Introduction
In ~~a moment of madness~~ an attempt to better understand the tools I use on
a day to day basis, I decided to try and implement regular expressions in
Fortran. This is my progress so far...

## Synopsis

```Fortran
program re_example
  use regex
  implicit none

  ! String matching
  print *, re_match("^Hello.*world!", "Hello, world!")    ! returns T
  print *, re_match("^Hello\s+world", "Hello, world!")    ! returns F
  print *, re_match_str("^Hello", "Hello, world!")        ! returns "Hello"

  ! String splitting
  print *, re_split("\s+", "12 16  2 9.6")                ! returns ["12 ", "16 ", "2  ", "9.6"]

  ! String replacing
  print *, re_replace("Hello", "Goodbye", "Hello, world!" ! returns "Goodbye, world!"
  
```

## Prerequisites
VScreen currently requires:

  - Requires a Fortran 2003 compiler

# Version
v0.2.1: 2017-12-01

# Author
Edward Higgins
