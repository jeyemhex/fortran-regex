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

  character(len=3), allocatable :: list(:)
  integer :: i

  ! String matching
  print *, re_match("^Hello .* !", "Hello, world!")                 ! returns T
  print *, re_match("^Hello \s+ world", "Hello, world!")            ! returns F, no comma
  print *, trim( re_match_str("^Hello", "Hello, world!") )          ! returns "Hello"

  ! String splitting
  call re_split("\s* , \s*", "12, 16, 2, 9.6", list)                ! returns ["12 ", "16 ", "2  ", "9.6"]
  do i = 1, size(list)
    print *, "'" // list(i) // "'"
  end do
  deallocate(list)

  ! String replacing
  print *, trim( re_replace("Hello", "Goodbye", "Hello, world!") )  ! returns "Goodbye, world!"

end program re_example
```

## Prerequisites
regex.f90 currently requires:

  - Requires a Fortran 2003 compiler

# Version
v0.2.1: 2017-12-01

# Author
Edward Higgins
