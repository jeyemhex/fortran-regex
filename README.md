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
  print *, re_match("^Hello .* !", "Hello, world!")                 ! Returns T
  print *, re_match("^Hello \s+ world", "Hello, world!")            ! Returns F, no comma
  print *, trim( re_match_str("^Hello", "Hello, world!") )          ! Returns "Hello"

  ! String splitting
  call re_split("\s* , \s*", "12, 16, 2, 9.6", list)                ! Returns ["12 ", "16 ", "2  ", "9.6"]
  do i = 1, size(list)
    print *, "'" // list(i) // "'"
  end do
  deallocate(list)

  ! String replacing
  print *, trim( re_replace("Hello", "Goodbye", "Hello, world!") )  ! Returns "Goodbye, world!"

end program re_example
```

## Syntax

As with any implementation of regular expressions, it is incompatible with all
others. However, the basic syntax is the same, and I have borrowed features from
popular existing implementations (mostly Perl 5 & 6).

The basic syntax is as follows:

  * `'c'` is a regular character, as should be expected.
  * `'c*'` matches zero or more `c`
  * `'c+'` matches one or more `c`
  * `'c?'` matches  zero or one `c`
  * `'^c'` matches an `c` at beginning of the string.
  * `'c$'` matches an `c` at end of the string.
  * `'ab|cd'` matches `ab` OR `cd`

Expressions can by grouped by parentheses:

  * `'a(bc)+d'` matches `abcd`, `abcbcd`, `abcbcbcd` etc...
  * `a(bc+)?d` matches `ad`, `abcd`, `abccd` etc... but not `accd`

Most common character groups are also available:

  * `'.'` matches any character
  * `'\a'` matches any alphanumeric character ([a:z]|[A:Z])
  * `'\d'` matches any decimal character ([0:9])
  * `'\w'` matches any word character (\a|\d|'_')
  * `'\s'` matches any white-space character (" "|\t)

In addition, the capital of the above match anything but that character (except
capital `.` of course. Maybe I'll implement that when I know what capital `.`
is...).  

All special characters can themselves be matched by escaping them with a `\`,
for example:

  * `'\('` matches a literal `(` 
  * `'\\'` matches a literal `\`
  * `'\ '` matches a literal `' '` (see below).
  * etc...

One important difference between this implementation and many others is that
white-space in the regular expression is ignored. For example:

 * `"a b"` will match `ab` but not `a b`. 

This can be done, as mentioned above, by escaping the space `"a\ b"`, or using
`\s` if you don't mind tabs being captured too. This is to hopefully improve
readability of the resulting regular expressions, and was borrowed from Perl's
`/<regex>/x` functionality.

For more examples of the syntax, see `re_test.f90`.

### NOT YET IMPLEMENTED
This is, (and will continue to be for a while I'm afraid), a work in progress.
As a result, I have not finished implementing all the features I would like. These are currently speculative, as the syntax hasn't been properly thought through.
Some features I would like to implement before I decide this is complete are:

  * Arbitrary character groups, e.g. `'[aeiouAEIOU]'` to match only vowels
  * Case insensitive searching
  * A negation operation. I'm tempted to use `'c!'` as a "matches zero `c`'s"
    operation, but I haven't really thought too much about the consequences of
    that yet.
  * More fine-grained match counting, e.g. `'ab{2:3}c'` to only match `abbc` or `abbbc`

  * Some way of referring back to bracketed expressions, e.g. after matching `'a(bc)((d)e)'`
      * `match([])` would return `abcde`
      * `match([1])` would return `bc`
      * `match([2])` would return `de`
      * `match([2,1])` would return `d`

   * Defined captures would be nice, e.g. `<integer: \d+>` would define a reusable rule that would match integers with `<integer>`. This would require making `<`, `>` and `:` special characters too.

   * Named captures too... e.g. `(name: \w+) = (value: \d+)` could be referenced within `match("name", [])` and `match("value", [])` respectively, or something. 


## Prerequisites
regex.f90 currently requires:

  * Fortran 2003 compiler

## Acknowledgements

This implementation of regular expressions was based on Russ Cox's description
at [https://swtch.com/~rsc/regexp](https://swtch.com/~rsc/regexp). Many thanks
to Russ for his excellent webpage!


## Version
v0.3.1: 2017-12-04

## Author
Edward Higgins
