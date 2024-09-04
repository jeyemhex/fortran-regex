program re_test
  use regex
  implicit none

  character(len=120)  ::  failed(6,1000)
  integer ::  ifail, nfails, nskips, ntests

  nfails = 0
  nskips = 0
  ntests = 0
  failed = ""

  ! Single character matching
  call test("a"    , "a"   , .true. , "a"   )
  call test("a"    , "b"   , .false.        )
  call test("a"    , "abc" , .true. , "a"   )
  call test("b"    , "axc" , .false.        )
  call test("b"    , "abc" , .true. , "b"   )
  call test("b"    , "axc" , .false.        )
  call test("c"    , "abc" , .true. , "c"   )
  call test("c"    , "abx" , .false.        )

  !Multi character matching
  call test("ab"   , "abc" , .true. , "ab"  )
  call test("ac"   , "axc" , .false.        )
  call test("bc"   , "abc" , .true. , "bc"  )
  call test("abc"  , "abc" , .true. , "abc" )

  ! Single character matching start of string
  call test("^a"   , "a"   , .true. , "a"   )
  call test("^a"   , "b"   , .false.        )
  call test("^a"   , "abc" , .true. , "a"   )
  call test("^b"   , "axc" , .false.        )
  call test("^b"   , "abc" , .false.        )
  call test("^c"   , "abc" , .false.        )
  call test("^c"   , "abx" , .false.        )

  !Multi character matching start of string
  call test("^ab"  , "abc" , .true. , "ab"  )
  call test("^bc"  , "abc" , .false.        )
  call test("^abc" , "abc" , .true. , "abc" )

  ! Single character matching end of string
  call test("a$"   , "a"   , .true. , "a"   )
  call test("a$"   , "b"   , .false.        )
  call test("a$"   , "abc" , .false.        )
  call test("b$"   , "axc" , .false.        )
  call test("b$"   , "abc" , .false.        )
  call test("c$"   , "abc" , .true. , "c"   )
  call test("c$"   , "abx" , .false.        )

  !Multi character matching end of string
  call test("ab$"  , "abc" , .false.        )
  call test("bc$"  , "abc" , .true. , "bc"  )
  call test("abc$" , "abc" , .true. , "abc" )

  ! Single character matching start and end of string
  call test("^a$"   , "a"   , .true. , "a"   )
  call test("^a$"   , "b"   , .false.        )
  call test("^a$"   , "abc" , .false.        )
  call test("^b$"   , "axc" , .false.        )
  call test("^b$"   , "abc" , .false.        )
  call test("^c$"   , "abc" , .false.        )
  call test("^c$"   , "abx" , .false.        )

  !Multi character matching start and end of string
  call test("^ab$"  , "abc" , .false.        )
  call test("^bc$"  , "abc" , .false.        )
  call test("^abc$" , "abc" , .true. , "abc" )


  ! Single character Or
  call test("a|b"   , "a"   , .true. , "a"   )
  call test("a|b"   , "b"   , .true. , "b"   )
  call test("a|b"   , "c"   , .false.        )

  ! Multi character Or
  call test("ab|bc" , "ab"  , .true. , "ab"  )
  call test("ab|bc" , "ac"  , .false.        )
  call test("ab|bc" , "bc"  , .true. , "bc"  )
  call test("ab|bc" , "a"   , .false.        )
  call test("ab|bc" , "b"   , .false.        )
  call test("ab|bc" , "c"   , .false.        )
  call test("ab|bc" , "abc" , .true. , "ab"  )

  !*, + and ? operators
  call test("a?"    , "a"   , .true. , "a"   )
  call test("a?"    , "b"   , .true. , ""    )
  call test("a?"    , "abc" , .true. , "a"   )
  call test("b?"    , "abc" , .true. , ""    )
  call test("c?"    , "abc" , .true. , ""    )
  call test("a?b"   , "a"   , .false.        )
  call test("a?b"   , "b"   , .true. , "b"   )
  call test("a?bc"  , "abc" , .true. , "abc" )
  call test("b?bc"  , "abc" , .true. , "bc"  )
  call test("abc?"  , "abc" , .true. , "abc" )
  call test("abx?"  , "abc" , .true. , "ab"  )

  ! Bracketed expressions
  call test("(ab)c"       , "abc" , .true. , "abc"   )
  call test("^(ab|xx)?c$" , "abc" , .true. , "abc"   )


  call test("abc"    , "ababc"                             , .true. , "abc"   )
  call test("cat"    , "He captured a catfish for his cat" , .true. , "cat"   )
  call test("hel*o?" , "hello"                             , .true. , "hello" )
  call test("hel*o?" , "hell"                              , .true. , "hell"  )
  call test("hel*o?" , "heo"                               , .true. , "heo"   )

  ! Random tests for finding word boundaries
  call test("^\w+ \w+$"            , "hello world"   , .false.                  )
  call test("^\w+\ \w+$"           , "hello world"   , .true. , "hello world"   )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello : world" , .true. , "hello : world" )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello = world" , .true. , "hello = world" )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello world"   , .true. , "hello world"   )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello, world"  , .false.                  )
  call test("^\w+ \s+ \w+$"        , "hello world"   , .true. , "hello world"   )
  call test("^\w+ \W+ \w+$"        , "hello world"   , .true. , "hello world"   )
  call test("^\w+ \W+ \w+$"        , "hello,world"   , .true. , "hello,world"   )
  call test("^\w+ \W+ \w+$"        , "HelloWorld"    , .false.                  )

  call test("^(\d+)\s+IN\s+SOA\s+(\S+)\s+(\S+)\s*\(\s*$" , "1 IN SOA non-sp1 non-sp2(" , .true. , "1 IN SOA non-sp1 non-sp2(" )
  call test("^(\d+)\s+IN\s+SOA\s+(\S+)\s+(\S+)\s*\(\s*$" , "1IN SOA non-sp1 non-sp2("  , .false.                              )
  call test("^(a(b(c)))(d(e(f)))(h(i(j)))(k(l(m)))$"     , "abcdefhijklm"              , .true. , "abcdefhijklm"              )

! Random tests for finding numbers
  call test("^(\d+(\.\d*)?)$"                           , "0.63"                  , .true. , "0.63"   )
  call test("^(\d+(\.\d*)?)$"                           , "724"                   , .true. , "724"    )
  call test("^(\d+(\.\d*)?)$"                           , "6.2_dp"                , .false.           )
  call test("^(\d+(\.\d*)?)$"                           , "6."                    , .true. , "6."     )
  call test("^(\d+(\.\d*)?)$"                           , ".32"                   , .false.           )
  call test("^((\d+\.\d*|\d*\.\d+|\d+))$"               , ".32"                   , .true. , ".32"    )
  call test("^(.\d\d\d)$"                              , "x123"                  , .true. , "x123"   )
  call test("^(.+\d\d\d)$"                              , "x1234"                 , .true. , "x1234"  )
  call test("^(.+\d\d\d)$"                              , "xx123"                 , .true. , "xx123"  )
  call test("^(.+\d\d\d)$"                              , "12345"                 , .true. , "12345"  )
  call test("^(.+\d\d\d)$"                              , "123"                   , .false.           )

  call test('\s (\+|-)? \d+ (\.\d*)? ((e|E) (\+|-)? \d+)? $' , "the number is -5.2e-6" , .true. , " -5.2e-6")
  call test('\s (\+|-)? \d+ (\.\d*)? ((e|E) (\+|-)? \d+)? $' , "the number is 1e0"     , .true. , " 1e0"    )
  call test('\s (\+|-)? \d+ (\.\d*)? ((e|E) (\+|-)? \d+)? $' , "the number is -6"      , .true. , " -6"     )
  call test('\s (\+|-)? \d+ (\.\d*)? ((e|E) (\+|-)? \d+)? $' , "the number is 5.6.2"   , .false.           )
  call test('\s (\+|-)? \d+ (\.\d*)? ((e|E) (\+|-)? \d+)? $' , "the number is 1e6.0"   , .false.           )
  call test('\s (\+|-)? \d+ (\.\d*)? ((e|E) (\+|-)? \d+)? $' , "the number is -e6"     , .false.           )

  ! Tests for capture groups
  call test('[aeiou]',                          'find a vowel',       .true., 'i'               )
  call test('a[01]',                            'a1',                 .true., 'a1'              )
  call test('a[01]',                            'a2',                 .false.                   )
  call test('a[01]b',                           'a0b',                .true., 'a0b'             )
  call test('a[01]b',                           'a2b',                .false.                   )
  call test('a[012345]b',                       'a5b',                .true., 'a5b'             )
  call test('a[012345]b',                       'ab',                 .false.                   )
  call test('test\ [01]\d',                     'test 12',            .true., 'test 12'         )
  call test('[({]5[)}]',                        'array(5)',           .true., '(5)'             )
  call test('[({]5[)}]',                        'array{5}',           .true., '{5}'             )
  call test('[({]5[)}]',                        'array[5]',           .false.                   )
  call test('(br[ea]d) \s and \s  (butt[oe]r)', 'my brad and buttor', .true., 'brad and buttor' )

!-[PROPOSED FEATURE TESTS]--------------------------------------------------------------------------


! Proposed tests for defined captures
!  call test('<integer: \d+> <integer>', 'there were 52 cats', .true. '52')
!  call test('<sign:     \+|-              > &
!                 &<decimal:  \d (\.\d*)?       > &
!                 &<exponent: (e|E) <sign>? \d+ > &
!                 &<sign>? <decimal> <exponent>?', 'the numebr is -5.2e-6', .true. '-5.2e-6')


  print *, " "
  select case (ntests)
    case(0:9)
      write(*,'(I1,A1,I1,A14)') ntests-nfails-nskips, '/', ntests, ' tests passed!'
    case(10:99)
      write(*,'(I2,A1,I2,A14)') ntests-nfails-nskips, '/', ntests, ' tests passed!'
    case(100:999)
      write(*,'(I3,A1,I3,A14)') ntests-nfails-nskips, '/', ntests, ' tests passed!'
    case default
      write(*,*) "Really?! You have", ntests, "tests??"
  end select

  if(nfails > 0) then
    write(*,*) " "
    call print_failures(nfails, failed, 0)
    stop nfails
  end if

contains
  subroutine test(re, str, succ, res)
    use iso_fortran_env
    character(len=*), intent(in)           :: re
    character(len=*), intent(in)           :: str
    logical,          intent(in)           :: succ
    character(len=*), intent(in), optional :: res

    logical             :: local_succ
    character(len=120)  :: local_res

    local_succ = .false.
    local_res = " "
    flush(error_unit)
    local_succ = re_match(re, str)
    if(local_succ .eqv. succ) then
      if(present(res)) then
        local_res = trim(re_match_str(re, str))
        if(trim(res) == trim(local_res)) then
          write(*,'(A1)', advance="no") "."
        else
          write(*,'(A1)', advance="no") "F"
          nfails = nfails + 1
          failed(1,nfails) = trim(re)
          failed(2,nfails) = trim(str)
          failed(3,nfails) = 'F'
          failed(4,nfails) = trim(local_res)
          failed(5,nfails) = 'F'
          failed(6,nfails) = trim(res)
          if(local_succ) failed(3,nfails) = 'T'
          if(succ) failed(5,nfails) = 'T'
        end if
      else
        write(*,'(A1)', advance="no") "."
      end if
    else
      local_res = trim(re_match_str(re, str))
      if(present(res)) then
        write(*,'(A1)', advance="no") "F"
        nfails = nfails + 1
        failed(1,nfails) = trim(re)
        failed(2,nfails) = trim(str)
        failed(3,nfails) = 'F'
        failed(4,nfails) = trim(local_res)
        failed(5,nfails) = 'F'
        failed(6,nfails) = trim(res)
        if(local_succ) failed(3,nfails) = 'T'
        if(succ) failed(5,nfails) = 'T'
      else
        write(*,'(A1)', advance="no") "F"
        nfails = nfails + 1
        failed(1,nfails) = trim(re)
        failed(2,nfails) = trim(str)
        failed(3,nfails) = 'F'
        failed(4,nfails) = trim(local_res)
        failed(5,nfails) = 'F'
        failed(6,nfails) = ""
        if(local_succ) failed(3,nfails) = 'T'
        if(succ) failed(5,nfails) = 'T'
      end if
    end if

    ntests = ntests + 1

  end subroutine test

  subroutine skip_test(re, str, succ, res)
    character(len=*), intent(in)           :: re
    character(len=*), intent(in)           :: str
    logical,          intent(in)           :: succ
    character(len=*), intent(in), optional :: res

    write(*,'(A1)', advance="no") "s"
    nskips = nskips + 1
    ntests = ntests + 1

  end subroutine skip_test

  subroutine print_failures(nfails, failed, f)
    integer,  intent(in)  ::  nfails
    character(len=*), intent(in)  ::  failed(:,:)
    integer,  intent(in)  ::  f
      write(f,*) "==========================================================="
    write(f,*) "  FAILED TESTS:"
    do ifail = 1, nfails
      write(f,*) "-----------------------------------------------------------"
      write(f,*) "Regex:"
      write(f,*) "  " // "/" // trim(failed(1,ifail)) // "/"
      write(f,*) "String:"
      write(f,*) "  " // "'" // trim(failed(2,ifail)) // "'"
      write(f,*) "Expected:"
      select case(trim(failed(5,ifail)))
        case('T')
          write(f,*) "  " // "MATCH"  // " '" // trim(failed(6,ifail)) // "'"
        case('F')
          write(f,*) "  " // "NO MATCH"
      end select
      write(f,*) "Got:"
      select case(trim(failed(3,ifail)))
        case('T')
          write(f,*) "  " // "MATCH"  // " '" // trim(failed(4,ifail)) // "'"
        case('F')
          write(f,*) "  " // "NO MATCH"
      end select

    end do
      write(f,*) "==========================================================="
  end subroutine print_failures

end program re_test
