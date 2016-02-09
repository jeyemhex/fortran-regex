program re_test
  use regex
  implicit none

  character(len=120)  ::  failed(6,1000)
  integer ::  ifail, nfails, ntests

  nfails = 0
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

  ! Single character Or
  call test("ab|bc" , "ab"  , .true. , "ab"  )
  call test("ab|bc" , "ac"  , .false.        )
  call test("ab|bc" , "bc"  , .true. , "bc"  )
  call test("ab|bc" , "a"   , .false.        )
  call test("ab|bc" , "b"   , .false.        )
  call test("ab|bc" , "c"   , .false.        )
  call test("ab|bc" , "abc" , .true. , "ab"  )

  
  ! Random tests for finding word boundaries
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello : world"  , .true. , "hello : world" )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello = world"  , .true. , "hello = world" )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello = world"  , .true. , "hello = world" )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello world"    , .true. , "hello world"   )
  call test("^\w+\s+(:|=)?\s*\w+$" , "hello, world"   , .false.                  )
  call test("^\w+\s+\w+$"          , "hello world"    , .true. , "hello world"   )
  call test("^\w+\W+\w+$"          , "hello world"    , .true. , "hello world"   )
  call test("^\w+\W+\w+$"          , "hello,world"    , .true. , "hello,world"   )
  call test("^\w+\W+\w+$"          , "HelloWorld"     , .false.                  )

  ! Random tests for finding numbers
  call test("^\d+(\.\d*)?$" , "0.63"   , .true. , "0.63" )
  call test("^\d+(\.\d*)?$" , "724"    , .true. , "724"  )
  call test("^\d+(\.\d*)?$" , "6.2_dp" , .false.         )
  call test("^\d+(\.\d*)?$" , "6."     , .true. , "6."   )
  call test("^\d+(\.\d*)?$" , ".32"    , .false.         )

  print *, " "
  select case (ntests)
    case(0:9)
      write(*,'(I1,A1,I1,A14)') ntests-nfails, '/', ntests, ' tests passed!'
    case(10:99)
      write(*,'(I2,A1,I2,A14)') ntests-nfails, '/', ntests, ' tests passed!'
    case(100:999)
      write(*,'(I3,A1,I3,A14)') ntests-nfails, '/', ntests, ' tests passed!'
    case default
      write(*,*) "Really?! You have", ntests, "tests??"
  end select

  if(nfails > 0) then
    call print_failures(nfails, failed, 0)
  end if

contains
  subroutine test(re, str, succ, res)
    character(len=*), intent(in)  :: re
    character(len=*), intent(in)  :: str
    logical,          intent(in)  :: succ
    character(len=*), intent(in), optional  ::  res

    logical             :: local_succ
    character(len=120)  :: local_res

    local_succ = .false.
    local_res = " "

    local_succ = re_match(re, str)
    if(local_succ .eqv. succ) then
      if(present(res)) then
        local_res = re_match_str(re, str)
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
      local_res = re_match_str(re, str)
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
