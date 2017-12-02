program re_example
  use regex

  implicit none

  character(len=3), allocatable :: list(:)
  integer :: i

  ! String matching
  print *, re_match(          "^Hello .* !",      "Hello, world!")    ! returns T
  print *, re_match(          "^Hello \s+ world", "Hello, world!")    ! returns F, no comma
  print *, trim( re_match_str("^Hello"          , "Hello, world!") )  ! returns "Hello"

  ! String splitting
  call re_split("\s* , \s*", "12, 16, 2, 9.6", list)                  ! returns ["12 ", "16 ", "2  ", "9.6"]
  do i = 1, size(list)
    print *, "'" // list(i) // "'"
  end do
  deallocate(list)

  ! String replacing
  print *, trim( re_replace("Hello", "Goodbye", "Hello, world!") )    ! returns "Goodbye, world!"
  
end program re_example

