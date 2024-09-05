program shakespeare_test
!==============================================================================#
! SHAKESPEARE_TEST
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2024-09-05
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use regex
  implicit none

  integer :: io, length
  character(len=52) :: query = '(\w+ nevermatch | This ) \s website .* Gutenberg™'
  character(:), allocatable :: lines

  open(newunit=io, file="data/shakespeare.txt", status="old", action="read", access="stream", form="unformatted")

  print *, "reading"
  inquire(io, size=length)

  allocate(character(len=length) :: lines)
  read(io) lines
  print *, "Found" , len_trim(lines) , "characters"

  print *, "matching query: {" // query // "}..."
  print *, 'match = "' // trim(re_match_str(query, lines)) // '"'

  deallocate(lines)
  close(io)

end program shakespeare_test
