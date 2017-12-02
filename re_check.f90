program re_check
  use regex

  character(len=100) :: str, re, repl

  if(command_argument_count() /= 3) stop "Needs 2 arguments"

  call get_command_argument(1, re)
  call get_command_argument(2, repl)
  call get_command_argument(3, str)

  print *, "'" // trim(re) // "'"
  print *, "'" // trim(repl) // "'"
  print *, "'" // trim(str) // "'"
  print *, "'" // trim(re_replace(trim(re), trim(repl), trim(str))) // "'"
end program re_check
