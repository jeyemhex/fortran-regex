program re_check
  use regex

  character(len=100) :: str, re

  if(command_argument_count() /= 2) stop "Needs 2 arguments"

  call get_command_argument(1, str)
  call get_command_argument(2, re)
  print *,  re_match(re,str), trim(re_match_str(re,str))

end program re_check
