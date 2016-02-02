module regex
  use io, only: io_abort, io_allocate_abort

  implicit none

  private

  public :: re_match

  integer,  parameter ::  split = 256
  integer,  parameter ::  match = 257
  integer,  parameter , public ::  postfix_buff_size = 8000

  type  ::  paren_list
    integer ::  n_atom
    integer ::  n_alt
  end type paren_list

  type, public  :: state
    integer               ::  c
    type(state),  pointer ::  out1 => null()
    type(state),  pointer ::  out2 => null()
    integer               ::  lastlist
  end type state

  type  :: ptr_list
    type(state),    pointer ::  s => null()
    integer                 ::  side = -1
    type(ptr_list), pointer ::  next => null()
  end type ptr_list

  type  :: frag
    type(state),    pointer ::  start => null()
    type(ptr_list), pointer ::  out1 => null()
  end type frag

  type  :: frag_stack
    type(frag), pointer ::  elem
  end type frag_stack

  integer :: n_states = 0

contains

  function new_state(c, out1, out2)
    type(state)  ::  new_state
    integer,                intent(in)  ::  c
    type(state),  pointer,  intent(in)  ::  out1, out2

    n_states = n_states + 1
    new_state%lastlist = 0
    new_state%c = c
    new_state%out1 => out1
    new_state%out2 => out2


  end function new_state

  function new_frag(s, l)
    type(frag), pointer ::  new_frag
    type(state),    pointer,  intent(in)  ::  s
    type(ptr_list), pointer,  intent(in)  ::  l

    allocate(new_frag)
    new_frag%start => s
    new_frag%out1  => l


  end function new_frag

  recursive subroutine print_state(s, depth)
    type(state), pointer, intent(in) ::  s
    integer,  optional,   intent(in) :: depth
    
    integer ::  local_depth, i
    type(state), pointer  ::  tmp_s
    character(len=12), save :: ws

    local_depth=0
    if(present(depth)) then 
      local_depth = depth
    end if

    if(local_depth > 16) then 
      print *, "Trying to print a superdeep structure!"
    else
      tmp_s => s
      do i = 1, local_depth
        write(*,'(A3)', advance="no") "|  "
      end do
      if(tmp_s%c == 0) then
        write(*,'(A7,A4)'), "State: ","NULL"
      else if(tmp_s%c == 256) then
        write(*,'(A7,A5)'), "State: ", "SPLIT"
      else if(tmp_s%c == 257) then
        write(*,'(A7,A5)'), "State: ", "MATCH"
      else
        write(*,'(A7,A4)'), "State: ", achar(tmp_s%c) // "   "
      end if
      if(associated(tmp_s%out1)) call print_state(tmp_s%out1, depth=local_depth+1)
      if(associated(tmp_s%out2)) call print_state(tmp_s%out2, depth=local_depth+1)
    end if

  end subroutine print_state

  function new_list(outp, side)
    type(ptr_list), pointer :: new_list
    type(state),    pointer,  intent(in)  ::  outp
    integer,                  intent(in)  ::  side

    integer ::  ierr

    allocate(new_list, stat=ierr)
    if(ierr /= 0) call io_allocate_abort("new_list", ierr)

    new_list%s    => outp
    new_list%side = side
    new_list%next => null()

  end function new_list

  function append(l1, l2)
    type(ptr_list), pointer ::  append
    type(ptr_list), pointer,  intent(inout) :: l1
    type(ptr_list), pointer,  intent(in)    :: l2

    append => l1
    do while( associated(l1%next) )
      l1 => l1%next
    end do

    l1%next => l2

  end function append

  subroutine patch(l, s)
    type(ptr_list), pointer, intent(inout)  ::  l
    type(state),    pointer, intent(in)     ::  s

    type(ptr_list), pointer :: tmp_l

    tmp_l => l
    do while( associated(tmp_l) )
      select case(tmp_l%side)
        case(1)
          tmp_l%s%out1 => s
        case(2)
          tmp_l%s%out2 => s
        case default
          call io_abort("Unexpected value of side")
      end select
      tmp_l => tmp_l%next
    end do

  end subroutine patch

  function re_to_post(re) result(pf)
    character(len=postfix_buff_size) :: pf
    character(len=*),   intent(in)  ::  re

    integer          :: n_alt, n_atom
    integer          :: re_loc, pf_loc
    type(paren_list) :: paren(100)
    integer          :: par_loc

    par_loc = 1
    re_loc  = 1
    pf_loc  = 1
    n_alt   = 0
    n_atom  = 0

    pf = " "

    if(len_trim(re) > postfix_buff_size/2) call io_abort("Regex too long!")
    do while(re_loc <= len_trim(re))
      select case(re(re_loc:re_loc))

        case('(')
          if(n_atom > 1) then
            n_atom = n_atom - 1
            pf(pf_loc:pf_loc) = "."
            pf_loc = pf_loc + 1
          end if
          if(par_loc > size(paren)) call io_abort("Too many embedded brackets!")
          paren(par_loc)%n_alt  = n_alt
          paren(par_loc)%n_atom = n_atom
          par_loc = par_loc + 1
          n_alt   = 0
          n_atom  = 0

        case('|')
          if(n_atom == 0) call io_abort("N_atom is 0. Apparently that's a bad thing...")

          n_atom = n_atom - 1
          do while(n_atom > 0)
            pf(pf_loc:pf_loc) = "."
            pf_loc = pf_loc + 1
            n_atom = n_atom - 1
          end do
          n_alt = n_alt + 1

        case (')')
          if(par_loc == 1) call io_abort("I think you have an unmatched paren? maybe?")
          if(n_atom == 0) call io_abort("N_atom is 0. Apparently that's a bad thing...")

          n_atom = n_atom - 1
          do while(n_atom > 0)
            pf(pf_loc:pf_loc) = "."
            pf_loc = pf_loc + 1
            n_atom = n_atom - 1
          end do

          do while(n_alt > 0)
            pf(pf_loc:pf_loc) = "|"
            pf_loc = pf_loc + 1
            n_alt = n_alt - 1
          end do

          par_loc = par_loc - 1
          n_alt = paren(par_loc)%n_alt
          n_atom = paren(par_loc)%n_atom
          n_atom = n_atom + 1

        case('*','+','?')
          if(n_atom == 0) call io_abort("N_atom is 0. Apparently that's a bad thing...")
          pf(pf_loc:pf_loc) = re(re_loc:re_loc)
          pf_loc = pf_loc + 1

        case default
          if(n_atom > 1) then
            n_atom = n_atom - 1
            pf(pf_loc:pf_loc) = "."
            pf_loc = pf_loc + 1
          end if
          pf(pf_loc:pf_loc) = re(re_loc:re_loc)
          pf_loc = pf_loc + 1
          n_atom = n_atom + 1

      end select
      re_loc = re_loc + 1

    end do

    if(par_loc /= 1) call io_abort("I think you've got unmatched parentheses")

    n_atom = n_atom - 1
    do while(n_atom > 0)
      pf(pf_loc:pf_loc) = "."
      pf_loc = pf_loc + 1
      n_atom = n_atom - 1
    end do

    do while(n_alt > 0)
      pf(pf_loc:pf_loc) = "|"
      pf_loc = pf_loc + 1
      n_alt = n_alt - 1
    end do

  end function re_to_post

  function post_to_nfa(postfix)
    type(state), pointer  ::  post_to_nfa
    character(len=*), intent(in)  ::  postfix

    integer ::  ch_loc, s_loc
    type(frag_stack)      ::  stack(1000)
    type(frag),   pointer ::  stack_p, e1, e2, e
    type(state),  pointer ::  s => null()
    type(state),  pointer ::  matchstate
    type(state),  pointer ::  nullstate

    integer ::  matchloc, nullloc
    

    allocate(matchstate, nullstate)
    matchstate = new_state(match, null(), null())
    nullstate = new_state(0, null(), null())
    stack_p => stack(1)%elem
    ch_loc  = 1
    s_loc   = 1

    matchloc = loc(matchstate)
    nullloc = loc(nullstate)
    if(associated(post_to_nfa)) call io_abort("post_to_nfa already associated")

    do while( ch_loc <= len_trim(postfix))
      s => null()
      select case( postfix(ch_loc:ch_loc) )

        case(".")
          e2 => pop()
          e1 => pop()
          call patch(e1%out1, e2%start)
          call push(new_frag(e1%start, e2%out1))
          e1 => null()
          e2 => null()

        case("|")
          e2 => pop()
          e1 => pop()
          allocate(s)
          s = new_state( split, e1%start, e2%start )
          call push( new_frag(s, append(e1%out1, e2%out1)) )
          s => null()
          e1 => null()
          e2 => null()

        case("?")
          e => pop()
          allocate(s)
          s = new_state( split, e%start, nullstate )
          call push( new_frag(s, append(e%out1, new_list(s, 2)))  )
          s => null()
          e => null()

        case("*")
          e => pop()
          allocate(s)
          s = new_state( split, e%start, nullstate )
          call patch(e%out1, s)
          call push( new_frag(s, new_list(s, 2))  )
          s => null()
          e => null()

        case("+")
          e => pop()
          allocate(s)
          s = new_state( split, e%start, nullstate )
          call patch(e%out1, s)
          call push( new_frag(e%start, new_list(s, 2))  )
          s => null()
          e => null()

        case default
          allocate(s)
          s = new_state( iachar(postfix(ch_loc:ch_loc)), nullstate, nullstate )
          call push( new_frag(s, new_list(s, 1)) )
          s => null()
          e => null()

      end select
      ch_loc = ch_loc + 1
    end do

    e => pop()

    if(s_loc /= 1) call io_abort("Stack is not empty on exit")
    call patch(e%out1, matchstate)

    post_to_nfa => e%start

    if(matchstate%c /= match) call io_abort("***** Matchstate has changed!")
    if(nullstate%c /= 0) call io_abort("***** Nullstate has changed!")
    e => null()
  contains

    subroutine push(f)
      type(frag), intent(in), pointer  ::  f

      s_loc = s_loc + 1
      stack(s_loc)%elem => f

    end subroutine push

    function pop()
      type(frag), pointer :: pop

      pop => stack(s_loc)%elem
      s_loc = s_loc - 1

    end function pop

  end function post_to_nfa

  recursive function simulate(nfa, str) result(res)
    logical :: res
    type(state),      intent(in)  ::  nfa
    character(len=*), intent(in)  ::  str

    select case (nfa%c)

      case (match)
        res = .true.

      case (0)
        res = .false.

      case(split)
        if((nfa%out1%c == match) .or. (nfa%out2%c == match)) then
          res = .true.
        else if( simulate(nfa%out1, trim(str)) )  then
          res = .true.
        else if( simulate(nfa%out2, trim(str)) ) then
          res = .true.
        else
          res = .false.
        end if

      case default
        if( nfa%c == iachar(str(1:1)) ) then
            if(len_trim(str) > 1) then
              res = simulate(nfa%out1, str(2:))
            else
              res = simulate(nfa%out1, str)
            end if
        else
          res = .false.
        end if

    end select

    return
  end function simulate

  function re_match(re, str)
    logical :: re_match
    character(len=*), intent(in)  ::  re
    character(len=*), intent(in)  ::  str

    character(len=postfix_buff_size)  ::  postfix
    type(state),  pointer             ::  nfa

    postfix = re_to_post(trim(re))
    nfa => post_to_nfa(postfix)
    re_match = simulate(nfa, trim(str))

  end function re_match

end module regex

program re_test
  use regex
  use io, only: io_initialise
  implicit none

  type(state), pointer ::  s
  type(state), pointer  ::  next
  character(len=120), allocatable :: args(:)
  character(len=120) :: re, str

  call io_initialise(args = args)
  re = args(1)
  str = args(2)

  print *, "String:  ", trim(str)

  print *, "Regex:   ", trim(re)

  print *, "Match:  ", re_match(re, str)

end program re_test
