module regex
  use io, only: io_abort, io_allocate_abort

  implicit none

  private

  public :: post_to_nfa, print_state

  integer,  parameter ::  split = 256
  integer,  parameter ::  match = 257

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

    if(local_depth > 8) then 
      print *, "Trying to print a superdeep structure!"
    else
      tmp_s => s
      do i = 1, local_depth
        write(*,'(A2)', advance="no") "  "
      end do
      print *, "State:", tmp_s%c
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

    do while( ch_loc <= len(trim(postfix)) )
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

    if(matchstate%c /= match) print *, "***** Matchstate has changed!", matchstate%c
    if(nullstate%c /= 0) print *, "***** Nullstate has changed!", nullstate%c
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

end module regex

program re_test
  use regex
  use io, only: io_initialise
  implicit none

  type(state), pointer ::  s
  type(state), pointer  ::  next
  character(len=120) :: postfix

  call io_initialise(postfix)

  s => post_to_nfa(postfix)
  call print_state(s)

end program re_test
