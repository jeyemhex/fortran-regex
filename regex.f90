!==============================================================================!
! REGEX                                                                        !
!==============================================================================!
! Module containing routines for string manipulations with regular             !
! expressions.                                                                 !
!                                                                              !
!   Implementation based on the description (and some of the code) from        !
!   https://swtch.com/~rsc/regexp. Many thanks to Russ for his excellent       !
!   webpage!                                                                   !
!                                                                              !
!==============================================================================!
! Author: Edward Higgins, 2014-10-01                                           !
!==============================================================================!

module regex

  implicit none

  private

  public :: re_match

  interface re_match
    module procedure re_match_logical
  end interface re_match

  integer,  parameter ::  pf_buff_size = 8000

  ! Special NFA states
  integer,  parameter ::  null_st      = -1  ! denotes a NULL node in the nfa
  integer,  parameter ::  split_st     = 256 ! denotes a SPLIT node in the nfa
  integer,  parameter ::  match_st     = 257 ! denotes a MATCH node in the nfa

  ! /re/ and postscript operators
  integer,  parameter ::  star_op      = 301 ! * operator (0 or more)
  integer,  parameter ::  plus_op      = 302 ! + operator (1 or more)
  integer,  parameter ::  quest_op     = 303 ! ? operator (0 or 1)
  integer,  parameter ::  or_op        = 304 ! | operator (a or b)
  integer,  parameter ::  cat_op       = 305 ! . operator (cats 2 fragments)

  ! NFA special matches
  integer,  parameter ::  any_ch       = 401 ! .  match (anything)
  integer,  parameter ::  alpha_ch     = 402 ! \a match ([a..z]|[A..Z])
  integer,  parameter ::  numeric_ch   = 403 ! \d match ([0..9])
  integer,  parameter ::  word_ch      = 404 ! \w match (\d|\a|_)
  integer,  parameter ::  space_ch     = 405 ! \s match (" "|\t)
  integer,  parameter ::  n_alpha_ch   = 406 ! \A match (anything but \a)
  integer,  parameter ::  n_numeric_ch = 407 ! \D match (anything but \d)
  integer,  parameter ::  n_word_ch    = 408 ! \W match (anything but \w)
  integer,  parameter ::  n_space_ch   = 409 ! \S match (anything but \s)
  integer,  parameter ::  start_ch     = 410 ! ^  match (start of the string)
  integer,  parameter ::  finish_ch    = 411 ! $  match (end of the string)

  type  ::  paren_list
    integer ::  n_atom
    integer ::  n_alt
  end type paren_list

  type, public  :: state
    integer               ::  c
    type(state),  pointer ::  out1 => null()
    type(state),  pointer ::  out2 => null()
    integer               ::  last_list
  end type state

  type  :: ptr_list
    type(state),    pointer ::  s    => null()
    integer                 ::  side =  -1
    type(ptr_list), pointer ::  next => null()
  end type ptr_list

  type  :: frag
    type(state),    pointer ::  start => null()
    type(ptr_list), pointer ::  out1  => null()
  end type frag

  type  :: frag_stack
    type(frag), pointer ::  elem
  end type frag_stack

  integer :: n_states

contains

  function new_state(c, out1, out2)
    type(state)  ::  new_state
    integer,                intent(in)  ::  c
    type(state),  pointer,  intent(in)  ::  out1, out2

    n_states = n_states + 1
    new_state%last_list = 0
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
      if(tmp_s%c /= null_st) then
        do i = 1, local_depth
          write(*,'(A3)', advance="no") "|  "
        end do
        select case (tmp_s%c)
        case(0:255)
          write(*,'(A7,A4)'), "State: ", achar(tmp_s%c) // "   "
        case(split_st)
          write(*,'(A7,A5)'), "State: ", "SPLIT"
        case(match_st)
          write(*,'(A7,A5)'), "State: ", "MATCH"
        case(any_ch)
          write(*,'(A7,A5)'), "State: ", ".    "
        case(start_ch)
          write(*,'(A7,A5)'), "State: ", "START"
        case(finish_ch)
          write(*,'(A7,A5)'), "State: ", "FIN  "
        case(alpha_ch)
          write(*,'(A7,A5)'), "State: ", "\a   "
        case(numeric_ch)
          write(*,'(A7,A5)'), "State: ", "\d   "
        case(word_ch)
          write(*,'(A7,A5)'), "State: ", "\w   "
        case(space_ch)
          write(*,'(A7,A5)'), "State: ", "\s   "
        case(n_alpha_ch)
          write(*,'(A7,A5)'), "State: ", "\A   "
        case(n_numeric_ch)
          write(*,'(A7,A5)'), "State: ", "\D   "
        case(n_word_ch)
          write(*,'(A7,A5)'), "State: ", "\W   "
        case(n_space_ch)
          write(*,'(A7,A5)'), "State: ", "\S   "
        case default
          stop "Unrecognised character in print_state"
      end select
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
    if(ierr /= 0) stop "Unable to allocate new_list"

    new_list%s    => outp
    new_list%side =  side
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
          stop "Unexpected value of side"
      end select
      tmp_l => tmp_l%next
    end do

  end subroutine patch

  function re_to_post(re) result(pf)
    integer ::  pf(pf_buff_size)
    character(len=*),   intent(in) :: re

    integer          :: n_alt, n_atom
    integer          :: re_loc, pf_loc
    type(paren_list) :: paren(100)
    integer          :: par_loc
    logical          :: escaped
    integer          :: escaped_chr

    par_loc = 1
    re_loc  = 1
    pf_loc  = 1
    n_alt   = 0
    n_atom  = 0
    escaped = .false.
    if(re(1:1) == "^") re_loc = 2

    pf = null_st

    if(len_trim(re) > pf_buff_size/2) stop "Regex too long!"
    do while(re_loc <= len_trim(re))
      if(.not. escaped) then
        select case(re(re_loc:re_loc))
          case('\')
            escaped = .true.

          case('(')
            if(n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            if(par_loc > size(paren)) stop "Too many embedded brackets!"
            paren(par_loc)%n_alt  = n_alt
            paren(par_loc)%n_atom = n_atom
            par_loc = par_loc + 1
            n_alt   = 0
            n_atom  = 0

          case('|')
            if(n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."

            n_atom = n_atom - 1
            do while(n_atom > 0)
              pf(pf_loc) = cat_op
              pf_loc = pf_loc + 1
              n_atom = n_atom - 1
            end do
            n_alt = n_alt + 1

          case (')')
            if(par_loc == 1) stop "I think you have an unmatched paren? maybe?"
            if(n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."

            n_atom = n_atom - 1
            do while(n_atom > 0)
              pf(pf_loc) = cat_op
              pf_loc = pf_loc + 1
              n_atom = n_atom - 1
            end do

            do while(n_alt > 0)
              pf(pf_loc) = or_op
              pf_loc = pf_loc + 1
              n_alt = n_alt - 1
            end do

            par_loc = par_loc - 1
            n_alt = paren(par_loc)%n_alt
            n_atom = paren(par_loc)%n_atom
            n_atom = n_atom + 1

          case('*')
            if(n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."
            pf(pf_loc) = star_op
            pf_loc = pf_loc + 1

          case('+')
            if(n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."
            pf(pf_loc) = plus_op
            pf_loc = pf_loc + 1

          case('?')
            if(n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."
            pf(pf_loc) = quest_op
            pf_loc = pf_loc + 1

          case ('.')
            if(n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = any_ch
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

          case ('^')
            if(n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = start_ch
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

          case ('$')
            if(n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = finish_ch
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1


          case default
            if(n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = iachar(re(re_loc:re_loc))
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

        end select
      else if(escaped) then

        select case(re(re_loc:re_loc))
          case('(','|',')','*','+','?','\','.','^','$')
            escaped_chr = iachar(re(re_loc:re_loc))
          case('a')
            escaped_chr = alpha_ch
          case('d')
            escaped_chr = numeric_ch
          case('w')
            escaped_chr = word_ch
          case('s')
            escaped_chr = space_ch
          case('A')
            escaped_chr = n_alpha_ch
          case('D')
            escaped_chr = n_numeric_ch
          case('W')
            escaped_chr = n_word_ch
          case('S')
            escaped_chr = n_space_ch

          case default
            stop "Unrecognised escaped character"
        end select

        if(n_atom > 1) then
          n_atom = n_atom - 1
          pf(pf_loc:pf_loc) = cat_op
          pf_loc = pf_loc + 1
        end if
        pf(pf_loc:pf_loc) = escaped_chr
        pf_loc = pf_loc + 1
        n_atom = n_atom + 1
        escaped = .false.
      end if



      re_loc = re_loc + 1

    end do

    if(par_loc /= 1) stop "I think you've got unmatched parentheses"

    n_atom = n_atom - 1
    do while(n_atom > 0)
      pf(pf_loc:pf_loc) = cat_op
      pf_loc = pf_loc + 1
      n_atom = n_atom - 1
    end do

    do while(n_alt > 0)
      pf(pf_loc:pf_loc) = or_op
      pf_loc = pf_loc + 1
      n_alt = n_alt - 1
    end do

  end function re_to_post

  function post_to_nfa(postfix)
    type(state), pointer  ::  post_to_nfa
    integer,  intent(in)  ::  postfix(pf_buff_size)

    integer ::  pf_loc, s_loc
    type(frag_stack)      ::  stack(1000)
    type(frag),   pointer ::  stack_p, e1, e2, e
    type(state),  pointer ::  s => null()
    type(state),  pointer ::  matchstate
    type(state),  pointer ::  nullstate

    integer ::  matchloc, nullloc


    allocate(matchstate, nullstate)
    matchstate = new_state(match_st, null(), null())
    nullstate = new_state(null_st, null(), null())
    stack_p => stack(1)%elem
    pf_loc  = 1
    s_loc   = 1

    matchloc = loc(matchstate)
    nullloc = loc(nullstate)
    if(associated(post_to_nfa)) stop "post_to_nfa already associated"

    do while(postfix(pf_loc) /= null_st)
      s => null()
      select case( postfix(pf_loc) )

      case(cat_op)
          e2 => pop()
          e1 => pop()
          call patch(e1%out1, e2%start)
          call push(new_frag(e1%start, e2%out1))
          e1 => null()
          e2 => null()

        case(or_op)
          e2 => pop()
          e1 => pop()
          allocate(s)
          s = new_state( split_st, e1%start, e2%start )
          call push( new_frag(s, append(e1%out1, e2%out1)) )
          s => null()
          e1 => null()
          e2 => null()

        case(quest_op)
          e => pop()
          allocate(s)
          s = new_state( split_st, e%start, nullstate )
          call push( new_frag(s, append(e%out1, new_list(s, 2)))  )
          s => null()
          e => null()

        case(star_op)
          e => pop()
          allocate(s)
          s = new_state( split_st, e%start, nullstate )
          call patch(e%out1, s)
          call push( new_frag(s, new_list(s, 2))  )
          s => null()
          e => null()

        case(plus_op)
          e => pop()
          allocate(s)
          s = new_state( split_st, e%start, nullstate )
          call patch(e%out1, s)
          call push( new_frag(e%start, new_list(s, 2))  )
          s => null()
          e => null()

        case default
          allocate(s)
          s = new_state( postfix(pf_loc), nullstate, nullstate )
          call push( new_frag(s, new_list(s, 1)) )
          s => null()
          e => null()

      end select
      pf_loc = pf_loc + 1
    end do

    e => pop()

    if(s_loc /= 1) stop "Stack is not empty on exit"
    call patch(e%out1, matchstate)

    post_to_nfa => e%start

    if(matchstate%c /= match_st) stop "***** Matchstate has changed!"
    if(nullstate%c /= -1) stop "***** Nullstate has changed!"
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

  function run_nfa(nfa, str, start, finish) result(res)
    logical :: res
    type(state),      intent(inout),  pointer   ::  nfa
    character(len=*), intent(in)                ::  str
    integer,          intent(in)                ::  start
    integer,          intent(out),    optional  ::  finish

    type  ::  list
      type(state),  pointer ::  s
    end type list

    type(list), allocatable, target ::  l1(:), l2(:)
    integer                         ::  list_id

    type(list), pointer ::  c_list(:), n_list(:), t(:)
    integer ::  ch_loc, n_cl, n_nl, n_t
    integer ::  ierr

    allocate(l1(1:n_states), l2(1:n_states), stat=ierr)
    if(ierr /= 0) stop "Error allocating l1,l2 in run_nfa"

    list_id = 0
    n_cl = 1
    n_nl = 1

    c_list => start_list(l1, n_cl, nfa)
    n_list => l2

    ch_loc = start

    do while (ch_loc <= len(str)+1)
      call step()
      t      => c_list
      c_list => n_list
      n_list => t
      n_t  = n_cl
      n_cl = n_nl
      n_nl = n_t
      if( is_match(c_list, n_cl) ) then
        res = .true.
        return
      end if
      ch_loc = ch_loc + 1
    end do

    res = .false.

  contains

    function start_list(l, n_l, s)
      type(list), pointer ::  start_list(:)
      type(list),   target,   intent(inout)  ::  l(:)
      integer,                intent(inout)  ::  n_l
      type(state),  pointer,  intent(inout)  ::  s

      n_l = 1
      list_id = list_id + 1
      start_list => l

      call add_state(start_list, n_l, s)

    end function start_list

    subroutine step()
      integer ::  i
      type(state),  pointer ::  s

      list_id = list_id + 1
      n_nl = 1

      do i=1, n_cl-1
        s => c_list(i)%s

        if(ch_loc <= len(str)) then
          select case(s%c)

            case(0:255)
              if( s%c == iachar(str(ch_loc:ch_loc)) ) call add_state(n_list, n_nl, s%out1)

            case(any_ch)
              call add_state(n_list, n_nl, s%out1)
            case(alpha_ch)
              select case( str(ch_loc:ch_loc) )
                case("a":"z","A":"Z")
                  call add_state(n_list, n_nl, s%out1)
              end select
            case(numeric_ch)
              select case( str(ch_loc:ch_loc) )
                case("0":"9")
                  call add_state(n_list, n_nl, s%out1)
              end select
            case(word_ch)
              select case( str(ch_loc:ch_loc) )
                case("a":"z","A":"Z","0:9","_")
                  call add_state(n_list, n_nl, s%out1)
              end select
            case(space_ch)
              select case( str(ch_loc:ch_loc) )
                case(" ", achar(9), achar(10))
                  call add_state(n_list, n_nl, s%out1)
              end select

            case(n_alpha_ch)
              select case( str(ch_loc:ch_loc) )
                case("a":"z","A":"Z")
                case default
                  call add_state(n_list, n_nl, s%out1)
              end select
            case(n_numeric_ch)
              select case( str(ch_loc:ch_loc) )
                case("0":"9")
                case default
                  call add_state(n_list, n_nl, s%out1)
              end select
            case(n_word_ch)
              select case( str(ch_loc:ch_loc) )
                case("a":"z","A":"Z","0:9","_")
                case default
                  call add_state(n_list, n_nl, s%out1)
              end select
            case(n_space_ch)
              select case( str(ch_loc:ch_loc) )
                case(" ", achar(9), achar(10))
                case default
                  call add_state(n_list, n_nl, s%out1)
              end select

            case(start_ch)
              if(ch_loc == start) call add_state(n_list, n_nl, s%out1)

            case(finish_ch)

            case default
              stop "Unrecognised state!"
          end select
        else
          if(s%c == finish_ch) then
            if(ch_loc == len(str)+1) call add_state(n_list, n_nl, s%out1)
          end if
        end if
      end do

    end subroutine step

    function is_match(l, n_l)
      logical ::  is_match
      type(list), pointer,  intent(in)  ::  l(:)
      integer,              intent(in)  ::  n_l

      integer ::  i

      do i = 1, n_l-1
        if( l(i)%s%c == match_st ) then
          is_match = .true.
          return
        end if
      end do
      is_match = .false.

    end function is_match

    recursive subroutine add_state(l, n_l, s)
      type(list),   pointer,  intent(inout) ::  l(:)
      integer,                intent(inout) ::  n_l
      type(state),  pointer,  intent(inout) ::  s

      if( (s%c == null_st) .or. (s%last_list == list_id) ) return
      s%last_list = list_id
      if(s%c == split_st) then
        call add_state(l, n_l, s%out1)
        call add_state(l, n_l, s%out2)
        return
      end if
      l(n_l)%s => s
      n_l = n_l + 1

    end subroutine add_state

  end function run_nfa

  function re_match_logical(re, str)
    logical :: re_match_logical
    character(len=*), intent(in)  ::  re
    character(len=*), intent(in)  ::  str

    integer               ::  postfix(pf_buff_size)
    type(state),  pointer ::  nfa
    integer ::  istart

    n_states = 0

    istart = 1

    if(len_trim(re) < 1) stop "Regular expression cannot be of length 0"
    postfix = re_to_post(trim(re))
    nfa => post_to_nfa(postfix)

!    print *, " "
!    call print_state(nfa)

    if(re(1:1) == "^") then
      re_match_logical = run_nfa(nfa, trim(str), start=1)
    else
      do istart = 1, len_trim(str)
        re_match_logical = run_nfa(nfa, trim(str), start=istart)
        if(re_match_logical) return
      end do
    end if

  end function re_match_logical

end module regex

program re_test
  use regex
  use io
  implicit none

  type(state), pointer ::  s
  type(state), pointer  ::  next
  character(len=120), allocatable :: args(:)
  character(len=120) :: re, str
  character(len=120) :: re_number

  call io_initialise(args = args)
  re = args(1)
  str = args(2)

  print *, "String:  ", trim(str)

  print *, "Regex:   ", trim(re)

  print *, "Match:  ", re_match(re, str)

end program re_test
