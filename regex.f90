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
!------------------------------------------------------------------------------!
! Author:  Edward Higgins <ed.j.higgins@gmail.com>                             !
!------------------------------------------------------------------------------!
! Version: 0.2.1, 2017-12-02                                                   !
!------------------------------------------------------------------------------!
! This code is distributed under the MIT license.                              !
!==============================================================================!

module regex

  implicit none

  private

  public :: re_match, re_match_str, re_split, re_replace

  integer,  parameter ::  pf_buff_size    = 8000  ! Maximum size of the postfix buffer
  integer,  parameter ::  pf_stack_size   = 4000  ! Maximum size of the postfix stack
  integer,  parameter ::  nfa_max_print   = 16    ! Maximum depth for print_state
  integer,  parameter ::  max_paren_depth = 100   ! Maximum depth of nested ()'s

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
  integer,  parameter ::  open_par_op  = 306 ! ( operator (for constructing match list)
  integer,  parameter ::  close_par_op = 307 ! ) operator (for constructing match list)

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

  ! List of parentheses for building the postfix
  type  ::  paren_list
    integer ::  n_atom
    integer ::  n_alt
  end type paren_list

  ! Full NFA and list of states
  type, public :: nfa_type
    type(state),    pointer :: head
    type(ptr_list), pointer :: states => null()
    integer                 :: n_states
  end type nfa_type

  ! State in the NFA
  type, public  :: state
    integer               ::  c
    type(state),  pointer ::  out1 => null()
    type(state),  pointer ::  out2 => null()
    integer               ::  last_list
  end type state

  ! List of pointers to states
  type  :: ptr_list
    type(state),    pointer ::  s    => null()
    integer                 ::  side =  -1
    type(ptr_list), pointer ::  next => null()
    integer                 ::  refs =  0
  end type ptr_list

  ! NFA fragment
  type  :: frag
    type(state),    pointer ::  start => null()
    type(ptr_list), pointer ::  out1  => null()
  end type frag

  ! Fragment stack node
  type  :: frag_stack
    type(frag), pointer ::  elem
  end type frag_stack

!EJH!   integer ::  submatch_pars(pf_stack_size)

contains
  !------------------------------------------------------------------------------!
    subroutine print_pf(pf)                                                      !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to print out a postfix expression in a human readable manner       !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   integer, intent(in) :: pf(:)                                               !
  !     Postfix expression stored as an array of integers                        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    integer,  intent(in)  ::  pf(:)

    integer ::  i

    print_loop: do i = 1, size(pf)
      select case(pf(i))
        case(null_st)
          exit print_loop
        case(1:255)
          write(*,'(A7,A4)') achar(pf(i)) // "   "
        case(open_par_op)
          write(*,'(A7,A5)') "OP ( "
        case(close_par_op)
          write(*,'(A7,A5)') "CL ) "
        case(cat_op)
          write(*,'(A7,A5)') "CAT  "
        case(plus_op)
          write(*,'(A7,A5)') "PLUS "
        case(or_op)
          write(*,'(A7,A5)') "OR   "
        case(quest_op)
          write(*,'(A7,A5)') "QUE  "
        case(star_op)
          write(*,'(A7,A5)') "STAR "

        case(split_st)
          write(*,'(A7,A5)') "SPLIT"
        case(match_st)
          write(*,'(A7,A5)') "MATCH"
        case(any_ch)
          write(*,'(A7,A5)') ".    "
        case(start_ch)
          write(*,'(A7,A5)') "START"
        case(finish_ch)
          write(*,'(A7,A5)') "FIN  "
        case(alpha_ch)
          write(*,'(A7,A5)') "\a   "
        case(numeric_ch)
          write(*,'(A7,A5)') "\d   "
        case(word_ch)
          write(*,'(A7,A5)') "\w   "
        case(space_ch)
          write(*,'(A7,A5)') "\s   "
        case(n_alpha_ch)
          write(*,'(A7,A5)') "\A   "
        case(n_numeric_ch)
          write(*,'(A7,A5)') "\D   "
        case(n_word_ch)
          write(*,'(A7,A5)') "\W   "
        case(n_space_ch)
          write(*,'(A7,A5)') "\S   "
        case default
          write(*,'(A22,I4)') "Unrecognised character", pf(i)
          stop
      end select
    end do print_loop
  end subroutine print_pf

  !------------------------------------------------------------------------------!
    recursive subroutine print_state(s, depth)                                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to print out an NFA state in a human readable manner. It is        !
  !   recursively called on all outputs of the state until nfa_max_print is      !
  !   reached.                                                                   !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(state), pointer, intent(in) :: s                                      !
  !     State to be printed                                                      !
  !                                                                              !
  !   integer, optional ,   intent(in) :: depth = 0                              !
  !     Depth of the state into the NFA                                          !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    type(state), pointer, intent(in) ::  s
    integer,  optional,   intent(in) :: depth

    integer ::  local_depth, i
    type(state), pointer  ::  tmp_s

    local_depth=0
    if (present(depth)) then
      local_depth = depth
    end if

    if (local_depth > nfa_max_print) then
      print *, "Trying to print a superdeep structure!"
    else
      tmp_s => s
      if (tmp_s%c /= null_st) then
        do i = 1, local_depth
          write(*,'(A3)', advance="no") "|  "
        end do
        select case (tmp_s%c)
        case(1:255)
          write(*,'(A7,A4)') "State: ", achar(tmp_s%c) // "   "
        case(split_st)
          write(*,'(A7,A5)') "State: ", "SPLIT"
        case(match_st)
          write(*,'(A7,A5)') "State: ", "MATCH"
        case(open_par_op)
          write(*,'(A7,A5)') "State: ", "OP ( "
        case(close_par_op)
          write(*,'(A7,A5)') "State: ", "CL ) "
        case(any_ch)
          write(*,'(A7,A5)') "State: ", ".    "
        case(start_ch)
          write(*,'(A7,A5)') "State: ", "START"
        case(finish_ch)
          write(*,'(A7,A5)') "State: ", "FIN  "
        case(alpha_ch)
          write(*,'(A7,A5)') "State: ", "\a   "
        case(numeric_ch)
          write(*,'(A7,A5)') "State: ", "\d   "
        case(word_ch)
          write(*,'(A7,A5)') "State: ", "\w   "
        case(space_ch)
          write(*,'(A7,A5)') "State: ", "\s   "
        case(n_alpha_ch)
          write(*,'(A7,A5)') "State: ", "\A   "
        case(n_numeric_ch)
          write(*,'(A7,A5)') "State: ", "\D   "
        case(n_word_ch)
          write(*,'(A7,A5)') "State: ", "\W   "
        case(n_space_ch)
          write(*,'(A7,A5)') "State: ", "\S   "
        case default
          stop "Unrecognised character in print_state"
      end select
      end if
      if (associated(tmp_s%out1)) call print_state(tmp_s%out1, depth=local_depth+1)
      if (associated(tmp_s%out2)) call print_state(tmp_s%out2, depth=local_depth+1)
    end if

  end subroutine print_state

  !------------------------------------------------------------------------------!
    function new_list(outp, side)                                                !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to create a new state list, with outp as the first state.          !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(state),    pointer,  intent(in)  ::  outp                             !
  !     First NFA state in the list                                              !
  !                                                                              !
  !   integer,                  intent(in)  ::  side                             !
  !     Which side of the the state goes on                                      !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   type(ptr_list), pointer                                                    !
  !     Pointer to the newly created list                                        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    type(ptr_list), pointer :: new_list
    type(state),    pointer,  intent(in)  ::  outp
    integer,                  intent(in)  ::  side

    integer ::  ierr

    new_list => null()

    allocate(new_list, stat=ierr)
    if (ierr /= 0) stop "Unable to allocate new_list"

    new_list%s    => outp
    new_list%side =  side
    new_list%next => null()
    new_list%refs =  0

  end function new_list

  recursive subroutine nullify_list(l,name)
    type(ptr_list), pointer, intent(inout) :: l
    character(len=*) , intent(in) :: name

    if (associated(l)) then
      l%refs=l%refs-1

      if(l%refs == 0) then
        if(associated(l%next)) call nullify_list(l%next,"l%next")
        deallocate(l)
      end if

      l => null()
    end if

  end subroutine nullify_list

  subroutine point_list(from, to,name)
    type(ptr_list), pointer, intent(in) :: from
    type(ptr_list), pointer, intent(inout) :: to
    character(len=*), intent(in) :: name

    if(associated(to)) call nullify_list(to,"to")

    if(associated(from)) then
      to => from
      to%refs = to%refs + 1
    endif

  end subroutine point_list

  !------------------------------------------------------------------------------!
    subroutine append(l1, l2)                                                    !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to append ptr_list l2 to the end of ptr_list l1.                   !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list), pointer,  intent(inout) :: l1                              !
  !     list to be appended to                                                   !
  !                                                                              !
  !   type(ptr_list), pointer,  intent(in)    :: l2                              !
  !     list to be appended                                                      !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   type(ptr_list), pointer                                                    !
  !     resultant list                                                           !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    type(ptr_list), pointer,  intent(inout) :: l1
    type(ptr_list), pointer,  intent(in)    :: l2

    type(ptr_list), pointer :: tmp_l

    tmp_l => null()

    call point_list(l1, tmp_l," tmp_l")
    do while ( associated(tmp_l%next) )
      call point_list(tmp_l%next, tmp_l," tmp_l")
    end do

    call point_list(l2, tmp_l%next," tmp_l%next")

    call nullify_list(tmp_l,"tmp_l")

  end subroutine append

  !------------------------------------------------------------------------------!
    subroutine deallocate_list(l, keep_states, n_states)                         !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to deallocate a ptr_list and, optionally, the NFA states in it.    !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list), pointer,  intent(inout) ::  l                              !
  !     Pointer list to be deallocated                                           !
  !                                                                              !
  !   logical,        optional, intent(in)    ::  keep_states = false            !
  !     Whether or not the states within the list should be deallocated as well  !
  !                                                                              !
  !   integer,        optional, intent(inout) ::  n_states = 0                   !
  !     Number of allocated states in the list                                   !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    type(ptr_list), pointer,  intent(inout) ::  l
    logical,        optional, intent(in)    ::  keep_states
    integer,        optional, intent(inout) ::  n_states

    type(ptr_list), pointer ::  tmp_l
    logical ::  local_ks
    integer ::  ierr

    tmp_l => null()
    local_ks = .false.
    if (present(keep_states)) local_ks = keep_states

    if (.not. associated(l)) return

    do while (associated(l%next))
      call point_list(l, tmp_l," tmp_l")
      call point_list(tmp_l%next, l," l")
      if ((associated(tmp_l%s)) .and. (.not. local_ks)) then
        deallocate(tmp_l%s)
        if (present(n_states)) n_states = n_states - 1
      else
        tmp_l%s => null()
      end if
!EJH!       call nullify_list(tmp_l%next,"tmp_l%next")
      call nullify_list(tmp_l,"tmp_l")
    end do

    if ((associated(l%s)) .and. (.not. local_ks)) then
      deallocate(l%s, stat=ierr)
      if (ierr /= 0) stop "Unable to deallocate l%s"
      if (present(n_states)) n_states = n_states - 1
    else
      l%s => null()
    end if

!EJH!     call nullify_list(tmp_l,"tmp_l")

    call nullify_list(l,"l")

  end subroutine deallocate_list

  !------------------------------------------------------------------------------!
    subroutine patch(l, s)                                                       !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to append state s to every dangling output in ptr_list l.          !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(ptr_list), pointer, intent(inout)  ::  l                              !
  !     List to be patched                                                       !
  !                                                                              !
  !   type(state),    pointer, intent(in)     ::  s                              !
  !     state with which to patch the list                                       !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    type(ptr_list), pointer, intent(inout)  ::  l
    type(state),    pointer, intent(in)     ::  s

    type(ptr_list), pointer :: tmp_l

    tmp_l => null()

    call point_list(l, tmp_l," tmp_l")
    do while ( associated(tmp_l) )
      select case(tmp_l%side)
        case(1)
          tmp_l%s%out1 => s
        case(2)
          tmp_l%s%out2 => s
        case default
          stop "Unexpected value of side"
      end select
      call point_list(tmp_l%next, tmp_l," tmp_l")
    end do

    call nullify_list(tmp_l,"tmp_l")

  end subroutine patch

  !------------------------------------------------------------------------------!
    function re_to_pf(re) result(pf)                                             !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to convert a regular expression string to a a postfix expression,  !
  !   stored in an array of integers.                                            !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*),   intent(in) :: re
  !     Regular expression to be converted to postfix                            !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   integer ::  pf(pf_buff_size)                                               !
  !     Postfix expression, stored as an array of integers                       !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    integer ::  pf(pf_buff_size)
    character(len=*),   intent(in) :: re

    integer          :: n_alt, n_atom
    integer          :: re_loc, pf_loc
    type(paren_list) :: paren(max_paren_depth)
    integer          :: par_loc
    logical          :: escaped
    integer          :: escaped_chr

    par_loc = 1
    re_loc  = 1
    pf_loc  = 1
    n_alt   = 0
    n_atom  = 0
    escaped = .false.

    pf = null_st

    if (len_trim(re) > pf_buff_size/2) stop "Regex too long!"
    do while (re_loc <= len_trim(re))
      if (.not. escaped) then
        select case(re(re_loc:re_loc))
          case('\')
            escaped = .true.

          case('(')
            if (n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            if (par_loc > size(paren)) stop "Too many embedded brackets!"
            paren(par_loc)%n_alt  = n_alt
            paren(par_loc)%n_atom = n_atom
            par_loc = par_loc + 1
            n_alt   = 0
            n_atom  = 0

          case('|')
            if (n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."

            n_atom = n_atom - 1
            do while (n_atom > 0)
              pf(pf_loc) = cat_op
              pf_loc = pf_loc + 1
              n_atom = n_atom - 1
            end do
            n_alt = n_alt + 1

          case (')')
            if (par_loc == 1) stop "I think you have an unmatched paren? maybe?"
            if (n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."

            n_atom = n_atom - 1
            do while (n_atom > 0)
              pf(pf_loc) = cat_op
              pf_loc = pf_loc + 1
              n_atom = n_atom - 1
            end do

            do while (n_alt > 0)
              pf(pf_loc) = or_op
              pf_loc = pf_loc + 1
              n_alt = n_alt - 1
            end do

            par_loc = par_loc - 1
            n_alt = paren(par_loc)%n_alt
            n_atom = paren(par_loc)%n_atom
            n_atom = n_atom + 1

          case('*')
            if (n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."
            pf(pf_loc) = star_op
            pf_loc = pf_loc + 1

          case('+')
            if (n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."
            pf(pf_loc) = plus_op
            pf_loc = pf_loc + 1

          case('?')
            if (n_atom == 0) stop "N_atom is 0. Apparently that's a bad thing..."
            pf(pf_loc) = quest_op
            pf_loc = pf_loc + 1

          case ('.')
            if (n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = any_ch
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

          case ('^')
            if (n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = start_ch
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

          case ('$')
            if (n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = finish_ch
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

          case(' ', achar(9))

          case default
            if (n_atom > 1) then
              n_atom = n_atom - 1
              pf(pf_loc:pf_loc) = cat_op
              pf_loc = pf_loc + 1
            end if
            pf(pf_loc:pf_loc) = iachar(re(re_loc:re_loc))
            pf_loc = pf_loc + 1
            n_atom = n_atom + 1

        end select
      else if (escaped) then

        select case(re(re_loc:re_loc))
          case('(','|',')','*','+','?','\','.','^','$',' ',achar(9))
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

        if (n_atom > 1) then
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

    if (par_loc /= 1) stop "I think you've got unmatched parentheses"

    n_atom = n_atom - 1
    do while (n_atom > 0)
      pf(pf_loc:pf_loc) = cat_op
      pf_loc = pf_loc + 1
      n_atom = n_atom - 1
    end do

    do while (n_alt > 0)
      pf(pf_loc:pf_loc) = or_op
      pf_loc = pf_loc + 1
      n_alt = n_alt - 1
    end do

  end function re_to_pf

  !------------------------------------------------------------------------------!
    subroutine allocate_nfa(nfa)                                                 !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to allocate and initialise the constituent parts of the nfa type.  !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa), intent(inout) :: nfa                                            !
  !     Finite automaton to be allocated                                         !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-12-29                                                 !
  !------------------------------------------------------------------------------!
    type(nfa_type), intent(inout) :: nfa

    nfa%head => null()
    nfa%states => null()
    call point_list(new_list(null(), 0), nfa%states," nfa%states")
    nfa%n_states = 0

  end subroutine allocate_nfa

  !------------------------------------------------------------------------------!
    subroutine deallocate_nfa(nfa)                                               !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to deallocate the constituent parts of the nfa type.               !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa), intent(inout) :: nfa                                            !
  !     Finite automaton to be allocated                                         !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-12-29                                                 !
  !------------------------------------------------------------------------------!
    type(nfa_type), intent(inout) :: nfa

    call deallocate_list(nfa%states, keep_states=.false., n_states = nfa%n_states)
    nfa%head => null()
    if (nfa%n_states /= 0) stop "Some states are still allocated!"

  end subroutine deallocate_nfa

  !------------------------------------------------------------------------------!
    function pf_to_nfa(postfix) result(nfa)                                      !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to convert a postfix expression to a Nondeterministic Finite       !
  !   Automaton, with the head stored in state 'states'                          !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   integer, intent(in) :: postfix(pf_buff_size)                               !
  !     Postfix expression stored as an array of integers                        !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   type(nfa)                                                                  !
  !     Resultant NFA                                                            !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    type(nfa_type)  ::  nfa
    integer,  intent(in)  ::  postfix(pf_buff_size)

    integer ::  pf_loc, s_loc
    type(frag_stack), allocatable ::  stack(:), allocated_frags(:)
    type(frag),     pointer ::  stack_p, e1, e2, e
    type(state),    pointer ::  s
    type(state),    pointer ::  matchstate
    type(state),    pointer ::  nullstate

    integer ::  nfrags, i, ierr

    call allocate_nfa(nfa)

    nfrags = 0
    allocate(allocated_frags(pf_stack_size), stat=ierr)
    if (ierr /= 0) stop "Unable to allocate frag stack"
    allocate(stack(pf_stack_size),stat=ierr)
    if (ierr /= 0) stop "Unable to allocate stack"


    do i = 1, pf_stack_size
      stack(i)%elem => null()
      allocated_frags(i)%elem => null()
    end do

    if (nfa%states%side /= 0) stop "Trying to build nfa with in-use states"

    matchstate => new_state(match_st, null(), null())
    nullstate => new_state(null_st, null(), null())

    stack_p => stack(1)%elem
    pf_loc  = 1
    s_loc   = 1

    do while (postfix(pf_loc) /= null_st)
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
          s => new_state( split_st, e1%start, e2%start )
          call append(e1%out1, e2%out1)
          call push( new_frag(s, e1%out1) )
          e1 => null()
          e2 => null()

        case(quest_op)
          e => pop()
          s => new_state( split_st, e%start, nullstate )
          call append(e%out1, new_list(s,2))
          call push( new_frag(s, e%out1) )
          e => null()

        case(star_op)
          e => pop()
          s => new_state( split_st, e%start, nullstate )
          call patch(e%out1, s)
          call push( new_frag(s, new_list(s, 2))  )
          e => null()

        case(plus_op)
          e => pop()
          s => new_state( split_st, e%start, nullstate )
          call patch(e%out1, s)
          call push( new_frag(e%start, new_list(s, 2))  )
          e => null()

        case default
          s => new_state( postfix(pf_loc), nullstate, nullstate )
          call push( new_frag(s, new_list(s, 1)) )
          e => null()

      end select
      pf_loc = pf_loc + 1
    end do

    e => pop()

    if (s_loc /= 1) stop "Stack is not empty on exit"
    call patch(e%out1, matchstate)

    nfa%head => e%start

    if (matchstate%c /= match_st) stop "***** Matchstate has changed!"
    if (nullstate%c /= null_st) stop "***** Nullstate has changed!"

    do i = 1, nfrags
      if (associated(allocated_frags(i)%elem)) then
        call deallocate_list(allocated_frags(i)%elem%out1, keep_states=.true.)
        if (associated(allocated_frags(i)%elem%start)) allocated_frags(i)%elem%start => null()
        deallocate(allocated_frags(i)%elem, stat=ierr)
        if (ierr /= 0) stop "Unable to deallocate fragment"
        allocated_frags(i)%elem => null()
      end if
    end do

    deallocate(stack, allocated_frags, stat=ierr)
    if (ierr /= 0) stop "Unable to deallocate stacks"
    e => null()

  contains

    !------------------------------------------------------------------------------!
      function new_frag(s, l)                                                      !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to create a new NFA fragment.                                      !
    !------------------------------------------------------------------------------!
      type(frag), pointer ::  new_frag
      type(state),    pointer,  intent(in)  ::  s
      type(ptr_list), pointer,  intent(in)  ::  l

      allocate(new_frag)
      new_frag%start => s
      call point_list(l, new_frag%out1," new_frag%out1")

      nfrags = nfrags + 1
      allocated_frags(nfrags)%elem => new_frag

    end function new_frag

    !------------------------------------------------------------------------------!
      function new_state(c, out1, out2)                                            !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to create a new NFA state, outputting to out1 and out2.            !
    !------------------------------------------------------------------------------!
      type(state), pointer  ::  new_state
      integer,                intent(in)  ::  c
      type(state),  pointer,  intent(in)  ::  out1, out2

      integer ::  ierr

      new_state => null()
      allocate(new_state, stat=ierr)
      if (ierr /= 0) stop "Unable to allocate new_state"
      new_state%last_list = 0
      new_state%c = c
      new_state%out1 => out1
      new_state%out2 => out2

      call append(nfa%states, new_list(new_state, -1))
      nfa%n_states = nfa%n_states + 1

    end function new_state

    !------------------------------------------------------------------------------!
      subroutine push(f)                                                           !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to push NFA fragment onto the stack.                               !
    !------------------------------------------------------------------------------!
      type(frag), intent(in), pointer  ::  f

      s_loc = s_loc + 1
      stack(s_loc)%elem => f

    end subroutine push

    !------------------------------------------------------------------------------!
      function pop()                                                               !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to push an NFA off the stack, and returning it.                    !
    !------------------------------------------------------------------------------!
      type(frag), pointer :: pop

      pop => stack(s_loc)%elem
      s_loc = s_loc - 1

    end function pop

  end function pf_to_nfa

  !------------------------------------------------------------------------------!
    function run_nfa_fast(nfa, str, start, finish) result(res)                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to simulate the NFA 'nfa' o n the string 'str', starting 'start'   !
  !   characters in. This routine uses the fast algorithm. This algorithm        !
  !   doesn't allow submatching.                                                 !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa_type),   intent(inout)             ::  nfa                        !
  !     NFA to be simulated                                                      !
  !                                                                              !
  !   character(len=*), intent(in)                ::  str                        !
  !     String to be searched                                                    !
  !                                                                              !
  !   integer,          intent(inout)             ::  start                      !
  !     Where in str to start. On exit, returns the start of the match if        !
  !     matched                                                                  !
  !                                                                              !
  !   integer,          intent(out),    optional  ::  finish                     !
  !     Last character of matched string                                         !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   TRUE if there is a match, FALSE otherwise.                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    logical :: res
    type(nfa_type),   intent(inout)             ::  nfa
    character(len=*), intent(in)                ::  str
    integer,          intent(inout)             ::  start
    integer,          intent(out),    optional  ::  finish

    type  ::  list
      type(state),  pointer ::  s
    end type list

    type(list), allocatable, target ::  l1(:), l2(:)
    integer                         ::  list_id = 0
    integer ::  loc_start
    logical ::  no_advance

    type(list), pointer ::  c_list(:), n_list(:), t(:)
    integer ::  ch_loc, n_cl, n_nl, n_t
    integer ::  istart, i, ierr

    allocate(l1(1:nfa%n_states), l2(1:nfa%n_states), stat=ierr)
    if (ierr /= 0) stop "Error allocating l1,l2 in run_nfa_fast"

    start_loop: do istart = start, len(str)
      do i = 1, nfa%n_states
        l1(i)%s => null()
        l2(i)%s => null()
      end do

      n_cl = 1
      n_nl = 1

      c_list => start_list(l1, n_cl, nfa%head)
      n_list => l2

      ch_loc = istart
      loc_start = istart

      res = .false.
      if ( is_match(c_list, n_cl) ) then
        res = .true.
        if (present(finish)) finish = min(ch_loc, len(str))
      end if

      if (present(finish)) finish = -1
      do while (ch_loc <= len(str)+1)
        no_advance  = .false.
        call step()
        t      => c_list
        c_list => n_list
        n_list => t
        n_t  = n_cl
        n_cl = n_nl
        n_nl = n_t
        if ( is_match(c_list, n_cl) ) then
          res = .true.
          if (present(finish)) finish = min(ch_loc, len(str))
        end if
        if (.not. no_advance) ch_loc = ch_loc + 1
      end do
      if (res) exit start_loop
    end do start_loop

    if (res) start = loc_start
    deallocate(l1, l2)

  contains

    !------------------------------------------------------------------------------!
      function start_list(l, n_l, s)                                               !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to initialise a list of active states.                             !
    !------------------------------------------------------------------------------!
      type(list), pointer ::  start_list(:)
      type(list),   target,   intent(inout)  ::  l(:)
      integer,                intent(inout)  ::  n_l
      type(state),  pointer,  intent(inout)  ::  s

      n_l = 1
      list_id = list_id + 1
      start_list => l

      call add_state(start_list, n_l, s)

    end function start_list

    !------------------------------------------------------------------------------!
      subroutine step()                                                            !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to step through one node of the NFA for each state in the current  !
    !   list.                                                                      !
    !------------------------------------------------------------------------------!
      integer ::  i
      type(state),  pointer ::  s => null()

      list_id = list_id + 1
      n_nl = 1

      do i=1, n_cl-1
        s => c_list(i)%s

        if (ch_loc <= len(str)) then
          select case(s%c)

            case(0:255)
              if ( s%c == iachar(str(ch_loc:ch_loc)) ) then
                call add_state(n_list, n_nl, s%out1)
              end if

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
                case("a":"z","A":"Z","0":"9","_")
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
              if (ch_loc == 1) call add_state(n_list, n_nl, s%out1)
              no_advance = .true.

            case(open_par_op)
              call add_state(n_list, n_nl, s%out1)
              no_advance = .true.

            case(close_par_op)
              call add_state(n_list, n_nl, s%out1)
              no_advance = .true.

            case(finish_ch)

            case( match_st )

            case default
              print *, "Unrecognised state ", s%c
              stop
          end select
        else
          if (s%c == finish_ch) then
            call add_state(n_list, n_nl, s%out1)
          end if
        end if
      end do

    end subroutine step

    !------------------------------------------------------------------------------!
      function is_match(l, n_l)                                                    !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to check if any nodes in the list l are match states in the NFA.   !
    !------------------------------------------------------------------------------!
      logical ::  is_match
      type(list), pointer,  intent(in)  ::  l(:)
      integer,              intent(in)  ::  n_l

      integer ::  i

      do i = 1, n_l-1
        if ( l(i)%s%c == match_st ) then
          is_match = .true.
          return
        end if
      end do
      is_match = .false.

    end function is_match

    !------------------------------------------------------------------------------!
      recursive subroutine add_state(l, n_l, s)                                    !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to add the state s to the end of list l. If s is a split_st, add   !
    !   its output instead.                                                        !
    !------------------------------------------------------------------------------!
      type(list),   pointer,  intent(inout) ::  l(:)
      integer,                intent(inout) ::  n_l
      type(state),  pointer,  intent(inout) ::  s

      if ( (s%c == null_st) .or. (s%last_list == list_id) ) return
      s%last_list = list_id
      if (s%c == split_st) then
        call add_state(l, n_l, s%out1)
        call add_state(l, n_l, s%out2)
        return
      end if
      l(n_l)%s => s
      n_l = n_l + 1

    end subroutine add_state

  end function run_nfa_fast

  !------------------------------------------------------------------------------!
    recursive function run_nfa_full(nfa, str, start, finish, s_in) result(res)   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to simulate the NFA 'nfa' o n the string 'str', starting 'start'   !
  !   characters in. This routine uses the slower algorithm. This algorithm      !
  !   does allow submatching.                                                    !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   type(nfa_type),       intent(inout)           ::  nfa                      !
  !     NFA to be simulated                                                      !
  !                                                                              !
  !   character(len=*),     intent(in)              ::  str                      !
  !     String to be searched                                                    !
  !                                                                              !
  !   integer,              intent(inout)           ::  start                    !
  !     Where in str to start. On exit, returns the start of the match if        !
  !     matched                                                                  !
  !                                                                              !
  !   integer,              intent(out),  optional  ::  finish                   !
  !     Last character of matched string                                         !
  !                                                                              !
  !   type(state), pointer, intent(in),   optional  ::  s_in                     !
  !     Node to start on  is the start of the NFA or not.                        !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   TRUE if there is a match, FALSE otherwise.                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    logical :: res
    type(nfa_type),       intent(inout)           ::  nfa
    character(len=*),     intent(in)              ::  str
    integer,              intent(inout)           ::  start
    integer,              intent(out),  optional  ::  finish
    type(state), pointer, intent(in),   optional  ::  s_in

    type(state), pointer :: s
    integer ::  istart, fin

    res = .false.
    if (present(finish)) finish = -1
    fin = -1

    if (present(s_in)) then
      istart = start
      s => s_in
      call step()
    else
      start_loop: do istart = start, len(str)
        s => nfa%head
        call step()
        if (res) exit start_loop
      end do start_loop
    end if

    if (present(finish)) then
      if (finish == -1) finish = fin
    end if
    start = istart

  contains

    !------------------------------------------------------------------------------!
      recursive subroutine step()                                                  !
    !------------------------------------------------------------------------------!
    ! DESCRPTION                                                                   !
    !   Routine to step through the NFA. If it does not reach an end, run_nfa_full !
    !   is re-called.                                                              !
    !------------------------------------------------------------------------------!
      integer ::  next_start

      next_start = -1
      if (istart <= len(str)) then
        select case(s%c)
          case( match_st )
            res = .true.
            if (present(finish)) finish = istart-1

          case( split_st )
            res = run_nfa_full(nfa, str, istart, fin, s_in = s%out1)
            if (.not. res) res = run_nfa_full(nfa, str, istart, fin, s_in = s%out2)

          case(0:255)
            if ( s%c == iachar(str(istart:istart)) ) then
              next_start = istart + 1
              res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end if

          case(any_ch)
            next_start = istart + 1
            res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
          case(alpha_ch)
            select case( str(istart:istart) )
              case("a":"z","A":"Z")
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select
          case(numeric_ch)
            select case( str(istart:istart) )
              case("0":"9")
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select
          case(word_ch)
            select case( str(istart:istart) )
              case("a":"z","A":"Z","0":"9","_")
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select
          case(space_ch)
            select case( str(istart:istart) )
              case(" ", achar(9), achar(10))
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select

          case(n_alpha_ch)
            select case( str(istart:istart) )
              case("a":"z","A":"Z")
              case default
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select
          case(n_numeric_ch)
            select case( str(istart:istart) )
              case("0":"9")
              case default
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select
          case(n_word_ch)
            select case( str(istart:istart) )
              case("a":"z","A":"Z","0:9","_")
              case default
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select
          case(n_space_ch)
            select case( str(istart:istart) )
              case(" ", achar(9), achar(10))
              case default
                next_start = istart + 1
                res = run_nfa_full(nfa, str, next_start, fin, s_in = s%out1)
            end select

          case(start_ch)
            if (start == 1) res = run_nfa_full(nfa, str, start, fin, s_in = s%out1)

          case(open_par_op)
            res = run_nfa_full(nfa, str, istart, fin, s_in = s%out1)

          case(close_par_op)
            res = run_nfa_full(nfa, str, istart, fin, s_in = s%out1)

          case(finish_ch)

          case default
            print *, "Unrecognised state ", s%c
            stop
        end select
      else
        select case(s%c)
          case( split_st )
            res = run_nfa_full(nfa, str, istart, fin, s_in = s%out1)
            if (.not. res) res = run_nfa_full(nfa, str, istart, fin, s_in = s%out2)
          case( match_st )
            res = .true.
            if (present(finish)) finish = len(str)
          case( finish_ch )
            res = run_nfa_full(nfa, str, istart, fin, s_in = s%out1)
        end select
      end if

    end subroutine step

  end function run_nfa_full


  !------------------------------------------------------------------------------!
    function re_match(re, str)                                                   !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to check a string str against a regular expression re.             !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str
  !     String to be searched                                                    !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   TRUE if there is a match, FALSE otherwise.                                 !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    logical :: re_match
    character(len=*), intent(in)  ::  re
    character(len=*), intent(in)  ::  str

    integer                 ::  postfix(pf_buff_size)
    logical                 ::  debug = .false.
    type(nfa_type)          ::  nfa
    integer ::  istart

    istart = 1

    if (len_trim(re) < 1) stop "Regular expression cannot be of length 0"
    postfix = re_to_pf(trim(re))
    if(debug) call print_pf(postfix)

    nfa = pf_to_nfa(postfix)
    if(debug) call print_state(nfa%head)

    re_match = run_nfa_full(nfa, trim(str), istart)

    call deallocate_nfa(nfa)

  end function re_match

  !------------------------------------------------------------------------------!
    function re_match_str(re, str)                                               !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to get a substring from str that matches the regular expression re.!
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str                                      !
  !     String to be searched                                                    !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   The matching string if there is a match, an empty string otherwise.        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    character(len=pf_buff_size) :: re_match_str
    character(len=*), intent(in)  ::  re
    character(len=*), intent(in)  ::  str

    integer                 ::  postfix(pf_buff_size)
    type(nfa_type)          ::  nfa
    integer ::  istart, ifin
    logical :: match

    istart = 1
    ifin = -1

    re_match_str = " "

    if (len_trim(re) < 1) stop "Regular expression cannot be of length 0"
    postfix = re_to_pf(trim(re))
    nfa = pf_to_nfa(postfix)

    match = run_nfa_fast(nfa, trim(str), istart, finish=ifin)
    if (match) re_match_str = str(istart:ifin)

    call deallocate_nfa(nfa)

  end function re_match_str

  !------------------------------------------------------------------------------!
    subroutine re_split(re, str, output)                                         !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to split a string into an array of substrings, based on the regular!
  !   expression re.                                                             !
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str                                      !
  !     String to be searched                                                    !
  !                                                                              !
  !   character(len=*), intent(inout), allocatable   :: output(:)                !
  !     Array containing the substrings. This will be (re)allocated within this  !
  !     routine to the size of the number of matches.                            !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    character(len=*), intent(in)  ::  re
    character(len=*), intent(in)  ::  str
    character(len=*), intent(inout), allocatable   :: output(:)

    type(nfa_type)          ::  nfa
    integer                 ::  postfix(pf_buff_size)
    logical                 ::   is_match

    integer :: istart, fin, isplit, last_fin, n_splits

    istart = 1


    if (len_trim(re) < 1) stop "Regular expression cannot be of length 0"
    postfix = re_to_pf(trim(re))
    nfa = pf_to_nfa(postfix)

    istart = 1
    isplit = 1
    n_splits = 0

    is_match = run_nfa_full(nfa, trim(str), istart, finish=fin)
    if (is_match) then
      n_splits = n_splits + 1
      last_fin = fin
      istart = last_fin+1
      isplit = 2
      do while (istart <= len_trim(str))
        is_match = run_nfa_full(nfa, trim(str), istart, finish=fin)
        if (.not. is_match) exit
        n_splits = n_splits + 1
        last_fin = fin
        isplit = isplit + 1
        istart = last_fin+1
      end do
      if (last_fin <= len_trim(str)) n_splits = n_splits + 1
    end if

    if (n_splits == 0) return

    if (allocated(output)) deallocate(output)
    allocate(output(n_splits))

    istart = 1
    isplit = 1
    output = " "

    is_match = run_nfa_fast(nfa, trim(str), istart, finish=fin)
    if (is_match) then
      output(1) = str(1:istart-1)
      last_fin = fin
      istart = last_fin+1
      isplit = 2
      do while (istart <= len_trim(str))
        is_match = run_nfa_fast(nfa, trim(str), istart, finish=fin)
        if (.not. is_match) exit
        output(isplit) = str(last_fin+1:istart-1)
        last_fin = fin
        isplit = isplit + 1
        istart = last_fin+1
      end do
      if (last_fin < len_trim(str)) output(isplit) = str(last_fin+1:)
    end if

    call deallocate_nfa(nfa)

  end subroutine re_split

  !------------------------------------------------------------------------------!
    function re_replace(re, repl, str)                                           !
  !------------------------------------------------------------------------------!
  ! DESCRPTION                                                                   !
  !   Routine to replace each occurance of re with repl in str                  .!
  !------------------------------------------------------------------------------!
  ! ARGUMENTS                                                                    !
  !   character(len=*), intent(in)  ::  re                                       !
  !     Regualr expression to be matched                                         !
  !                                                                              !
  !   character(len=*), intent(in)  ::  repl                                     !
  !     String to replace the regular expression                                 !
  !                                                                              !
  !   character(len=*), intent(in)  ::  str                                      !
  !     String to be searched                                                    !
  !------------------------------------------------------------------------------!
  ! RETURNS                                                                      !
  !   The matching string if there is a match, an empty string otherwise.        !
  !------------------------------------------------------------------------------!
  ! AUTHORS                                                                      !
  !   Edward Higgins, 2016-02-01                                                 !
  !------------------------------------------------------------------------------!
    character(len=pf_buff_size) :: re_replace
    character(len=*), intent(in)  ::  re
    character(len=*), intent(in)  ::  repl
    character(len=*), intent(in)  ::  str

    integer                 ::  postfix(pf_buff_size)
    type(nfa_type)          ::  nfa
    integer ::  istart, ifin, last_fin, rep_ptr
    logical :: match

    istart = 1
    ifin = -1
    last_fin = 0
    rep_ptr = 0

    re_replace = " "

    if (len_trim(re) < 1) stop "Regular expression cannot be of length 0"
    postfix = re_to_pf(trim(re))
    nfa = pf_to_nfa(postfix)

    match = run_nfa_fast(nfa, trim(str), istart, finish=ifin)
    if (match) then
      re_replace = str(1:istart-1) // repl
      rep_ptr = istart + len(repl)-1
      last_fin = ifin
    end if

    do while (ifin <= len(str))
      istart=ifin+1
      match = run_nfa_fast(nfa, trim(str), istart, finish=ifin)
      if (match) then
        re_replace = re_replace(1:rep_ptr) // str(last_fin+1:istart-1) // repl
        rep_ptr = rep_ptr + (istart-last_fin+1) + len(repl)-2
        last_fin = ifin
      else
        exit
      end if
    end do

    re_replace = re_replace(1:rep_ptr) // str(last_fin+1:)

    call deallocate_nfa(nfa)

  end function re_replace

end module regex
