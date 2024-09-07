module nfa_table_mod
!==============================================================================#
! NFA_TABLE_MOD
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2024-09-07
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use nfa_mod
  implicit none

  private
  real,    parameter :: nfa_table_load_factor = 0.7
  integer, parameter :: max_name_len = 64

  type, public :: nfa_table
    type(nfa_table_entry), allocatable :: entries(:)
    integer                            :: num_entries
    integer                            :: capacity
  contains
    procedure, public :: init   => nfa_table_init
    procedure, public :: insert => nfa_table_insert
    procedure, public :: lookup => nfa_table_lookup
    procedure, public :: resize => nfa_table_resize
    procedure, public :: copy   => nfa_table_copy
    procedure, public :: free   => nfa_table_free
  end type nfa_table

  type :: nfa_table_entry
    character(len=max_name_len) :: nfa_name
    type(nfa_type), pointer     :: nfa
  end type nfa_table_entry

contains

  subroutine nfa_table_init(this, initial_capacity)
    class(nfa_table), intent(inout)          :: this
    integer,          intent(in),   optional :: initial_capacity

    integer :: capacity

    capacity = 1
    if (present(initial_capacity)) capacity = initial_capacity

    allocate(this%entries(capacity))
    this%capacity = capacity
    this%num_entries = 0

  end subroutine nfa_table_init

  subroutine nfa_table_insert(this, nfa_name, nfa)
    class(nfa_table),           intent(inout) :: this
    character(len=*),           intent(in)    :: nfa_name
    type(nfa_type),   pointer,  intent(in)    :: nfa

    integer :: id

    if ((this%num_entries + 1) > nfa_table_load_factor*this%capacity) then
      call this%resize(this%capacity*2)
    end if

    id = nfa_table_hash(nfa_name, this%capacity)

    do while (this%entries(id)%nfa_name /= "")
      id = mod(id, this%capacity) + 1
    end do

    this%entries(id)%nfa_name = nfa_name
    this%entries(id)%nfa => nfa

  end subroutine nfa_table_insert

  function nfa_table_lookup(this, nfa_name) result(nfa)
    class(nfa_table), intent(in)  :: this
    character(len=*), intent(in) :: nfa_name

    type(nfa_type), pointer :: nfa

    integer :: id

    id = nfa_table_hash(nfa_name, this%capacity)

    do while (this%entries(id)%nfa_name /= nfa_name .and. this%entries(id)%nfa_name /= "")
      id = mod(id, this%capacity) + 1
    end do

    if (this%entries(id)%nfa_name == nfa_name) then
      nfa => this%entries(id)%nfa
    else
      nfa => null()
    end if

  end function nfa_table_lookup

  subroutine nfa_table_resize(this, new_capacity)
    class(nfa_table), intent(inout) :: this
    integer,         intent(in)    :: new_capacity

    type(nfa_table) :: old_table
    integer :: i

    old_table = this%copy()

    deallocate(this%entries)

    allocate(this%entries(new_capacity))
    this%capacity = new_capacity

    do i=1, old_table%capacity
      if (old_table%entries(i)%nfa_name /= "") then
        call this%insert(old_table%entries(i)%nfa_name, old_table%entries(i)%nfa)
      end if
    end do

    call old_table%free(free_nfas=.false.)

  end subroutine nfa_table_resize

  function nfa_table_copy(this) result(copy)
    class(nfa_table), intent(in) :: this
    type(nfa_table) :: copy

    integer :: i

    call copy%init(this%capacity)

    do i=1, this%capacity
      if (this%entries(i)%nfa_name /= "") then
        copy%entries(i)%nfa_name = this%entries(i)%nfa_name
        copy%entries(i)%nfa => this%entries(i)%nfa
      end if
    end do

    copy%num_entries = this%num_entries

  end function nfa_table_copy

  subroutine nfa_table_free(this, free_nfas)
    class(nfa_table), intent(inout)          :: this
    logical,         intent(in),   optional :: free_nfas
    
    integer :: i
    logical :: local_free_nfas

    local_free_nfas = .false.
    if (present(free_nfas)) local_free_nfas = free_nfas

    if (local_free_nfas) then
      do i=1, this%capacity
        call this%entries(i)%nfa%free()
      end do
    end if

    deallocate(this%entries)
    this%num_entries = 0
    this%capacity = 0

  end subroutine nfa_table_free

  function nfa_table_hash(str, table_size) result(hash)
    integer :: hash
    character(len=*), intent(in) :: str
    integer,          intent(in) :: table_size

    integer :: str_int, i
    real    :: A, hash_product

    str_int = 0
    do i=1, len_trim(str)
      str_int = str_int + iachar(str(i:i))
    end do

    hash_product = A*str_int
    hash = ior(ishft(int(2**32 * (hash_product - int(hash_product))), -nint(log(real(table_size))/log(2.0))), 0)

  end function nfa_table_hash

end module nfa_table_mod
