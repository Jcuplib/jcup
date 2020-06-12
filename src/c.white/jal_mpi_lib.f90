module jal_mpi_lib
  use mpi
  implicit none

!--------------------------------   public  ----------------------------------!

  public :: jal_init_window
  public :: jal_set_window_data
  public :: jal_get_window_data
  public :: jal_free_window
  
!--------------------------------   private  ---------------------------------!

  interface jal_init_window
     module procedure jal_init_window_leader
  end interface jal_init_window
  
  integer :: sizeint
  integer,pointer :: data_ptr(:)
  integer :: win
  integer :: my_comp
  logical :: is_leader

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine jal_init_window_leader(my_comp_id, window_size)
    use jcup_mpi_lib, only : jml_isLocalLeader, jml_GetCommLeader
    implicit none
    integer, intent(IN) :: my_comp_id
    integer, intent(IN) :: window_size
    integer :: ierror
    integer(kind=MPI_ADDRESS_KIND) :: size
    integer :: disp_unit

    my_comp = my_comp_id
    
    is_leader = jml_isLocalLeader(my_comp_id)
    
    if (.not.is_leader) return
    
    call MPI_type_size(MPI_INTEGER, sizeint, ierror)
    disp_unit = sizeint

    size = window_size*sizeint
    allocate(data_ptr(window_size))
   
    call MPI_win_create(data_ptr, size, disp_unit, MPI_INFO_NULL, jml_GetCommLeader(), win, ierror)
    
  end subroutine jal_init_window_leader
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine jal_set_window_data(data, n_data)
    implicit none
    integer, intent(IN) :: data(:)
    integer, intent(IN) :: n_data
    integer :: ierror

    if (.not.is_leader) return
    
    data_ptr(1:n_data) = data(1:n_data)
    
    !call MPI_win_fence(0, win, ierror)
    !call MPI_win_lock(MPI_LOCK_SHARED, target, MPI_MODE_NOCHECK, win, ierror)
    !call mpi_put(data, 1, MPI_INTEGER, target, 0, 1, MPI_INTEGER, win, ierror)
    !call MPI_win_unlock(target, win, ierror)
    !call MPI_win_fence(0, win, ierror)

  end subroutine jal_set_window_data
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine jal_get_window_data(target_rank, pos, data, n_data)
    use jcup_mpi_lib, only : jml_BcastLocal
    implicit none
    integer, intent(IN) :: target_rank
    integer, intent(IN) :: pos
    integer, intent(INOUT) :: data(:)
    integer, intent(IN) :: n_data
    integer :: ierror
    integer(kind=MPI_ADDRESS_KIND) :: tdisp

    if (is_leader) then
      tdisp = pos*sizeint
    
      !call MPI_win_fence(0, win, ierror)
      !if (is_my_comp) then
      !else
      !  call mpi_get(data, 1, MPI_INTEGER, target_rank, tdisp, 1, MPI_INTEGER, win, ierror)
      !  write(0, *) "jal_get_window_data ", data(1)
      !end if
      !call MPI_win_fence(0, win, ierror)

      call MPI_win_lock(MPI_LOCK_SHARED, target_rank, MPI_MODE_NOCHECK, win, ierror)
      call mpi_get(data, n_data, MPI_INTEGER, target_rank, tdisp, n_data, MPI_INTEGER, win, ierror)
      call MPI_win_unlock(target_rank, win, ierror)

    end if

    call jml_BcastLocal(my_comp, data, 1, n_data)
    
  end subroutine jal_get_window_data
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine jal_free_window()
    implicit none
    integer :: ierror
   
    if (.not.is_leader) return

    call MPI_win_free(win, ierror)

  end subroutine jal_free_window

!=======+=========+=========+=========+=========+=========+=========+=========+


end module jal_mpi_lib
