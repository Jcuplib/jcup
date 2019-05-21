!====================================================================================================
!> @brief
!> jcup inter node communcation  module
! 2017/02/8 [NEW]
! This module can not be used on overlapped component.

module jcup_intercomm
  use mpi
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_advanced_exchange ! subroutine (num_of_child)
  public :: init_intercomm ! subroutine ()
  public :: init_my_info   ! subroutine (send_comp_id, recv_comp_id, mapping_tag, num_of_pe, num_of_point)
  public :: set_my_info    ! subroutine (send_comp_id, recv_comp_id, mapping_tag, pe_num, offset, num_of_data)
  public :: set_boss_info  ! subroutine (send_comp_id, recv_comp_id, mapping_tag)
  public :: send_data_intercomm ! subroutine (send_comp_id, recv_comp_id, mapping_tag, data, num_of_data)
  public :: recv_data_intercomm ! subroutine (send_comp_id, recv_comp_id, mapping_tag, data, num_of_data)

!--------------------------------   private  ---------------------------------!
  integer :: num_of_child = 0    ! number of family members
  integer :: current_send_comp   ! send comp id of current sed/recv
  integer :: current_recv_comp   ! recv comp id of current send/recv
  integer :: current_mapping_tag ! maping tag of current send/recv
  integer :: my_comp_id
  integer :: my_size, my_rank, my_comm
  integer :: my_rank_global
  logical :: is_boss
  integer :: boss_rank
  integer :: family_comm  ! communicator for local boss family
  integer :: boss_comm    ! communicator for local bosses

  type target_info_type
    integer :: rank_num ! rank of target component
    integer :: offset ! data offset of local data buffer 
    integer :: target_grid_size ! number of target grid point per one data
    integer :: global_offset ! data offset to rearrange boss buffer, this variable is valid only in the boss
  end type

  type pe_info_type
    logical :: ofirst = .true.
    integer :: my_grid_size ! number of send/recv grid point of my area, my_grid_size = sum(target_grid_size)
    integer :: num_of_target ! number of pe of my target
    type(target_info_type), pointer :: target_info(:)
  end type

  type boss_info_type
     integer :: num_of_target_boss ! number of target boss for inter-node send/recv
     integer, pointer :: target_boss_rank(:) ! local rank of target boss for inter-node send/recv
     integer, pointer :: target_boss_grid_size(:) ! grid size of each target boss for inter-node send/recv
     integer, pointer :: target_boss_offset(:)    ! buffer offset of each target boss for inter-node send/recv

     integer :: num_of_target_child 
     integer, pointer :: child_grid_size(:) ! sum of target_grid_size per child = my_grid_size
     integer, pointer :: child_grid_size_tmp(:) ! = child_grid_size*num_of_data
     integer, pointer :: child_offset(:)    ! = child_offset(i-1)+child_grid_size(i-1)
     integer, pointer :: child_offset_tmp(:) ! = child_offset*num_of_data
     type(pe_info_type), pointer :: child_info(:)
  end type

  type boss_array_type
    type(boss_info_type), pointer :: boss_info(:)
    type(pe_info_type),   pointer :: my_info(:)
  end type

  type(pe_info_type), pointer   :: my_info_ptr
  type(boss_info_type), pointer :: boss_info_ptr

  type(boss_array_type), allocatable :: send_data(:)
  type(boss_array_type), allocatable :: recv_data(:)

  real(kind=8), allocatable :: local_buffer(:)
  real(kind=8), allocatable :: boss_buffer(:)
  real(kind=8), pointer :: temp_buffer(:)


  logical :: is_init = .false.

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set num_of_child
subroutine init_advanced_exchange(noc)
  implicit none
  integer, intent(IN) :: noc

  num_of_child = noc

end subroutine init_advanced_exchange

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize intercomm
subroutine init_intercomm()
  use jcup_utils, only : error
  use jcup_mpi_lib, only : jml_GetMyrank, jml_GetCommSizeLocal, jml_GetComm, &
                           jml_GetMyrankGlobal
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  use jcup_comp, only : get_num_of_total_component, get_num_of_my_component, &
                        is_my_component  
  implicit none
  integer :: total_comp
  integer :: ierr
  integer :: i, j
  integer :: boss_flag

  if (is_init) return

  if (get_num_of_my_component()>=2) then
    call error("init_intercomm", "intercomm module can not be used for overlapped exchange")
  end if

  if (num_of_child <= 0) then
    call error("init_intercomm", "subroutine jcup_init_advanced_exhcange is not called")
  end if

  total_comp = get_num_of_total_component()

  allocate(send_data(total_comp))
  allocate(recv_data(total_comp))

  do i = 1, total_comp
    allocate(send_data(i)%boss_info(NUM_OF_EXCHANGE_GRID))
    allocate(send_data(i)%my_info(NUM_OF_EXCHANGE_GRID))
    allocate(recv_data(i)%boss_info(NUM_OF_EXCHANGE_GRID))
    allocate(recv_data(i)%my_info(NUM_OF_EXCHANGE_GRID))
  end do

  do i = 1, total_comp
    do j = 1, NUM_OF_EXCHANGE_GRID
      send_data(i)%my_info(j)%ofirst = .true.
      recv_data(i)%my_info(j)%ofirst = .true.
    end do
  end do

  do i = 1, total_comp
    if (is_my_component(i)) then
      my_comp_id = i
      exit
    end if
  end do

  my_size = jml_GetCommSizeLocal(my_comp_id)
  my_rank = jml_GetMyrank(my_comp_id)
  my_comm = jml_GetComm(my_comp_id)
  my_rank_global = jml_GetMyrankGlobal()

  if (mod(my_size, num_of_child) /= 0) then
    call error("init_intercomm", "mod(my_size, num_of_family_members) /= 0, check the argument of jcup_init_advance_exchange")
  end if

  call MPI_Comm_split(my_comm, my_rank/num_of_child, my_rank, family_comm, ierr)

  if (mod(my_rank, num_of_child) == 0) then
    is_boss = .true.
    boss_rank = my_rank
    boss_flag = 1
  else
    is_boss = .false.
    boss_rank = num_of_child*(my_rank/num_of_child)
    boss_flag = MPI_UNDEFINED
  end if

  call MPI_COMM_split(my_comm, boss_flag, my_rank, boss_comm, ierr)

  is_init = .true.

end subroutine init_intercomm

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize intercomm
subroutine init_my_info(send_comp_id, recv_comp_id, mapping_tag, num_of_target)
  use jcup_comp, only : is_my_component
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA
  use jcup_mpi_lib, only : jml_SendLeader, jml_RecvLeader
  implicit none
  integer, intent(IN) :: send_comp_id
  integer, intent(IN) :: recv_comp_id
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: num_of_target
  integer :: int_array(1)
  integer :: target_comp
  integer :: ierr

  if (is_my_component(send_comp_id)) then
    my_info_ptr => send_data(recv_comp_id)%my_info(mapping_tag)
    boss_info_ptr => send_data(recv_comp_id)%boss_info(mapping_tag)
  else
    my_info_ptr => recv_data(send_comp_id)%my_info(mapping_tag)
    boss_info_ptr => recv_data(send_comp_id)%boss_info(mapping_tag)
  end if

  current_send_comp = send_comp_id
  current_recv_comp = recv_comp_id
  current_mapping_tag = mapping_tag

  if (.not.my_info_ptr%ofirst) return

  my_info_ptr%num_of_target = num_of_target
  allocate(my_info_ptr%target_info(num_of_target))

  if (is_boss) then
     if (is_my_component(send_comp_id)) then
       int_array(1) = num_of_child
       call jml_SendLeader(int_array, 1, 1, recv_comp_id-1)

       call jml_RecvLeader(int_array, 1, 1, recv_comp_id-1)
       call MPI_Bcast(int_array, 1, MPI_INTEGER, 0, boss_comm, ierr)
       boss_info_ptr%num_of_target_child = int_array(1)
     else
       call jml_RecvLeader(int_array, 1, 1, send_comp_id-1)
       call MPI_Bcast(int_array, 1, MPI_INTEGER, 0, boss_comm, ierr)
       boss_info_ptr%num_of_target_child = int_array(1)

       int_array(1) = num_of_child
       call jml_SendLeader(int_array, 1, 1, send_comp_id-1)
     end if
  end if

end subroutine init_my_info

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize intercomm
subroutine set_my_info(pe_index, rank_num, offset, num_of_data)
  use jcup_comp, only : is_my_component
  implicit none
  integer, intent(IN) :: pe_index
  integer, intent(IN) :: rank_num
  integer, intent(IN) :: offset
  integer, intent(IN) :: num_of_data
  integer :: target_comp

  if (.not.my_info_ptr%ofirst) return

  my_info_ptr%target_info(pe_index)%rank_num         = rank_num
  my_info_ptr%target_info(pe_index)%offset           = offset
  my_info_ptr%target_info(pe_index)%target_grid_size = num_of_data

end subroutine set_my_info

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize intercomm
subroutine set_boss_info()
  use jcup_comp, only : is_my_component
  implicit none
  integer :: child_info(2)
  integer, allocatable :: child_target_info(:)
  integer :: recv_buffer(1), recv_count(1), recv_offset(1) 
  integer :: i, ierr

  if (.not.my_info_ptr%ofirst) return

  call make_local_buffer(my_info_ptr)

  child_info(1) = my_info_ptr%num_of_target
  child_info(2) = my_info_ptr%my_grid_size

  allocate(child_target_info(3*my_info_ptr%num_of_target))
  do i = 1, my_info_ptr%num_of_target
     child_target_info(i*3-2) = my_info_ptr%target_info(i)%rank_num
     child_target_info(i*3-1) = my_info_ptr%target_info(i)%offset
     child_target_info(i*3-0) = my_info_ptr%target_info(i)%target_grid_size
  end do

  if (is_boss) then

    allocate(boss_info_ptr%child_info(num_of_child))
    allocate(boss_info_ptr%child_grid_size(num_of_child))
    allocate(boss_info_ptr%child_grid_size_tmp(num_of_child))
    allocate(boss_info_ptr%child_offset(num_of_child))
    allocate(boss_info_ptr%child_offset_tmp(num_of_child))

    call recv_child_info(boss_info_ptr, child_info)
    call recv_child_target_info(boss_info_ptr, child_target_info)
    call cal_child_size_and_offset(boss_info_ptr)
    call make_boss_buffer(boss_info_ptr)

    call cal_target_boss_info(boss_info_ptr)

    if (is_my_component(current_send_comp)) then
      call cal_global_offset_send(boss_info_ptr)
    else
      call cal_global_offset_recv(boss_info_ptr)
    end if

  else
    call MPI_Gather(child_info, 2, MPI_INTEGER, recv_buffer, 2, MPI_INTEGER, 0, family_comm, ierr)
    call MPI_GatherV(child_target_info, my_info_ptr%num_of_target*3, MPI_INTEGER, recv_buffer, recv_count, &
                     recv_buffer, MPI_INTEGER, 0, family_comm, ierr)
  end if    

  deallocate(child_target_info)
  my_info_ptr%ofirst = .false.

end subroutine set_boss_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine make_local_buffer(my_info_ptr)
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA
  implicit none
  type(pe_info_type), pointer :: my_info_ptr
  integer :: grid_size
  integer :: i
  
  grid_size = 0
  do i = 1, my_info_ptr%num_of_target
    grid_size = grid_size + my_info_ptr%target_info(i)%target_grid_size
  end do

  my_info_ptr%my_grid_size = grid_size

  if (allocated(local_buffer)) then
    if (size(local_buffer) < grid_size*NUM_OF_EXCHANGE_DATA) then
       deallocate(local_buffer)
       allocate(local_buffer(grid_size*NUM_OF_EXCHANGE_DATA))
     end if
  else
     allocate(local_buffer(grid_size*NUM_OF_EXCHANGE_DATA))
  end if

end subroutine make_local_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_child_info(boss_info_ptr, child_info)
  implicit none
  type(boss_info_type), pointer :: boss_info_ptr
  integer, intent(IN) :: child_info(:)
  integer, allocatable :: recv_buffer(:)
  integer :: i, ierr

  allocate(recv_buffer(2*num_of_child))
  call MPI_Gather(child_info, 2, MPI_INTEGER, recv_buffer, 2, MPI_INTEGER, 0, family_comm, ierr)

  do i = 1, num_of_child
    boss_info_ptr%child_info(i)%num_of_target    = recv_buffer(i*2-1)
    boss_info_ptr%child_info(i)%my_grid_size = recv_buffer(i*2)
    allocate(boss_info_ptr%child_info(i)%target_info(recv_buffer(i*2-1)))
  end do    
  deallocate(recv_buffer)

end subroutine recv_child_info
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_child_target_info(boss_info_ptr, child_target_info)
  implicit none
  type(boss_info_type), pointer :: boss_info_ptr
  integer, intent(IN) :: child_target_info(:)
  integer :: recv_array_size
  integer, allocatable :: recv_buffer(:), recv_count(:), recv_offset(:)
  integer :: i, j, ierr

  recv_array_size = 0
  do i = 1, num_of_child
     recv_array_size = recv_array_size + boss_info_ptr%child_info(i)%num_of_target
  end do

  allocate(recv_buffer(recv_array_size*3))
  allocate(recv_count(num_of_child))
  allocate(recv_offset(num_of_child))

  do i = 1, num_of_child
    recv_count(i) = boss_info_ptr%child_info(i)%num_of_target*3
  end do

  recv_offset(1) = 0
  do i = 2, num_of_child
    recv_offset(i) = recv_offset(i-1) + boss_info_ptr%child_info(i-1)%num_of_target*3
  end do

  call MPI_GatherV(child_target_info, my_info_ptr%num_of_target*3, MPI_INTEGER, recv_buffer, recv_count, &
                   recv_offset, MPI_INTEGER, 0, family_comm, ierr)

  do i = 1, num_of_child
    do j = 1, boss_info_ptr%child_info(i)%num_of_target
       boss_info_ptr%child_info(i)%target_info(j)%rank_num = recv_buffer(recv_offset(i)+(j-1)*3+1)      
       boss_info_ptr%child_info(i)%target_info(j)%offset   = recv_buffer(recv_offset(i)+(j-1)*3+2)      
       boss_info_ptr%child_info(i)%target_info(j)%target_grid_size = recv_buffer(recv_offset(i)+(j-1)*3+3)      
    end do
  end do

  deallocate(recv_buffer, recv_count, recv_offset)

end subroutine recv_child_target_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_child_size_and_offset(boss_info_ptr)
  implicit none
  type(boss_info_type), pointer :: boss_info_ptr
  integer :: i, j

  do i = 1, num_of_child
    boss_info_ptr%child_grid_size(i) = 0
    do j = 1, boss_info_ptr%child_info(i)%num_of_target
       boss_info_ptr%child_grid_size(i) = boss_info_ptr%child_grid_size(i) &
                                        + boss_info_ptr%child_info(i)%target_info(j)%target_grid_size
    end do
  end do

  boss_info_ptr%child_offset(1) = 0
  do i = 2, num_of_child
     boss_info_ptr%child_offset(i) = boss_info_ptr%child_offset(i-1)+boss_info_ptr%child_grid_size(i-1)
  end do

end subroutine cal_child_size_and_offset

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine make_boss_buffer(boss_info_ptr)
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA
  implicit none
  type(boss_info_type), pointer :: boss_info_ptr
  integer :: total_boss_array_size
  integer :: i

  total_boss_array_size = 0
  do i = 1, num_of_child
    total_boss_array_size = total_boss_array_size + boss_info_ptr%child_grid_size(i)
  end do

  if (allocated(boss_buffer)) then
    if (size(boss_buffer) < total_boss_array_size*NUM_OF_EXCHANGE_DATA) then
       deallocate(boss_buffer)
       deallocate(temp_buffer)
       allocate(boss_buffer(total_boss_array_size*NUM_OF_EXCHANGE_DATA))
       allocate(temp_buffer(total_boss_array_size*NUM_OF_EXCHANGE_DATA))
    end if
  else
    allocate(boss_buffer(total_boss_array_size*NUM_OF_EXCHANGE_DATA))
    allocate(temp_buffer(total_boss_array_size*NUM_OF_EXCHANGE_DATA))
  end if

end subroutine make_boss_buffer


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_target_boss_info(boss_ptr)
  use jcup_utils, only : sort_int_1d
  implicit none
  type(boss_info_type), pointer :: boss_ptr
  integer :: total_target
  integer, allocatable :: target_rank_array(:)
  integer :: current_target
  integer :: current_boss, before_boss
  integer :: boss_counter
  integer :: counter
  integer :: i, j, k

  total_target = 0
  do i = 1, num_of_child
    total_target = total_target + boss_ptr%child_info(i)%num_of_target
  end do

  allocate(target_rank_array(total_target))

  counter = 0
  do i = 1, num_of_child
    do j= 1, boss_ptr%child_info(i)%num_of_target
      counter = counter + 1
      target_rank_array(counter) = boss_ptr%child_info(i)%target_info(j)%rank_num
    end do
  end do

  call sort_int_1d(total_target, target_rank_array)

  !write(0, * )"total_rank_array ", target_rank_array
  ! cal num of target boss
  current_target = -1
  before_boss = -1
  boss_counter = 0
  do i = 1, total_target
    if (current_target == target_rank_array(i)) cycle
    current_target = target_rank_array(i)
    current_boss = current_target/boss_ptr%num_of_target_child
    if (before_boss /= current_boss) then
      boss_counter = boss_counter + 1
      before_boss = current_boss
    end if
  end do
  !write(0, * ) "num_of_target_boss ",boss_counter

  boss_ptr%num_of_target_boss = boss_counter
  allocate(boss_ptr%target_boss_rank(boss_counter))
  allocate(boss_ptr%target_boss_grid_size(boss_counter))
  allocate(boss_ptr%target_boss_offset(boss_counter))

  boss_ptr%target_boss_grid_size(:) = 0

  ! cal target boss grid size
  current_target = -1
  before_boss = -1
  boss_counter = 0
  !write(0, *) "total target = ", total_target, num_of_child, boss_ptr%num_of_target_child
  do i = 1, total_target
    if (current_target == target_rank_array(i)) cycle
    current_target = target_rank_array(i)
    current_boss = current_target/boss_ptr%num_of_target_child

    if (before_boss /= current_boss) then
      boss_counter = boss_counter + 1
      before_boss = current_boss
      boss_ptr%target_boss_rank(boss_counter) = current_boss*boss_ptr%num_of_target_child

      do j = 1, num_of_child
        !write(0, *) "num_of_target = ", boss_ptr%child_info(j)%num_of_target
        do k = 1, boss_ptr%child_info(j)%num_of_target 
           if (boss_ptr%child_info(j)%target_info(k)%rank_num/boss_ptr%num_of_target_child == current_boss) then
             boss_ptr%target_boss_grid_size(boss_counter) = boss_ptr%target_boss_grid_size(boss_counter) &
                                                          + boss_ptr%child_info(j)%target_info(k)%target_grid_size
             !write(0, *) "cal grid size ", boss_counter, boss_ptr%child_info(j)%target_info(k)%target_grid_size
           end if
        end do
      end do

    end if

  end do

  ! cal target boss offset
  boss_ptr%target_boss_offset(1) = 0
  do i = 2, boss_ptr%num_of_target_boss
    boss_ptr%target_boss_offset(i) = boss_ptr%target_boss_offset(i-1) + boss_ptr%target_boss_grid_size(i-1)
  end do

  deallocate(target_rank_array)

end subroutine cal_target_boss_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_global_offset_send(boss_ptr)
  use jcup_utils, only : sort_int_1d
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  type(boss_info_type), pointer :: boss_ptr
  integer :: total_target
  integer, allocatable :: target_rank_array(:)
  integer :: global_offset
  integer :: current_target
  integer :: counter
  integer :: i, j, k

  total_target = 0
  do i = 1, num_of_child
    total_target = total_target + boss_ptr%child_info(i)%num_of_target
  end do

  allocate(target_rank_array(total_target))

  counter = 0
  do i = 1, num_of_child
    do j= 1, boss_ptr%child_info(i)%num_of_target
      counter = counter + 1
      target_rank_array(counter) = boss_ptr%child_info(i)%target_info(j)%rank_num
    end do
  end do

  call sort_int_1d(total_target, target_rank_array)

 ! cal global offset
  global_offset = 0
  current_target = -1
  
  do i = 1, total_target

    if (current_target == target_rank_array(i)) cycle

    current_target = target_rank_array(i)

    do j = 1, num_of_child
      do k = 1, boss_ptr%child_info(j)%num_of_target 
         if (boss_ptr%child_info(j)%target_info(k)%rank_num == current_target) then
           boss_ptr%child_info(j)%target_info(k)%global_offset = global_offset
           global_offset = global_offset + boss_ptr%child_info(j)%target_info(k)%target_grid_size
         end if
      end do
    end do

  end do

  !if (jml_GetMyrankGlobal() == 0) then
  !  do j = 1, num_of_child
  !    do k = 1, boss_ptr%child_info(j)%num_of_target 
  !         write(0, *) "global offset send = ", j, k, boss_ptr%child_info(j)%target_info(k)%rank_num, 
  !                             boss_ptr%child_info(j)%target_info(k)%global_offset
  !    end do
  !  end do
  !end if

  deallocate(target_rank_array)

end subroutine cal_global_offset_send

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_global_offset_recv(boss_ptr)
  use jcup_utils, only : sort_int_1d
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  type(boss_info_type), pointer :: boss_ptr
  integer :: total_target
  integer, allocatable :: target_rank_array(:)
  integer, allocatable :: target_boss_array(:)
  integer :: global_offset
  integer :: counter
  integer :: i, j, k

  total_target = 0
  do i = 1, num_of_child
    total_target = total_target + boss_ptr%child_info(i)%num_of_target
  end do

  allocate(target_rank_array(total_target))
  counter = 0
  do i = 1, num_of_child
    do j= 1, boss_ptr%child_info(i)%num_of_target
      counter = counter + 1
      target_rank_array(counter) = boss_ptr%child_info(i)%target_info(j)%rank_num
    end do
  end do

  allocate(target_boss_array(total_target))
  do i = 1, total_target
    target_boss_array(i) = (target_rank_array(i)/boss_ptr%num_of_target_child)*boss_ptr%num_of_target_child
  end do

 ! cal global offset
  global_offset = 0
  do k = 1, boss_ptr%num_of_target_boss
    counter = 0
    do i = 1, num_of_child
      do j = 1, boss_ptr%child_info(i)%num_of_target
        counter = counter + 1
        if (boss_ptr%target_boss_rank(k) == target_boss_array(counter)) then
          boss_ptr%child_info(i)%target_info(j)%global_offset = global_offset
          global_offset = global_offset + boss_ptr%child_info(i)%target_info(j)%target_grid_size
        end if
      end do
    end do
  end do

  !if (jml_GetMyrankGlobal() == 0) then
  !  do j = 1, num_of_child
  !    do k = 1, boss_ptr%child_info(j)%num_of_target 
  !         write(0, *) "global offset recv = ", j, k, boss_ptr%child_info(j)%target_info(k)%rank_num, 
  !         boss_ptr%child_info(j)%target_info(k)%global_offset
  !    end do
  !  end do
  !end if

  deallocate(target_rank_array)

end subroutine cal_global_offset_recv

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine send_data_intercomm(data, num_of_data, exchange_data_id)
  use jcup_mpi_lib, only : jml_ISendModel, jml_send_waitall, jml_GetMyrankGlobal
  real(kind=8), pointer :: data(:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  integer :: temp_offset, buffer_offset
  integer :: i, j, k
  real(kind=8), pointer :: data_ptr

  call gather_data_to_boss(data, num_of_data)

  if (.not.is_boss) return

  !if (jml_GetMyrankGlobal() == 0) write(0, *) "boss_buffer ", boss_buffer

  buffer_offset = 0
  do i = 1, num_of_child
    do j = 1, boss_info_ptr%child_info(i)%num_of_target
      temp_offset = boss_info_ptr%child_info(i)%target_info(j)%global_offset * num_of_data
      do k = 1, boss_info_ptr%child_info(i)%target_info(j)%target_grid_size * num_of_data
        temp_buffer(temp_offset + k) = boss_buffer(buffer_offset + k)
      end do
      buffer_offset = buffer_offset + boss_info_ptr%child_info(i)%target_info(j)%target_grid_size * num_of_data
    end do
  end do

  !if (jml_GetMyrankGlobal() == 0) write(0, *) "temp buffer ", temp_buffer

  do i = 1, boss_info_ptr%num_of_target_boss
    data_ptr => temp_buffer(boss_info_ptr%target_boss_offset(i) * num_of_data + 1)    
    call jml_ISendModel(current_send_comp, data_ptr, 1, boss_info_ptr%target_boss_grid_size(i) * num_of_data, &
                       current_recv_comp, boss_info_ptr%target_boss_rank(i), exchange_data_id)
  end do
  
  call jml_send_waitall()
  
end subroutine send_data_intercomm

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize intercomm
subroutine gather_data_to_boss(data, num_of_data)
  implicit none
  real(kind=8), pointer :: data(:)
  integer, intent(IN) :: num_of_data
  real(kind=8) :: dummy_array1(1)
  integer :: dummy_array2(1), dummy_array3(1)  
  integer :: offset
  integer :: counter
  integer :: i, j
  integer :: ierr

  ! pack data to local buffer
  !write(0, * ) "gather_data_to_boss ", my_rank_global, my_info_ptr%my_grid_size, num_of_data, size(local_buffer) 

  counter = 0
  do i = 1, my_info_ptr%num_of_target
    offset = my_info_ptr%target_info(i)%offset 
     !write(0, *) "pack data ", my_rank_global, i, my_info_ptr%target_info(i)%target_grid_size
    do j = 1, my_info_ptr%target_info(i)%target_grid_size * num_of_data
      counter = counter + 1
      local_buffer(counter) = data(offset + j)
    end do
  end do

  !if (jml_GetMyrankGlobal() == 0) write(0,*) "local buffer ", num_of_data, local_buffer
  
  if (is_boss) then
    boss_info_ptr%child_grid_size_tmp(:) = boss_info_ptr%child_grid_size(:) * num_of_data
    boss_info_ptr%child_offset_tmp(:) = boss_info_ptr%child_offset(:) * num_of_data
    call MPI_GatherV(local_buffer, counter, MPI_DOUBLE_PRECISION, &
                     boss_buffer, boss_info_ptr%child_grid_size_tmp, boss_info_ptr%child_offset_tmp, &
                     MPI_DOUBLE_PRECISION, 0, family_comm, ierr)                    
    !if (jml_GetMyrankGlobal() == 0) write(0,*) boss_buffer
 else        
    call MPI_GatherV(local_buffer, counter, MPI_DOUBLE_PRECISION, &
                     dummy_array1, dummy_array2, dummy_array3, MPI_DOUBLE_PRECISION, 0, family_comm, ierr)                    
  end if

end subroutine gather_data_to_boss


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_data_intercomm(data, num_of_data, exchange_data_id)
  use jcup_mpi_lib, only : jml_IRecvModel, jml_recv_waitall, jml_GetMyrankGlobal
  real(kind=8), pointer :: data(:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  integer :: temp_offset, buffer_offset
  integer :: i, j, k
  real(kind=8), pointer :: data_ptr

  if (is_boss) then

    !write(0, * ) "recv start ", boss_info_ptr%num_of_target_boss
    do i = 1, boss_info_ptr%num_of_target_boss
      data_ptr => temp_buffer(boss_info_ptr%target_boss_offset(i) * num_of_data + 1)    
      call jml_IRecvModel(current_recv_comp, data_ptr, 1, boss_info_ptr%target_boss_grid_size(i) * num_of_data, &
                          current_send_comp, boss_info_ptr%target_boss_rank(i), exchange_data_id)
    end do
  
    call jml_recv_waitall()

    !if (jml_GetMyrankGlobal() == 0) write(0,*) "temp buffer ", temp_buffer

    buffer_offset = 0
    do i = 1, num_of_child
    do j = 1, boss_info_ptr%child_info(i)%num_of_target
        temp_offset = boss_info_ptr%child_info(i)%target_info(j)%global_offset * num_of_data      
        do k = 1, boss_info_ptr%child_info(i)%target_info(j)%target_grid_size * num_of_data
          boss_buffer(buffer_offset + k) = temp_buffer(temp_offset + k)
        end do
        buffer_offset = buffer_offset + boss_info_ptr%child_info(i)%target_info(j)%target_grid_size * num_of_data
      end do
    end do
    !if (jml_GetMyrankGlobal() == 0) write(0,*) "recv data ", boss_buffer

  end if

  call scatter_data_from_boss(data, num_of_data)
  
end subroutine recv_data_intercomm

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize intercomm
subroutine scatter_data_from_boss(data, num_of_data)
  implicit none
  real(kind=8), pointer :: data(:)
  integer, intent(IN) :: num_of_data
  real(kind=8) :: dummy_array1(1)
  integer :: dummy_array2(1), dummy_array3(1)  
  integer :: offset
  integer :: counter
  integer :: i, j
  integer :: ierr



  if (is_boss) then
    boss_info_ptr%child_grid_size_tmp(:) = boss_info_ptr%child_grid_size(:) * num_of_data
    boss_info_ptr%child_offset_tmp(:) = boss_info_ptr%child_offset(:) * num_of_data
    call MPI_ScatterV(boss_buffer, boss_info_ptr%child_grid_size_tmp, boss_info_ptr%child_offset_tmp, &
                      MPI_DOUBLE_PRECISION, &
                     local_buffer, my_info_ptr%my_grid_size*num_of_data, MPI_DOUBLE_PRECISION, 0, &
                     family_comm, ierr)                    
  else        
    call MPI_ScatterV(dummy_array1, dummy_array2, dummy_array3, MPI_DOUBLE_PRECISION, &
                     local_buffer, my_info_ptr%my_grid_size * num_of_data, MPI_DOUBLE_PRECISION, 0, &
                     family_comm, ierr)                    
  end if

  ! unpack data to local buffer
  !write(0, *) "scatter_data_from_boss ", my_rank, my_info_ptr%my_grid_size, num_of_data, size(local_buffer)
  counter = 0
  do i = 1, my_info_ptr%num_of_target
    !write(0, *) "unpack data ", my_rank, i, my_info_ptr%target_info(i)%target_grid_size
    do j = 1, my_info_ptr%target_info(i)%target_grid_size * num_of_data
      counter = counter + 1
      offset = my_info_ptr%target_info(i)%offset
      data(offset + j) = local_buffer(counter) 
    end do
  end do

end subroutine scatter_data_from_boss


end module jcup_intercomm
