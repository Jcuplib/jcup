!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

!
!Copyright (c) 2020, arakawa@rist.jp
!All rights reserved.
!
module jcup_grid_checker
  use jcup_constant, only : STR_SHORT
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_checker         ! subroutine (checker_flag)
  public :: check_grid_index     ! subroutine (grid_index, comp_name, grid_name)
  public :: check_mapping_table  ! subroutine (my_name, send_comp_name, send_grid_name,
                                 !                      recv_comp_name, recv_grid_name,
                                 !                      send_index, recv_index)
  public :: finalize_checker     ! subroutine ()
  
!--------------------------------   private  ---------------------------------!

  logical, private :: check_flag = .false.
  
  type grid_checker_type
     character(len=STR_SHORT) :: comp_name
     character(len=STR_SHORT) :: grid_name
     integer :: comp_id
     integer :: local_grid_size 
     integer, pointer :: local_index(:) => null()
     integer :: global_grid_size = 0 
     integer, pointer :: global_index(:) => null() 
     type(grid_checker_type), pointer :: next_ptr => null()
  end type grid_checker_type

  type(grid_checker_type), pointer :: checker => null()

contains
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine init_checker(checker_flag)
    use jcup_mpi_lib, only : jml_AllReduceMax
    use jcup_utils, only : put_log
    implicit none
    logical, intent(IN) :: checker_flag
    integer :: int_array(1)
    integer :: res
    
    check_flag = checker_flag

    if (check_flag) then
       int_array(1) = 1
    else
       int_array(1) = 0
    end if

    call jml_AllReduceMax(int_array(1), res)

    if (res > 0) then
       check_flag = .true.
       call put_log("grid checker flag is set .true.. Grid index and mapping table check will be done")
    else
       check_flag = .false.
       call put_log("grid checker flag is set .false.. Grid index and mapping table check will be skipped")
    end if
    
  end subroutine init_checker
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine finalize_checker()
    use jcup_utils, only : put_log
    implicit none
    type(grid_checker_type), pointer :: cptr
    type(grid_checker_type), pointer :: bptr
    
    if (check_flag == .false.) return

    cptr => checker
    bptr => null()
    
    do while(associated(cptr))
       if (associated(cptr%local_index)) deallocate(cptr%local_index)
       if (associated(cptr%global_index)) deallocate(cptr%global_index)
       bptr => cptr
       cptr => cptr%next_ptr
       deallocate(bptr)
    end do

    call put_log("grid checker finalize OK")
    
  end subroutine finalize_checker
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine check_grid_index(grid_index, model_name, grid_name)
    use jcup_constant, only : STR_LONG
    use jcup_mpi_lib, only : jml_isLocalLeader
    use jcup_utils, only : put_log, error, sort_int_1d
    use jcup_comp, only : get_comp_id_from_name
    implicit none
    integer, intent(IN) :: grid_index(:)
    character(len=*), intent(IN) :: model_name
    character(len=*), intent(IN) :: grid_name
    type(grid_checker_type), pointer :: ckr_ptr
    character(len=STR_LONG) :: log_str
    integer :: comp_id 
    integer :: i

    if (check_flag == .false.) return
    
    if (size(grid_index) <= 0) return ! no check
    
    if (minval(grid_index) <= 0) then
       call error("check_grid_index", "grid index error, grid index must be >= 1 "// &
            ", grid name = "//trim(grid_name))
    end if

    call put_log("grid index check start, grid name = "//trim(grid_name))
    
    comp_id = get_comp_id_from_name(trim(model_name))

    call set_grid_index(checker, grid_index, model_name, grid_name)

    ckr_ptr => get_grid_ptr(checker, model_name, grid_name)
    
    call cal_global_sorted_array(comp_id, ckr_ptr%local_index, &
                                 ckr_ptr%global_grid_size, ckr_ptr%global_index) 

    if (jml_isLocalLeader(comp_id))  then
       do i = 1,ckr_ptr%global_grid_size - 1
          if (ckr_ptr%global_index(i) == ckr_ptr%global_index(i+1)) then
             write(log_str, '(A,I7)') "grid index overlapping, comp name = "//&
                  trim(model_name)//", grid name = "//trim(grid_name)//&
                  ", index = ", ckr_ptr%global_index(i)
             call error("check_grid_index", trim(log_str))
          end if
       end do
    end if
    
    call put_log("grid index check finish, grid name = "//trim(grid_name))

  end subroutine check_grid_index
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine set_grid_index(self, grid_index, comp_name, grid_name)
    use jcup_comp, only : get_num_of_my_component
    implicit none
    type(grid_checker_type), pointer :: self
    integer, intent(IN) :: grid_index(:)
    character(len=*), intent(IN) :: comp_name
    character(len=*), intent(IN) :: grid_name
    type(grid_checker_type), pointer :: cptr
    type(grid_checker_type), pointer :: bptr

    cptr => self
    bptr => null()

    do while(associated(cptr))
       bptr => cptr
       cptr => cptr%next_ptr
    end do

    allocate(cptr)

    if (associated(bptr)) then
       bptr%next_ptr => cptr
    else
       self => cptr
    end if

    cptr%next_ptr => null()

    cptr%comp_name = trim(comp_name)
    cptr%grid_name = trim(grid_name)
    cptr%local_grid_size = size(grid_index)
    allocate(cptr%local_index(cptr%local_grid_size))
    cptr%local_index(:) = grid_index(:)
     
  end subroutine set_grid_index
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  function get_grid_ptr(self, comp_name, grid_name) result(res)
    implicit none
    type(grid_checker_type), pointer :: self
    character(len=*), intent(IN) :: comp_name
    character(len=*), intent(IN) :: grid_name
    type(grid_checker_type), pointer :: res

    res => self

    do while(associated(res))
       if ((trim(comp_name) == trim(res%comp_name)).and.(trim(grid_name) == trim(res%grid_name))) return
       res => res%next_ptr
    end do

  end function get_grid_ptr
    
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine cal_global_sorted_array(comp_id, local_array, global_array_size, global_array)
    use jcup_mpi_lib, only : jml_GetCommSizeLocal, jml_GatherLocal, jml_GatherVLocal, jml_isLocalLeader
    use jcup_utils, only : sort_int_1d
    implicit none
    integer, intent(IN) :: comp_id
    integer, intent(IN) :: local_array(:)
    integer, intent(OUT) :: global_array_size
    integer, pointer    :: global_array(:)
    integer :: num_of_pe
    integer :: my_size(1)
    integer :: global_size
    integer, pointer :: array_size_array(:)
    integer, pointer :: offset(:)
    integer :: i
    
    num_of_pe = jml_GetCommSizeLocal(comp_id)
    my_size(1) = size(local_array)
    
    allocate(array_size_array(num_of_pe))
    call jml_GatherLocal(comp_id, my_size, 1, 1, array_size_array)

    allocate(offset(num_of_pe+1))
    offset(1) = 0
    do i = 1, num_of_pe
       offset(i+1) = offset(i) + array_size_array(i)
    end do
    
    global_size = sum(array_size_array)

    if (jml_isLocalLeader(comp_id)) then
       allocate(global_array(global_size))
    else
       allocate(global_array(1))
    end if
    
    global_array_size = global_size
    
    call jml_GatherVLocal(comp_id, local_array, my_size(1), global_array, array_size_array, offset)

    if (jml_isLocalLeader(comp_id)) then
      call sort_int_1d(global_size, global_array)
   end if

   deallocate(array_size_array)
   deallocate(offset)
   
  end subroutine cal_global_sorted_array
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine check_mapping_table(my_name, send_comp_name, send_grid_name, recv_comp_name, recv_grid_name, &
                                 send_index, recv_index)
    use jcup_mpi_lib, only : jml_SendLeader, jml_RecvLeader, jml_isLocalLeader
    use jcup_utils, only : put_log, sort_int_1d, error
    use jcup_comp, only : get_comp_id_from_name
    implicit none
    character(len=*), intent(IN) :: my_name
    character(len=*), intent(IN) :: send_comp_name, send_grid_name
    character(len=*), intent(IN) :: recv_comp_name, recv_grid_name
    integer, optional, intent(IN) :: send_index(:), recv_index(:)
    type(grid_checker_type), pointer :: ckr_ptr
    character(len=STR_SHORT) :: grid_name
    integer :: int_array(1)
    integer, pointer :: grid_index(:)
    integer :: my_comp_id, send_comp_id, recv_comp_id

    if (check_flag == .false.) return

    my_comp_id = get_comp_id_from_name(trim(my_name))
    if (.not.jml_isLocalLeader(my_comp_id)) return
    

    if (trim(my_name) == trim(send_comp_name)) then
       ckr_ptr => get_grid_ptr(checker, send_comp_name, send_grid_name)
       if (.not.associated(ckr_ptr)) then
           call error("check_mapping_table", &
              "no such comp or grid, comp = "//&
              trim(send_comp_name)//", grid = "//trim(send_grid_name))
        end if
        grid_name = send_grid_name
    else
       ckr_ptr => get_grid_ptr(checker, recv_comp_name, recv_grid_name)
       if (.not.associated(ckr_ptr)) then
           call error("check_mapping_table", &
              "no such comp or grid, comp = "//&
              trim(recv_comp_name)//", grid = "//trim(recv_grid_name))
       end if
        grid_name = recv_grid_name
    endif
    
    call put_log("mapping table check start, grid name = "//trim(grid_name))

    send_comp_id = get_comp_id_from_name(trim(send_comp_name))
    recv_comp_id = get_comp_id_from_name(trim(recv_comp_name))
    
    if (present(send_index)) then
       if (trim(my_name) == trim(send_comp_name)) then
          int_array(1) = size(recv_index)
          call jml_SendLeader(int_array, 1, 1, recv_comp_id-1)
          call jml_SendLeader(recv_index, 1, int_array(1), recv_comp_id-1)
          allocate(grid_index(int_array(1)))
          grid_index(:) = send_index(:)
       else
          int_array(1) = size(recv_index)
          call jml_SendLeader(int_array, 1, 1, send_comp_id-1)
          call jml_SendLeader(send_index, 1, int_array(1), send_comp_id-1)
          allocate(grid_index(int_array(1)))
          grid_index(:) = recv_index(:)
       end if
    else
       if (trim(my_name) == trim(send_comp_name)) then
          call jml_RecvLeader(int_array, 1, 1, recv_comp_id-1)
          allocate(grid_index(int_array(1)))
          call jml_RecvLeader(grid_index, 1, int_array(1), recv_comp_id-1)
       else
          call jml_RecvLeader(int_array, 1, 1, send_comp_id-1)
          allocate(grid_index(int_array(1)))
          call jml_RecvLeader(grid_index, 1, int_array(1), send_comp_id-1)
       end if
    end if
    
    call sort_int_1d(int_array(1), grid_index)
    
    call check_inclusiveness(grid_index, ckr_ptr%global_index)

    deallocate(grid_index)
    
    call put_log("mapping table check finish, grid name = "//trim(grid_name))

  end subroutine check_mapping_table
  
!=======+=========+=========+=========+=========+=========+=========+=========+
! check if array_a is a subset of array_b
  subroutine check_inclusiveness(array_a, array_b)
    use jcup_constant, only : STR_LONG
    use jcup_utils, only : error
    implicit none
    integer, intent(INOUT) :: array_a(:)
    integer, intent(INOUT) :: array_b(:)
    integer :: size_a, size_b
    integer :: value_a, value_b
    integer :: i, j
    integer :: point_b
    character(len=STR_LONG) :: log_str

    call delete_same_index(array_a, size_a)
    call delete_same_index(array_b, size_b)
    
    point_b = 1
    do i = 1, size_a
      value_a = array_a(i)
      if (value_a < array_b(point_b)) then ! value_a is not included in array_b
        write(log_str, '(A,I7,A)') "mapping table index ", value_a, " is not included in grid index"
        call error("check_mapping_table", trim(log_str))
      end if
       
      do j = point_b, size_b
        value_b = array_b(j)
        if (value_a == value_b) then
          point_b = min(j + 1, size_b)
          exit
        end if
      end do

      if (j > size_b) then
        write(log_str, '(A,I7,A)') "mapping table index ", value_a, " is not included in grid index"
        call error("check_mapping_table", trim(log_str))
      end if
      
    end do

  end subroutine check_inclusiveness
  
!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine delete_same_index(array, array_size)
    implicit none
    integer, intent(INOUT) :: array(:)
    integer, intent(OUT)   :: array_size
    integer :: i

    array_size = 1
    do i = 2, size(array)
       if (array(i) /= array(i-1)) then
          array_size = array_size + 1
          array(array_size) = array(i)
       end if
    end do

  end subroutine delete_same_index
  
          
end module jcup_grid_checker
