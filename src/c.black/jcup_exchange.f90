!====================================================================================================
!> @brief
!> jcup data exchange module

module jcup_exchange
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA, NUM_OF_EXCHANGE_GRID, REAL_DATA, DOUBLE_DATA
  use jcup_constant, only : DATA_1D, DATA_2D, DATA_25D, DATA_3D
  use jcup_constant, only : STR_SHORT
  use jcup_time, only : time_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_exchange               ! subroutine (num_of_comp)
  public :: finalize_exchange           ! subroutine ()
  public :: allocate_recv_flag          ! subroutine (flag_size)
  public :: init_buffer_1d              ! subroutine (max_grid, max_data)
  public :: init_buffer_25d             ! subroutine (max_data)
  public :: set_step_flag               ! subroutine (step_flag)
  public :: set_exchange_comp_id        ! subroutine (comp_id)
  public :: set_send_mapping_table      ! subroutine (send_comp_id, recv_comp_id, grid_tag)
  public :: set_recv_mapping_table      ! subroutine (recv_comp_id, send_comp_id, grid_tag)
  public :: check_mapping_table_setting ! subroutine ()
  public :: set_send_grid_tag           ! subroutine (send_comp_id, recv_comp_id, map_num, grid_tag)
  public :: set_recv_grid_tag           ! subroutine (recv_comp_id, send_comp_id, map_num, grid_tag)
  public :: send_final_step_data        ! subroutine ()
  public :: set_fill_value              ! subroutine (fill_value)
  public :: get_fill_value              ! real(kind=89 function ()
  public :: set_restart_flag            ! subroutine (restart_flag) 
  public :: jcup_exchange_data_parallel
  public :: jcup_exchange_data_serial
  public :: jcup_send_data_immediately
  public :: jcup_recv_data_immediately
  public :: jcup_put_data_1d_double
  public :: jcup_put_data_25d_double
  public :: jcup_get_data_1d_double
  public :: jcup_get_data_25d_double
  public :: write_all_scalar_data       ! subroutine(fid, comp_id)
  public :: recv_all_scalar_data        ! subroutine()

!--------------------------------   private  ---------------------------------!

  real(kind=8), private :: fill_value = HUGE(kind(1.d0))

  integer :: max_grid_size
  integer :: max_data_num

  logical, private :: is_first_serial_step ! 2014/12/09 [ADD]

  integer, private :: before_comp_id

  logical, pointer, private :: is_initial_step(:) ! first step flag. used for serial exchange only 

  logical, pointer, private :: recv_flag(:) ! recv flag for data recv

  real(kind=8), pointer, private :: buffer_double1d(:,:)  ! nx, num_of_exchange_data
  real(kind=8), pointer, private :: buffer_double25d(:,:) ! nx, num_of_2d_array

  integer, private, pointer :: send_table_checker(:,:), recv_table_checker(:,:) ! (my_comp_id, target_comp_id)

  integer, private, pointer :: send_mapping_tag(:,:), recv_mapping_tag(:,:) ! (my_comp_id, target_comp_id)
  integer, private, pointer :: my_send_grid_tag(:,:,:), my_recv_grid_tag(:,:,:) ! (my_comp_id, target_comp_id, grid_num)

  integer, private :: current_grid_tag

  integer, private :: current_comp_id
  logical, save, private :: step_flag = .true.
  logical, save, private :: is_restart = .false.  ! 2020/06/16 
  
contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_exchange(num_of_comp)
  implicit none
  integer, intent(IN) :: num_of_comp

  is_first_serial_step = .true. ! 2014/12/09 [ADD]

  allocate(is_initial_step(num_of_comp)) 
  is_initial_step(:) = .true.

  allocate(send_table_checker(num_of_comp, num_of_comp))
  send_table_checker(:,:) = 0
  allocate(recv_table_checker(num_of_comp, num_of_comp))
  recv_table_checker(:,:) = 0

  allocate(send_mapping_tag(num_of_comp, num_of_comp))
  send_mapping_tag(:,:) = 1
  allocate(recv_mapping_tag(num_of_comp, num_of_comp))
  recv_mapping_tag(:,:) = 1

  allocate(my_send_grid_tag(num_of_comp, num_of_comp, NUM_OF_EXCHANGE_GRID))
  my_send_grid_tag(:,:,:) = -9999
  allocate(my_recv_grid_tag(num_of_comp, num_of_comp, NUM_OF_EXCHANGE_GRID))
  my_recv_grid_tag(:,:,:) = -9999

  step_flag = .true.

end subroutine init_exchange

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine finalize_exchange()
  implicit none

  if (associated(buffer_double1d)) deallocate(buffer_double1d)
  if (associated(buffer_double25d)) deallocate(buffer_double25d)
  if (associated(send_table_checker)) deallocate(send_table_checker)
  if (associated(recv_table_checker)) deallocate(recv_table_checker)
  if (associated(send_mapping_tag)) deallocate(send_mapping_tag)
  if (associated(recv_mapping_tag)) deallocate(recv_mapping_tag)
  
end subroutine finalize_exchange

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_buffer_1d(max_grid, max_data)
  implicit none
  integer, intent(IN) :: max_grid
  integer, intent(IN) :: max_data

  max_grid_size = max_grid
  max_data_num  = max_data

  allocate(buffer_double1d(max_grid_size, max_data_num))

end subroutine init_buffer_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_buffer_25d(max_data)
  implicit none
  integer, intent(IN) :: max_data

  if (associated(buffer_double25d)) then
    if (max_data > size(buffer_double25d, 2)) then
      deallocate(buffer_double25d)
      allocate(buffer_double25d(max_grid_size, max_data))
    end if
  else
    allocate(buffer_double25d(max_grid_size, max_data))
  end if

end subroutine init_buffer_25d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_step_flag(flag)
  implicit none
  logical, intent(IN) :: flag
  
  step_flag = flag

end subroutine set_step_flag

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_first_step()
  implicit none

  is_first_step = step_flag

end function is_first_step

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_exchange_comp_id(comp_id)
  implicit none
  integer, intent(IN) :: comp_id

  current_comp_id = comp_id

end subroutine set_exchange_comp_id


!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_exchange_comp_id()
  implicit none

  get_exchange_comp_id = current_comp_id

end function get_exchange_comp_id

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine allocate_recv_flag(flag_size)
  implicit none
  integer, intent(IN) :: flag_size

  allocate(recv_flag(flag_size))
 
end subroutine allocate_recv_flag

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_send_mapping_table(send_comp_id, recv_comp_id, grid_num)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  use jcup_utils, only : error, IntToStr
  implicit none
  integer, intent(IN) :: send_comp_id, recv_comp_id, grid_num

  if (grid_num>NUM_OF_EXCHANGE_GRID) then
    call error("set_send_mapping_table", "grid_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if

  if (send_table_checker(send_comp_id, recv_comp_id)/=grid_num-1) then
     call error("set_send_mapping_table", "mapping table check err, model:" &
                            //trim(IntToStr(recv_comp_id))//", index:"//trim(IntToStr(grid_num)))
  end if

  send_table_checker(send_comp_id, recv_comp_id) = max(send_table_checker(send_comp_id, recv_comp_id), grid_num)

end subroutine set_send_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_recv_mapping_table(recv_comp_id, send_comp_id, grid_num)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  use jcup_utils, only : IntToStr, error
  implicit none
  integer, intent(IN) :: recv_comp_id, send_comp_id, grid_num

  if (grid_num>NUM_OF_EXCHANGE_GRID) then
     call error("set_recv_mapping_table", "grid_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if

  if (recv_table_checker(recv_comp_id, send_comp_id)/=grid_num-1) then
     call error("set_recv_mapping_table", "mapping table check err, model:" &
                            //trim(IntToStr(send_comp_id))//", index:"//trim(IntToStr(grid_num)))
  end if

  recv_table_checker(recv_comp_id, send_comp_id) = max(recv_table_checker(recv_comp_id, send_comp_id), grid_num)

end subroutine set_recv_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_mapping_table_setting()
  use jcup_constant, only : NO_SEND_RECV
  use jcup_utils, only : error
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name
  use jcup_config, only : get_comp_exchange_type
  implicit none
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      do j = 1, get_num_of_total_component()
        if (get_comp_exchange_type(i,j) /= NO_SEND_RECV) then
          if (send_table_checker(i,j)==0.and.recv_table_checker(i,j)==0) then
            call error("check_mapping_table_setting", "subroutine jcup_set_mapping_table must be called on component : " &
                      //trim(get_component_name(i))//" and "//trim(get_component_name(j)))
          end if
        end if
      end do
    end if
  end do

end subroutine check_mapping_table_setting

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_send_grid_tag(send_comp_id, recv_comp_id, map_num, grid_tag)
  implicit none
  integer, intent(IN) :: send_comp_id, recv_comp_id, map_num, grid_tag
  
  my_send_grid_tag(send_comp_id, recv_comp_id, map_num) = grid_tag

end subroutine set_send_grid_tag
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_recv_grid_tag(recv_comp_id, send_comp_id, map_num, grid_tag)
  implicit none
  integer, intent(IN) :: recv_comp_id, send_comp_id, map_num, grid_tag
  
  my_recv_grid_tag(recv_comp_id, send_comp_id, map_num) = grid_tag

end subroutine set_recv_grid_tag
  
!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isSendOK(name)
  use jcup_config, only : isSendData
  use jcup_utils, only  : put_log
  implicit none
  character(len=*),intent(IN) :: name

  !if (.not.is_Initialize_completed) then
  !   call jcup_abnormal_end("jcup_isSendOK", "jcup_SetMappigTable not called")
  !end if

  jcup_isSendOK = .false.

  if (.not.isSendData(DATA_NAME = name)) then
    call put_log("Data : "//trim(name)//", send_flag /= 1, send data skip", 1)
    return
  end if

  jcup_isSendOK = .true.

end function jcup_isSendOK

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isRecvOK(name)
  use jcup_config, only : is_my_recv_data, isRecvData, is_recv_step_data
  use jcup_utils, only  :  put_log, error
  implicit none
  character(len=*),intent(IN) :: name

  !if (.not.is_Initialize_completed) then
  !   call jcup_abnormal_end("jcup_isRecvOK", "jcup_SetMappigTable not called")
  !end if

  jcup_isRecvOK = .false.

  if (.not.is_my_recv_data(trim(name))) then
    call error("jcup_isRecvOK", &
           "Data "//trim(name)//" is not listed in coupler.conf file. Check your code and file")
  end if
  
  if (.not.isRecvData(DATA_NAME = name)) then
    call put_log("Data : "//trim(name)//", recv_flag /= 1, recv skip", 1)
    return
  end if

  if (.not.is_recv_step_data(name)) then
    call put_log("Recv data skipped, data : "//trim(name), 1)
    return
  end if

  jcup_isRecvOK = .true.

end function jcup_isRecvOK

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> send final step data when SERIAL EXCHANGE and ADVANCE_SEND_RECV
! 2014/12/08 [ADD]   
subroutine send_final_step_data()
  use jcup_constant, only : ADVANCE_SEND_RECV
  use jcup_config, only : get_comp_exchange_type, set_current_conf
  use jcup_utils, only : put_log
  use jcup_comp, only : get_num_of_total_component, get_component_name, is_my_component
  implicit none
  character(len=STR_SHORT) :: component_name
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call set_current_conf(i)
      component_name = trim(get_component_name(i))
      !!!call jcup_set_time(component_name, time_array, 0, IS_EXCHANGE=.false.) ! 2014/11/14 [DELETE]
      call set_exchange_comp_id(i)
      do j = 1, get_num_of_total_component()
        if (get_comp_exchange_type(i,j) == ADVANCE_SEND_RECV) then
          if (.not.is_my_component(j)) then ! 2014/12/08 [ADD]
            call put_log("!!!!!!!!!!!!!!  extra data send start  !!!!!!!!!!!!!", 1)
            call jcup_exchange_data_send(i, j, .true.)  ! send final step data, if recv model time lag == 1
            call put_log("!!!!!!!!!!!!!!  extra data send finish !!!!!!!!!!!!!", 1)
          end if
        end if
      end do
    end if
  end do

  !!!!call buffer_check_write()

end subroutine send_final_step_data

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_send(send_comp_id, recv_comp_id, is_final_step)
  use jcup_constant, only : STR_LONG
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, &
                          is_my_exchange_step, is_send_step_data, &
                          get_average_data_name, get_send_data_id
  use jcup_comp, only : get_num_of_total_component, get_component_name
  use jcup_data, only : get_send_data_dimension, get_num_of_exchange_send_data
  implicit none
  integer, intent(IN) :: send_comp_id
  integer, intent(IN) :: recv_comp_id
  logical, intent(IN) :: is_final_step

  integer :: i,j,mdl
  integer :: my_model
  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=STR_SHORT) :: data_name(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: average_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STR_LONG) :: log_str
  integer :: exchange_data_id

  my_model = send_comp_id

  i = recv_comp_id

    if (i /= my_model) then

      recv_flag(:) = .false.
      num_of_data = 0

      do d = 1, get_num_of_recv_data(i)
        if (recv_flag(d)) cycle
        rd => get_recv_data_conf_ptr(trim(get_component_name(i)),d)
        if (.not.rd%is_recv) cycle

        if (trim(rd%send_model)==trim(get_component_name(my_model))) then

          if (rd%time_lag == 0) cycle ! skip when time lag == 0

          if ((is_final_step).and.(rd%time_lag /= 1)) cycle ! skip final step send if target time_lag /= 1

          if ((is_first_step()).and.(rd%time_lag==1)) cycle ! skip first step send if target time_lag == 1

          if (is_send_step_data(rd%model_id, rd%send_data)) then

            ! set first data name
            num_of_data = 1
            data_name(num_of_data) = trim(rd%send_data)
            exchange_data_id = rd%data_id

            average_data_name(num_of_data) = data_name(num_of_data)
            is_average(num_of_data) = rd%is_average
            if (rd%is_average) then
              average_data_name(num_of_data) = trim(get_average_data_name(rd%send_data, rd%model_id, rd%name))
            end if
         
            ! count number of data
            if (get_send_data_dimension(send_comp_id, rd%send_data) == DATA_25D) then
              num_of_2d_array = get_num_of_exchange_send_data(send_comp_id, rd%send_data)
            else

              do dd = d+1, get_num_of_recv_data(i)
                rrdd => get_recv_data_conf_ptr(trim(get_component_name(i)),dd)
                if (.not.rrdd%is_recv) cycle
                if (trim(rrdd%send_model)==trim(get_component_name(get_exchange_comp_id()))) then
                  if (rd%recv_tag == rrdd%recv_tag ) then
                    recv_flag(dd) = .true.
                    num_of_data = num_of_data+1
                    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                      call error("jcup_exchange_data_send", &
                      "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag")
                    end if
                    data_name(num_of_data) = trim(rrdd%send_data)
                    average_data_name(num_of_data) = data_name(num_of_data)
                    is_average(num_of_data) = rrdd%is_average
                    if (rrdd%is_average) then
                      average_data_name(num_of_data) = trim(get_average_data_name(rrdd%send_data, rrdd%model_id, rrdd%name))
                    end if
                  end if
                end if
              end do
            end if

            call put_log("SEND DATA START! dest model, "//trim(get_component_name(i)) &
                        //", number of send data:"//trim(IntToStr(num_of_data)), 1)

            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "SEND DATA NAME : "//trim(data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

            select case(get_send_data_dimension(send_comp_id, rd%send_data))
            case (DATA_1D)
              call jcup_exchange_data_1d_double(trim(get_component_name(i)), data_name, average_data_name, &
                                                num_of_data, exchange_data_id, is_average)
            case (DATA_25D)
              call jcup_exchange_data_25d_double(trim(get_component_name(i)), data_name(1), average_data_name(1), &
                                                 num_of_2d_array, exchange_data_id, is_average(1))
            case default
              call error("jcup_exchange_data_send","data dimension error")
            end select
            call put_log("SEND DATA FINISH! dest model, "//trim(get_component_name(i)), 1) 
          end if
        end if
      end do
    end if

end subroutine jcup_exchange_data_send

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD] len=14 -> len=20
subroutine jcup_exchange_data_parallel()
  use jcup_constant, only : DATA_2D, DATA_3D, CONCURRENT_SEND_RECV, NO_SEND_RECV, &
                            ADVANCE_SEND_RECV, BEHIND_SEND_RECV, IMMEDIATE_SEND_RECV
  use jcup_utils, only : put_log, IntToStr
  use jcup_config, only : get_num_of_recv_data, is_my_exchange_step, &
                          get_comp_exchange_type, get_current_comp_id, get_num_of_recv_data, &
                          set_current_conf, get_comp_name_from_comp_id, &
                          get_comp_exchange_type, is_exchange_step
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_time, only : get_current_time
  !use jcup_exchange, only : allocate_recv_flag, get_exchange_comp_id, is_first_step, set_step_flag, jcup_exchange_data_local
  implicit none
  integer :: i,j,mdl
  integer :: my_model
  integer :: num_of_data
  character(len=20) :: time_str
  type(time_type) :: c_time
  integer :: max_flag_size
  integer :: send_comp_id, recv_comp_id
  integer :: temp_current_comp, temp_target_comp
  integer :: exchange_type

  call get_current_time(get_current_comp_id(), 1, time_str)
  call get_current_time(get_current_comp_id(), 1, c_time)
  !write(110+jml_GetMyrankGlobal(),*) "exchange data 2 3 ", get_current_comp_id()

  if (is_first_step()) then
    max_flag_size = 0
    do i = 1, get_num_of_total_component()
      max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
    end do
    call allocate_recv_flag(max_flag_size)
  end if

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!! DATA EXCHANGE START !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!  PARALLEL  EXCHANGE !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange time      : "//trim(time_str))
    call put_log("exchange component : "//trim(get_comp_name_from_comp_id(get_exchange_comp_id())))
    
    ! local data exchange
    do i = 1, get_num_of_total_component()
      do j = i+1, get_num_of_total_component()
        if (is_my_component(i).or.is_my_component(j)) then

          if (get_comp_exchange_type(i,j) == CONCURRENT_SEND_RECV) then 
            send_comp_id = i 
            recv_comp_id = j

            if (is_exchange_step(send_comp_id, recv_comp_id, c_time)) then
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
              call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
              call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
            end if
            send_comp_id = j
            recv_comp_id = i
            if (is_exchange_step(send_comp_id, recv_comp_id, c_time)) then
              !!call set_current_conf(temp_current_comp)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
              call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
              call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
              !!call set_current_conf(get_exchange_comp_id)
            end if
          end if
        end if
      end do
    end do    

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!  PARALLEL  EXCHANGE  !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!! DATA EXCHANGE FINISH !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  if (is_first_step()) call set_step_flag(.false.)

  !write(0,*) "exchange data finish, ", my_model, jml_GetMyrank()

end subroutine jcup_exchange_data_parallel

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/11/19 [MOD] 
! 2014/12/08 [MOD] 
subroutine jcup_exchange_data_serial(my_comp_id)
  use jcup_constant, only : DATA_2D, DATA_3D, CONCURRENT_SEND_RECV, NO_SEND_RECV, &
                            ADVANCE_SEND_RECV, BEHIND_SEND_RECV, IMMEDIATE_SEND_RECV
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_utils, only : put_log, IntToStr
  use jcup_config, only : get_num_of_recv_data, is_my_exchange_step, &
                          get_comp_exchange_type, get_current_comp_id, get_num_of_recv_data, &
                          set_current_conf, get_comp_name_from_comp_id, &
                          get_comp_exchange_type
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_time, only : get_current_time
  !use jcup_exchange, only : allocate_recv_flag, is_first_step, set_step_flag, jcup_exchange_data_local

  implicit none
  integer, intent(IN) :: my_comp_id
  integer :: target_comp_id
  integer :: i,j,mdl
  integer :: my_model
  integer :: num_of_data
  character(len=20) :: time_str
  type(time_type) :: c_time
  integer :: max_flag_size
  integer :: send_comp_id, recv_comp_id
  integer :: temp_current_comp, temp_target_comp
  integer :: exchange_type
  integer :: my_rank
  !integer :: before_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 1 ", my_comp_id, target_comp_id, current_comp_id

  my_rank = jml_GetMyrankGlobal()

  call get_current_time(get_current_comp_id(), 1, time_str)
  call get_current_time(get_current_comp_id(), 1, c_time)
  !write(110+jml_GetMyrankGlobal(),*) "exchange data 2 3 ", get_current_comp_id()

  if (is_first_step()) then
    max_flag_size = 0
    do i = 1, get_num_of_total_component()
      max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
    end do
    call allocate_recv_flag(max_flag_size)
  end if

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!! DATA EXCHANGE START !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   SERIAL  EXCHANGE  !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange time      : "//trim(time_str))
    !call put_log("exchange component : "//trim(get_comp_name_from_comp_id(my_comp_id))//", "// &
    !                                      trim(get_comp_name_from_comp_id(target_comp_id)))

  if (is_first_serial_step) then
    ! send all my serial component data to the ADVANCE components
    do i = 1, get_num_of_total_component()
      if (is_my_component(i)) then
        do j = 1, get_num_of_total_component()
          if (i == j) cycle
          if (get_comp_exchange_type(i, j) == BEHIND_SEND_RECV) then
            !if (.not.is_my_component(j)) then

              send_comp_id = i
              recv_comp_id = j

              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  FIRST STEP SEND !!!!!!!!!!!!!! ", 1)
              call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
              call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH FIRST STEP SEND !!!!!!!!!!!!!! ", 1)

            !end if
          end if
        end do
      end if
    end do

    do i = 1, get_num_of_total_component()
      if ((get_comp_exchange_type(my_comp_id, i) == ADVANCE_SEND_RECV) .or. &
          (get_comp_exchange_type(my_comp_id, i) == BEHIND_SEND_RECV)) then
             if (i == j) cycle
             if (is_my_component(i)) cycle
              send_comp_id = i
              recv_comp_id = my_comp_id

              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  FIRST STEP RECV !!!!!!!!!!!!!! ", 1)
              call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
              call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH FIRST STEP RECV !!!!!!!!!!!!!! ", 1)

      end if
    end do

    is_first_serial_step = .false.
    before_comp_id = my_comp_id

    return
  end if

  
  !before_comp_id = my_comp_id

  !do i = my_comp_id - 1, 1, -1
  !  if (.not.is_my_component(i)) cycle 
  !  do j = 1, get_num_of_total_component()
  !    if ((get_comp_exchange_type(i, j) == ADVANCE_SEND_RECV) .or. &
  !        (get_comp_exchange_type(i, j) == BEHIND_SEND_RECV)) then
  !      before_comp_id = i
  !      goto 2000
  !    end if            
  !  end do
  !end do

  !do i = get_num_of_total_component(), my_comp_id + 1, -1
  !  if (.not.is_my_component(i)) cycle
  !  do j = 1, get_num_of_total_component()
  !    if ((get_comp_exchange_type(i, j) == ADVANCE_SEND_RECV) .or. &
  !        (get_comp_exchange_type(i, j) == BEHIND_SEND_RECV)) then
  !      before_comp_id = i
  !      goto 2000
  !    end if            
  !  end do
  !end do

  !2000 continue

  do i = 1, get_num_of_total_component()
    if (i == before_comp_id) cycle
    if ((get_comp_exchange_type(before_comp_id, i) == ADVANCE_SEND_RECV) .or. &
        (get_comp_exchange_type(before_comp_id, i) == BEHIND_SEND_RECV)) then
        send_comp_id = before_comp_id
        recv_comp_id = i
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  SEND !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH SEND !!!!!!!!!!!!!! ", 1)
    end if
  end do
    

  !if (is_initial_step(my_comp_id)) then
  !  do i = 1, get_num_of_total_component()
  !    if (i == my_comp_id) cycle
  !    if (i == before_comp_id) cycle      
  !    if ((get_comp_exchange_type(my_comp_id, i) == BEHIND_SEND_RECV)) then
  !        send_comp_id = i
  !        recv_comp_id = my_comp_id
  !        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  SEND !!!!!!!!!!!!!! ", 1)
  !        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
  !        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
  !        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH SEND !!!!!!!!!!!!!! ", 1)
  !    end if
  !    if ((get_comp_exchange_type(my_comp_id, i) == ADVANCE_SEND_RECV).and.(.not.is_my_component(i))) then
  !        send_comp_id = i
  !        recv_comp_id = my_comp_id
  !        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  SEND !!!!!!!!!!!!!! ", 1)
  !        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
  !        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
  !        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH SEND !!!!!!!!!!!!!! ", 1)
  !    end if
  !  end do
  !  is_initial_step(my_comp_id)  = .false.

  !else

    recv_comp_id = my_comp_id
    do i = 1, get_num_of_total_component()
      if (i == my_comp_id) cycle
      if (i == before_comp_id) cycle 
      if ((get_comp_exchange_type(recv_comp_id, i) == ADVANCE_SEND_RECV) .or. &
          (get_comp_exchange_type(recv_comp_id, i) == BEHIND_SEND_RECV)) then
          send_comp_id = i
          !if (is_my_component(i)) cycle
          call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  RECV !!!!!!!!!!!!!! ", 1)
          call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
          call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
          call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH RECV !!!!!!!!!!!!!! ", 1)
      end if
    end do
  !end if

  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!   SERIAL  EXCHANGE   !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!! DATA EXCHANGE FINISH !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  if (is_first_step()) call set_step_flag(.false.)

  before_comp_id = my_comp_id

end subroutine jcup_exchange_data_serial

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/11/19 [MOD] 
! 2014/12/08 [MOD] 
subroutine jcup_exchange_data_serial_old(my_comp_id, target_comp_id)
  use jcup_constant, only : DATA_2D, DATA_3D, CONCURRENT_SEND_RECV, NO_SEND_RECV, &
                            ADVANCE_SEND_RECV, BEHIND_SEND_RECV, IMMEDIATE_SEND_RECV
  use jcup_utils, only : put_log, IntToStr
  use jcup_config, only : get_num_of_recv_data, is_my_exchange_step, &
                          get_comp_exchange_type, get_current_comp_id, get_num_of_recv_data, &
                          set_current_conf, get_comp_name_from_comp_id, &
                          get_comp_exchange_type
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_time, only : get_current_time
  !use jcup_exchange, only : allocate_recv_flag, set_step_flag, is_first_step, jcup_exchange_data_local
  implicit none
  integer, intent(IN) :: my_comp_id, target_comp_id

  integer :: i,j,mdl
  integer :: my_model
  integer :: num_of_data
  character(len=20) :: time_str
  type(time_type) :: c_time
  integer :: max_flag_size
  integer :: send_comp_id, recv_comp_id
  integer :: temp_current_comp, temp_target_comp
  integer :: exchange_type

  !!!!write(0,*) "jcup_exchange_data_serial 1 ", my_comp_id, target_comp_id, current_comp_id

  call get_current_time(get_current_comp_id(), 1, time_str)
  call get_current_time(get_current_comp_id(), 1, c_time)
  !write(110+jml_GetMyrankGlobal(),*) "exchange data 2 3 ", get_current_comp_id()

  if (is_first_step()) then
    max_flag_size = 0
    do i = 1, get_num_of_total_component()
      max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
    end do
    call allocate_recv_flag(max_flag_size)
  end if

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!! DATA EXCHANGE START !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   SERIAL  EXCHANGE  !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange time      : "//trim(time_str))
    call put_log("exchange component : "//trim(get_comp_name_from_comp_id(my_comp_id))//", "// &
                                          trim(get_comp_name_from_comp_id(target_comp_id)))

    ! local data exchange
    !if (is_my_component(target_comp_id)) then
    !  if (is_initial_step(my_comp_id)) then
    !    is_initial_step(my_comp_id) = .false.
    !    send_comp_id = target_comp_id
    !    recv_comp_id = my_comp_id
        !!!!return ! 2014/11/19 [MOD]
    !  end if
    !  if ((get_comp_exchange_type(my_comp_id, target_comp_id) == ADVANCE_SEND_RECV)) then
    !    call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE 1 START  !!!!!!!!!!!!!! ", 1)
    !    call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
    !    call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
    !    call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE 1 FINISH !!!!!!!!!!!!!! ", 1)
    !  end if
    !end if

  if (is_my_component(target_comp_id)) then
      
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 3 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  (SAME COMPONENT) !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH (SAME COMPONENT) !!!!!!!!!!!!!! ", 1)


  else ! not same component

    if ((get_comp_exchange_type(my_comp_id, target_comp_id) == ADVANCE_SEND_RECV)) then

      if (is_initial_step(my_comp_id)) then ! initial step

        is_initial_step(my_comp_id) = .false.

        ! BSAR
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 3 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  ADVANCE, INITIAL !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH ADVANCE, INITIAL !!!!!!!!!!!!!! ", 1)

      else ! not initial step

  !!!!!write(0,*) "jcup_exchange_data_serial 4 ", my_comp_id, send_comp_id, recv_comp_id
        ! ASBR
        send_comp_id = my_comp_id
        recv_comp_id = target_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  ADVANCE, ASBR !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH ADVANCE, ASBR !!!!!!!!!!!!!! ", 1)

  !!!!!write(0,*) "jcup_exchange_data_serial 4.5 ", my_comp_id, send_comp_id, recv_comp_id
        ! 

        ! BSAR
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  ADVANCE, BSAR !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH ADVANCE, BSAR !!!!!!!!!!!!!! ", 1)
       end if

    else ! BEHIND_SEND_RECV

      if (is_initial_step(my_comp_id)) then
        is_initial_step(my_comp_id) = .false.

        ! BSAR
        send_comp_id = my_comp_id
        recv_comp_id = target_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 3 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  BEHIND, BSAR !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH BEHIND, BSAR !!!!!!!!!!!!!! ", 1)

        ! ASBR
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 3 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  BEHIND, ASBR !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH BEHIND, ASBR !!!!!!!!!!!!!! ", 1)

      else ! not initial step

  !!!!write(0,*) "jcup_exchange_data_serial 5 ", my_comp_id, send_comp_id, recv_comp_id
        ! BSAR
        send_comp_id = my_comp_id
        recv_comp_id = target_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  BEHIND, BSAR !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH BEHIND, BSAR !!!!!!!!!!!!!! ", 1)

        ! ASBR
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  BEHIND, ASBR !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH BEHIND, ASBR !!!!!!!!!!!!!! ", 1)
      end if
    end if

  end if ! not same component if end

  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!   SERIAL  EXCHANGE   !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!! DATA EXCHANGE FINISH !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  if (is_first_step()) call set_step_flag(.false.)

  !write(0,*) "exchange data finish, ", my_model, jml_GetMyrank()

end subroutine jcup_exchange_data_serial_old



!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, is_final_step)
  use jcup_constant, only : STR_LONG
  use jcup_mpi_lib, only : jml_GetMyrankGlobal, jml_GetCommSizeLocal
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : set_current_conf, get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, &
                          is_my_exchange_step, is_send_step_data, is_recv_step_data, &
                          get_average_data_name, get_send_data_id
  use jcup_comp, only : get_num_of_total_component, &
                        get_component_name, &
                        is_model_running, is_my_component
  use jcup_data, only : get_send_data_dimension, get_recv_data_dimension, &
                        get_num_of_exchange_send_data, get_num_of_exchange_recv_data
  implicit none
  integer, intent(IN) :: send_comp_id, recv_comp_id
  type(time_type), intent(IN) :: c_time
  logical, intent(IN) :: is_final_step

  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=STR_SHORT) :: send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: average_send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: recv_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STR_LONG) :: log_str
  integer :: exchange_data_id
  integer :: target_comp
  integer :: data_dimension
  logical :: is_exchange_step

    recv_flag(:) = .false.
    num_of_data = 0

    !!!write(0,*) "exchange_data_local_new 1 ", jml_GetMyrankGlobal(), send_comp_id, recv_comp_id

    do d = 1, get_num_of_recv_data(recv_comp_id)
      if (recv_flag(d)) cycle

      rd => get_recv_data_conf_ptr(trim(get_component_name(recv_comp_id)),d)

      if (.not.rd%is_recv) cycle
  
        call set_exchange_comp_id(send_comp_id)

        call set_current_conf(send_comp_id)

        !!!write(0,*) "jcup_exchange_data_local_new 3 ", jml_GetMyrankGlobal(), get_exchange_comp_id

        if (.not.is_my_exchange_step(c_time)) then
          call put_log("Current time is not exchange step, data send skip. Component ID : " & 
                       //trim(IntToStr(send_comp_id)))
          cycle
        end if

        if (rd%send_model_id==send_comp_id) then

          if (rd%time_lag == 0) cycle ! skip when time lag == 0

          if (is_my_component(send_comp_id)) then
            if ((is_final_step).and.(rd%time_lag /= 1)) cycle ! skip final step send if target time_lag /= 1
            !if ((is_first_step).and.(rd%time_lag==1)) cycle ! skip first step send if target time_lag == 1
          end if


          if (is_my_component(send_comp_id)) then ! 2013/04/08 T.Arakawa [MOD]
            is_exchange_step = is_send_step_data(rd%model_id, rd%send_data)
          else
            call set_current_conf(recv_comp_id)
            is_exchange_step = is_recv_step_data(rd%name)
            call set_current_conf(get_exchange_comp_id())
          end if

          if (is_exchange_step) then

          !if (is_send_step_data(rd%model_id, rd%send_data)) then

            ! set first data name
            num_of_data = 1
            send_data_name(num_of_data) = trim(rd%send_data)
            recv_data_name(num_of_data) = trim(rd%name)
            exchange_data_id = rd%data_id

            data_dimension = 9999

            if (is_my_component(send_comp_id)) then
              data_dimension = get_send_data_dimension(send_comp_id, rd%send_data)
            end if
            if (is_my_component(recv_comp_id)) then
              data_dimension = get_recv_data_dimension(recv_comp_id, rd%name)
            end if

        !!!write(0,*) "jcup_exchange_data_local_new 3.5 ", current_comp_id data_dimension, DATA_25D

            average_send_data_name(num_of_data) = send_data_name(num_of_data)
            is_average(num_of_data) = rd%is_average

            if (rd%is_average) then
              average_send_data_name(num_of_data) = trim(get_average_data_name(rd%send_data, rd%model_id, rd%name))
            end if
         
            ! count number of data
            if (data_dimension == DATA_25D) then
        !!!write(0,*) "jcup_exchange_data_local_new 3.6 ", current_comp_id, data_dimension, DATA_25D, send_comp_id, jml_GetMyrankGlobal()
              if (is_my_component(send_comp_id)) then
                num_of_2d_array = get_num_of_exchange_send_data(send_comp_id, rd%send_data)
              else
                num_of_2d_array = get_num_of_exchange_recv_data(recv_comp_id, rd%name)
              end if

        !!!write(0,*) "jcup_exchange_data_local_new 3.7 ", current_comp_id, data_dimension, DATA_25D
            else

              do dd = d+1, get_num_of_recv_data(recv_comp_id)
                rrdd => get_recv_data_conf_ptr(trim(get_component_name(recv_comp_id)),dd)
                if (.not.rrdd%is_recv) cycle
                if (rrdd%send_model_id==send_comp_id) then
                  if (rd%recv_tag == rrdd%recv_tag ) then
                    recv_flag(dd) = .true.
                    num_of_data = num_of_data+1
                    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                      call error("jcup_exchange_data_send", &
                      "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag")
                    end if
                    send_data_name(num_of_data) = trim(rrdd%send_data)
                    average_send_data_name(num_of_data) = send_data_name(num_of_data)
                    recv_data_name(num_of_data) = trim(rrdd%name)
                    is_average(num_of_data) = rrdd%is_average
                    if (rrdd%is_average) then
                      average_send_data_name(num_of_data) = trim(get_average_data_name(rrdd%send_data, rrdd%model_id, rrdd%name))
                    end if
                  end if
                end if
              end do
            end if

            call put_log("EXCHANGE DATA START! dest model, "//trim(get_component_name(recv_comp_id)) &
                        //", number of send data:"//trim(IntToStr(num_of_data)), 1)


            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "SEND DATA NAME : "//trim(send_data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(send_data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

        !!!write(0,*) "jcup_exchange_data_local_new 4 ", current_comp_id, jml_GetMyrankGlobal()

          select case(data_dimension)
            case (DATA_1D)
              call set_current_conf(get_exchange_comp_id())
              call jcup_exchange_data_1d_double(trim(get_component_name(recv_comp_id)), send_data_name, &
                                            average_send_data_name, &
                                            num_of_data, exchange_data_id, is_average)
              call set_current_conf(recv_comp_id)
              call set_exchange_comp_id(recv_comp_id)

              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_1d_double(trim(rd%send_model), recv_data_name, num_of_data, &
                                              exchange_data_id)
              end if
              call set_current_conf(send_comp_id)
              call set_exchange_comp_id(send_comp_id)
            case (DATA_25D)
              call set_current_conf(get_exchange_comp_id())
              call jcup_exchange_data_25d_double(trim(get_component_name(recv_comp_id)), send_data_name(1), &
                                             average_send_data_name(1), &
                                             num_of_2d_array, exchange_data_id, is_average(1))
              call set_current_conf(recv_comp_id)
              call set_exchange_comp_id(recv_comp_id)

              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_25d_double(trim(rd%send_model), recv_data_name(1), num_of_2d_array, &
                                               exchange_data_id)
              end if
              call set_exchange_comp_id(send_comp_id)
              call set_current_conf(send_comp_id)
            case default
              call error("jcup_exchange_data_local", "data dimension error")
            end select
            call put_log("EXCHANGE DATA FINISH! dest model, "//trim(get_component_name(recv_comp_id)), 1) 
          end if
        end if
      end do

end subroutine jcup_exchange_data_local

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> immediate send
!> @param[in] send_comp_name name of send component
!> @param[in] recv_comp_name name of recv component
!> @param[in] time_lag time_lag setting
! 2014/10/31 [MOD] get_current_time -> get_before_time
subroutine jcup_send_data_immediately(send_comp_name, recv_comp_name, time_lag) !dest_task, c_time, is_final_step)
  use jcup_constant, only : STR_LONG
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : set_current_conf, get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, get_current_comp_id, &
                          is_my_exchange_step, is_send_step_data, &
                          get_average_data_name, get_send_data_id
  use jcup_comp, only : get_comp_id_from_name
  use jcup_data, only : get_send_data_dimension, get_num_of_exchange_send_data
  use jcup_time, only : get_before_time
  !use jcup_exchange, only : set_exchange_comp_id, get_exchange_comp_id, set_step_flag, is_first_step, &
  !                          jcup_exchange_data_1d_double, jcup_exchange_data_25d_double
  implicit none
  character(len=*), intent(IN) :: send_comp_name ! name of my component
  character(len=*), intent(IN) :: recv_comp_name ! name of destination component
  integer, intent(IN) :: time_lag

  integer :: send_comp_id, recv_comp_id
  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=STR_SHORT) :: data_name(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: average_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STR_LONG) :: log_str
  character(len=20) :: time_str
  integer :: exchange_data_id
  integer :: target_comp, target_comp_id
  integer :: my_comp, my_comp_id
  logical :: is_first_step_temp

  call get_before_time(get_current_comp_id(), 1, time_str)

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   IMMEDIATE SEND    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   DATA SEND START   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange time      : "//trim(time_str))

  send_comp_id = get_comp_id_from_name(send_comp_name)
  recv_comp_id = get_comp_id_from_name(recv_comp_name)

  my_comp_id = send_comp_id

  target_comp_id = recv_comp_id

    recv_flag(:) = .false.
    num_of_data = 0

    do d = 1, get_num_of_recv_data(target_comp_id)

      if (recv_flag(d)) cycle
      rd => get_recv_data_conf_ptr(recv_comp_name,d)
      if (.not.rd%is_recv) cycle

        call set_exchange_comp_id(my_comp_id)

        call set_current_conf(my_comp_id)

        if (rd%send_model_id==my_comp_id) then

          !!!if (is_send_step_data(rd%model_id, rd%send_data)) then

            ! set first data name
            num_of_data = 1
            data_name(num_of_data) = trim(rd%send_data)
            exchange_data_id = rd%data_id

            average_data_name(num_of_data) = data_name(num_of_data)
            is_average(num_of_data) = rd%is_average
            if (rd%is_average) then
              average_data_name(num_of_data) = trim(get_average_data_name(rd%send_data, rd%model_id, rd%name))
            end if
         
            ! count number of data
            if (get_send_data_dimension(send_comp_id, rd%send_data) == DATA_25D) then
              num_of_2d_array = get_num_of_exchange_send_data(send_comp_id, rd%send_data)
            else

              do dd = d+1, get_num_of_recv_data(target_comp_id)
                rrdd => get_recv_data_conf_ptr(recv_comp_name, dd)
                if (.not.rrdd%is_recv) cycle
                if (rrdd%send_model_id==my_comp_id) then
                  if (rd%recv_tag == rrdd%recv_tag ) then
                    recv_flag(dd) = .true.
                    num_of_data = num_of_data+1
                    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                      call error("jcup_exchange_data_send", &
                      "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag")
                    end if
                    data_name(num_of_data) = trim(rrdd%send_data)
                    average_data_name(num_of_data) = data_name(num_of_data)
                    is_average(num_of_data) = rrdd%is_average
                    if (rrdd%is_average) then
                      average_data_name(num_of_data) = trim(get_average_data_name(rrdd%send_data, rrdd%model_id, rrdd%name))
                    end if
                  end if
                end if
              end do
            end if

            call put_log("SEND DATA START! dest model, "//trim(recv_comp_name) &
                        //", number of send data:"//trim(IntToStr(num_of_data)), 1)

            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "SEND DATA NAME : "//trim(data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

            if (time_lag==0) then
              is_first_step_temp = is_first_step()
              !!!is_first_step = .true. ! set is_first_step .true. for immediate data exchange
            end if

            select case(get_send_data_dimension(send_comp_id, rd%send_data))
            case (DATA_1D)
              call set_current_conf(get_exchange_comp_id())
              call jcup_exchange_data_1d_double(recv_comp_name, data_name, average_data_name, &
                                                num_of_data, exchange_data_id, is_average)
            case (DATA_25D)
              call set_current_conf(get_exchange_comp_id())
              call jcup_exchange_data_25d_double(recv_comp_name, data_name(1), average_data_name(1), &
                                                 num_of_2d_array, exchange_data_id, is_average(1))
            case default
              call error("jcup_send_data_immediately","data dimension error")
            end select

            if (time_lag==0) then
              call set_step_flag(is_first_step_temp)
            end if

            call put_log("SEND DATA FINISH! dest model, "//trim(recv_comp_name), 1) 
          !!!end if
        end if
    end do

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!    IMMEDIATE SEND    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!   DATA SEND FINISH   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)

end subroutine jcup_send_data_immediately

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> immediate recv
!> @param[in] send_comp_name name of send component
!> @param[in] recv_comp_name name of recv component
! 2014/07/08 [MOD] len=14 -> len=20
! 2014/10/31 [MOD] get_current_time -> get_before_time
subroutine jcup_recv_data_immediately(send_comp_name, recv_comp_name)
  use jcup_constant, only : STR_LONG
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : set_current_conf, get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, get_current_comp_id, &
                          is_my_exchange_step, is_send_step_data, is_recv_step_data, get_num_of_recv_data, &
                          get_average_data_name, get_send_data_id
  use jcup_comp, only : get_comp_id_from_name, is_model_running, is_my_component
  use jcup_data, only : get_recv_data_dimension, get_num_of_exchange_recv_data
  use jcup_time, only : get_before_time
  !use jcup_exchange, only : set_exchange_comp_id, get_exchange_comp_id, set_step_flag, is_first_step, &
  !                          jcup_exchange_data_1d_double, jcup_exchange_data_25d_double, &
  !                          jcup_interpolate_data_1d_double, jcup_interpolate_data_25d_double
  implicit none
  character(len=*), intent(IN) :: send_comp_name
  character(len=*), intent(IN) :: recv_comp_name ! my_comp_name
  integer :: source_task

  integer :: send_comp_id, recv_comp_id
  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=STR_SHORT) :: send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: average_send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: recv_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STR_LONG) :: log_str
  character(len=20) :: time_str
  integer :: exchange_data_id
  integer :: target_comp, target_comp_id
  integer :: my_comp, my_comp_id
  logical :: is_first_step_temp

  call get_before_time(get_current_comp_id(), 1, time_str)

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   IMMEDIATE RECV    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   DATA RECV START   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange time      : "//trim(time_str))

  send_comp_id = get_comp_id_from_name(send_comp_name)
  recv_comp_id = get_comp_id_from_name(recv_comp_name)

    target_comp_id = send_comp_id
    my_comp_id = recv_comp_id

    call set_exchange_comp_id(my_comp_id)
    call set_current_conf(my_comp_id)


    recv_flag(:) = .false.

    do d = 1, get_num_of_recv_data(my_comp_id)

      if (recv_flag(d)) cycle

      rd => get_recv_data_conf_ptr(recv_comp_name,d)

      if (.not.rd%is_recv) cycle

        if (rd%send_model_id /= target_comp_id) cycle

        if (is_model_running(rd%send_model)) then

          !!!if (is_recv_step_data(rd%name)) then


            num_of_data = 1
            send_data_name(num_of_data) = trim(rd%send_data)
            recv_data_name(num_of_data) = trim(rd%name)

            exchange_data_id = rd%data_id

            average_send_data_name(num_of_data) = send_data_name(num_of_data)
            is_average(num_of_data) = rd%is_average

            if (get_recv_data_dimension(recv_comp_id, rd%name) == DATA_25D) then
              num_of_2d_array = get_num_of_exchange_recv_data(recv_comp_id, rd%name)
            else

              do dd = d+1, get_num_of_recv_data(my_comp_id)
                rrdd => get_recv_data_conf_ptr(recv_comp_name,dd)
                if (.not.rrdd%is_recv) cycle
                if (rd%recv_tag == rrdd%recv_tag ) then
                  recv_flag(dd) = .true.
                  num_of_data = num_of_data+1
                  if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                    call error("jcup_exchange_data_send", &
                  "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag.")
                  end if
                  is_average(num_of_data) = rrdd%is_average
                  send_data_name(num_of_data) = trim(rrdd%send_data)
                  average_send_data_name(num_of_data) = send_data_name(num_of_data)
                  recv_data_name(num_of_data) = trim(rrdd%name)
                end if
              end do
            end if

            call put_log("RECV DATA START! source model:"//trim(rd%send_model) &
                        //", number of recv data:"//trim(IntToStr(num_of_data)), 1)

            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "RECV DATA NAME : "//trim(recv_data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(recv_data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if


            is_first_step_temp = is_first_step()
            call set_step_flag(.false.) ! set is_first_step .false. for immediate data exchange

            select case(get_recv_data_dimension(recv_comp_id, rd%name))
            case (DATA_1D)
              call set_exchange_comp_id(send_comp_id)
              call set_current_conf(send_comp_id)
              call jcup_exchange_data_1d_double(recv_comp_name, send_data_name, &
                                                average_send_data_name, &
                                                num_of_data, exchange_data_id, is_average)
              call set_current_conf(recv_comp_id)
              call set_exchange_comp_id(recv_comp_id)
              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_1d_double(trim(rd%send_model), recv_data_name, num_of_data, exchange_data_id, .true.) ! 2014/10/31 
              end if
            case (DATA_25D)
              call set_current_conf(send_comp_id)
              call set_exchange_comp_id(send_comp_id)
              call jcup_exchange_data_25d_double(recv_comp_name, send_data_name(1), &
                                                 average_send_data_name(1), &
                                                 num_of_2d_array, exchange_data_id, is_average(1))
              call set_current_conf(recv_comp_id)
              call set_exchange_comp_id(recv_comp_id)
              if (is_my_component(recv_comp_id)) then
                 call jcup_interpolate_data_25d_double(trim(rd%send_model), recv_data_name(1), num_of_2d_array, &
                                                       exchange_data_id, .true.)
              end if
            case default
              call error("jcup_recv_data_immediately", "data dimension error")
            end select
            call put_log("RECV DATA FINISH! source model, "//trim(rd%send_model), 1)

            call set_step_flag(is_first_step_temp)

          !!!end if
        end if
    end do

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!    IMMEDIATE RECV    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!   DATA RECV FINISH   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


end subroutine jcup_recv_data_immediately

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_1d_double(dest_model_name, data_name, average_data_name, num_of_data, exchange_data_id, &
                                      is_average)
  use jcup_constant, only : DATA_2D
  use jcup_utils, only : error, put_log
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, exchange_data_comp
  use jcup_time, only : time_type, get_current_time, get_before_time
  use jcup_comp, only :  get_comp_id_from_name, is_my_component, get_component_name
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_buffer, only : get_send_data_type, get_send_data
  use jcup_config, only : GetMappingTag, get_send_data_id
  implicit none
  character(len=*), intent(IN) :: dest_model_name
  character(len=STR_SHORT), intent(IN) :: data_name(:)
  character(len=STR_SHORT), intent(IN) :: average_data_name(:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  logical, intent(IN) :: is_average(:)

  type(time_type) :: time
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d

  if (.not.jcup_isSendOK(trim(data_name(1)))) return
 
  call put_log("exchange_data_1d_double start. source model : "//trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

  recv_comp_id = get_comp_id_from_name(trim(dest_model_name))

  call set_current_mapping_tag(current_comp_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name(1)))

  if (is_my_component(current_comp_id)) then

    if (is_first_step()) then 
      if (is_restart) then
        if (trim(data_name(1)) /= trim(average_data_name(1))) then ! if average data
          call get_current_time(current_comp_id, 1, time)
          call put_log("get_current_time ", 1)
        else
          call get_before_time(current_comp_id, 1, time)
          call put_log("get_before_time ", 1)
        end if
      else
        call get_current_time(current_comp_id, 1, time)
        call put_log("get_current_time ", 1)
      end if
    else
      if (trim(data_name(1)) /= trim(average_data_name(1))) then ! if average data
        call get_current_time(current_comp_id, 1, time)
        call put_log("get_current_time ", 1)
      else
        call get_before_time(current_comp_id, 1, time)
        call put_log("get_before_time ", 1)
      end if
    end if

    call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
    ni = ie-is+1

    if (is_first_step()) then
      do d = 1, num_of_data
        call get_send_data(buffer_double1d(1:ni,d), time, current_comp_id, &
                           get_send_data_id(recv_comp_id, data_name(d), is_average(d)), trim(data_name(d)))
      end do
    else
      do d = 1, num_of_data
        call get_send_data(buffer_double1d(1:ni,d), time, current_comp_id, &
                           get_send_data_id(recv_comp_id, data_name(d), is_average(d)), trim(average_data_name(d)))
      end do
    end if

    call set_data(buffer_double1d)

  end if

  call exchange_data_comp(current_comp_id, recv_comp_id, send_mapping_tag(current_comp_id, recv_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_data_id, DATA_1D)

  call put_log("exchange_data_1d_double completed. source model : "&
               //trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

  !!write(0,*) "jcup_send_data_double_1d_new 2 ", jml_GetMyrankGlobal(), is_my_component(current_comp_id)
end subroutine jcup_exchange_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_25d_double(dest_model_name, data_name, average_data_name, num_of_data, exchange_data_id, &
                                       is_average)
  use jcup_utils, only : error, put_log
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, send_data_1d, exchange_data_comp
  use jcup_time, only : time_type, get_current_time, get_before_time
  use jcup_comp, only : get_comp_id_from_name, is_my_component, get_component_name
  use jcup_buffer, only : get_send_data_type, get_send_data
  use jcup_config, only : GetMappingTag, get_send_data_id
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  character(len=*), intent(IN) :: dest_model_name
  character(len=STR_SHORT), intent(IN) :: data_name
  character(len=STR_SHORT), intent(IN) :: average_data_name
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  logical, intent(IN) :: is_average

  type(time_type) :: time
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: i

  if (.not.jcup_isSendOK(data_name)) return

  call put_log("exchange_data_25d_double start. source model : "//trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

  recv_comp_id = get_comp_id_from_name(trim(dest_model_name))

  call set_current_mapping_tag(current_comp_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name))

  if (is_my_component(current_comp_id)) then

    if (is_first_step()) then 
      if (is_restart) then
        if (trim(data_name) /= trim(average_data_name)) then ! if average data
          call get_current_time(current_comp_id, 1, time)
          call put_log("get_current_time ", 1)
        else
          call get_before_time(current_comp_id, 1, time)
          call put_log("get_before_time ", 1)
        end if
      else
        call get_current_time(current_comp_id, 1, time)
        call put_log("get_current_time ", 1)
      end if
    else
      if (trim(data_name) /= trim(average_data_name)) then ! if average data
        call get_current_time(current_comp_id, 1, time)
        call put_log("get_current_time ", 1)
      else
        call get_before_time(current_comp_id, 1, time)
        call put_log("get_before_time ", 1)
      end if
    end if

    call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
    ni = ie-is+1

    if (is_first_step()) then
      call get_send_data(buffer_double25d(1:ni,1:num_of_data), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, data_name, is_average), data_name)
    else
      call get_send_data(buffer_double25d(1:ni,1:num_of_data), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, data_name, is_average), average_data_name)
    end if

    call set_data(buffer_double25d)
  
  end if

  call exchange_data_comp(current_comp_id, recv_comp_id, send_mapping_tag(current_comp_id, recv_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_data_id, DATA_2D)


  call put_log("exchange_data_25d_double completed. source model : "&
               //trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

end subroutine jcup_exchange_data_25d_double


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_fill_value(fill_v)
  implicit none
  real(kind=8), intent(IN) :: fill_v

  fill_value = fill_v

end subroutine set_fill_value

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_fill_value() result(res)
  implicit none
  real(kind=8) :: res

  res = fill_value

end function get_fill_value

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2020/06/16 [NEW]
subroutine set_restart_flag(restart_flag)
  implicit none
  logical, intent(IN) :: restart_flag

  is_restart = restart_flag

end subroutine set_restart_flag


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_interpolate_data_1d_double(source_model_name, data_name, num_of_data, exchange_data_id, is_immediate_recv)
  use jcup_utils, only : error, put_log
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : interpolate_data_1d, get_data, recv_data
  use jcup_comp, only : get_comp_id_from_name, get_component_name
  use jcup_time, only : time_type, get_current_time, get_before_time
  use jcup_buffer, only : put_recv_data
  use jcup_config, only : GetRecvMappingTag, GetExchangeTag, get_recv_data_id_from_data_name
  implicit none
  character(len=*), intent(IN) :: source_model_name
  character(len=STR_SHORT), intent(IN) :: data_name(:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  logical, optional, intent(IN) :: is_immediate_recv

  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  type(time_type) :: time
  integer :: i, j, counter
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  integer :: source_comp_id

  if (.not.jcup_isRecvOK(trim(data_name(1)))) return

  call put_log("interpolate_data_1d_double start. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))


  do d = 1, num_of_data
    call jcup_check_recv_error(source_model_name, data_name(d))
  end do


  source_comp_id = get_comp_id_from_name(source_model_name)

  call set_current_mapping_tag(source_comp_id, current_comp_id, GetRecvMappingTag(RECV_DATA_NAME=trim(data_name(1))))

  do d = 1, num_of_data
    exchange_tag(d) = GetExchangeTag(trim(data_name(d)))
  end do

  call get_current_time(current_comp_id, 1, time)
  
  if (present(is_immediate_recv)) then
   if (is_immediate_recv) call get_before_time(current_comp_id, 1, time)
  end if

  call interpolate_data_1d(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_tag)

  buffer_double1d = fill_value

  call get_data(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                buffer_double1d(:,:), num_of_data)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

  ni = ie-is+1
  
  do d = 1, num_of_data
    call put_recv_data(buffer_double1d(1:ni,d), time, current_comp_id, &
                       get_recv_data_id_from_data_name(trim(data_name(d))), trim(data_name(d)))
  end do

  call put_log("interpolate_data_1d_double completed. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))

end subroutine jcup_interpolate_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_interpolate_data_25d_double(source_model_name, data_name, num_of_data, exchange_data_id, is_immediate_recv)
  use jcup_utils, only : error, put_log
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : interpolate_data_1d, get_data, recv_data
  use jcup_comp, only : get_comp_id_from_name, get_component_name
  use jcup_time, only : time_type, get_current_time, get_before_time
  use jcup_buffer, only : put_recv_data
  use jcup_config, only : GetRecvMappingTag, GetExchangeTag, get_recv_data_id_from_data_name
  implicit none
  character(len=*), intent(IN) :: source_model_name
  character(len=STR_SHORT), intent(IN) :: data_name
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  logical, optional, intent(IN) :: is_immediate_recv

  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  type(time_type) :: time
  integer :: i, j, counter
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: source_comp_id

  if (.not.jcup_isRecvOK(data_name)) return

  call put_log("interpolate_data_25d_double start. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))

  source_comp_id = get_comp_id_from_name(trim(source_model_name))

  call jcup_check_recv_error(source_model_name, data_name)

  call set_current_mapping_tag(source_comp_id, current_comp_id, GetRecvMappingTag(RECV_DATA_NAME=trim(data_name)))

  exchange_tag(1) = GetExchangeTag(data_name)

  call get_current_time(current_comp_id, 1, time)

  if (present(is_immediate_recv)) then ! 2014/10/31 [ADD]
   if (is_immediate_recv) call get_before_time(current_comp_id, 1, time)
  end if

  call interpolate_data_1d(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_tag)

  buffer_double25d = fill_value

  call get_data(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                buffer_double25d(:,:), num_of_data)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

  ni = ie-is+1

  call put_recv_data(buffer_double25d(1:ni,1:num_of_data), time, current_comp_id, &
                     get_recv_data_id_from_data_name(data_name), data_name)

  call put_log("interpolate_data_25d_double completed. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))

end subroutine jcup_interpolate_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_check_recv_error(source_model_name, my_data_name)
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr, isRecvData2
  use jcup_comp, only : is_model_running
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: source_model_name, my_data_name
  integer :: i
  type(recv_data_conf_type), pointer :: recv_data_ptr

  if (.not.is_model_running(trim(source_model_name))) then
    call error("jcup_check_recv_error", &
                              "Model "//trim(source_model_name)//" is not running")
  end if

  recv_data_ptr => get_recv_data_conf_ptr(DATA_NAME = my_data_name)

  if (.not.isRecvData2(source_model_name, recv_data_ptr%send_data)) then
    call error("jcup_check_recv_error", &
                              "Data "//trim(my_data_name)//" is not a send data. Check coupling.conf file")
  end if

end subroutine jcup_check_recv_error

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_mapping_tag(send_comp_id, recv_comp_id, mapping_tag)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  implicit none
  integer, intent(IN) :: send_comp_id
  integer, intent(IN) :: recv_comp_id
  integer, intent(IN) :: mapping_tag

  if (current_comp_id==send_comp_id) then
     !!!call check_mapping_table(send_table_checker(current_comp_id,:), recv_comp_id, mapping_tag)
     send_mapping_tag(current_comp_id, recv_comp_id) = mapping_tag
     current_grid_tag = my_send_grid_tag(current_comp_id, recv_comp_id, mapping_tag)
  else 
     !!!call check_mapping_table(recv_table_checker(current_comp_id,:), send_comp_id, mapping_tag)
     recv_mapping_tag(current_comp_id, send_comp_id) = mapping_tag
     current_grid_tag = my_recv_grid_tag(current_comp_id, send_comp_id, mapping_tag)
  end if
  
end subroutine set_current_mapping_tag

!=======+=========+=========+=========+=========+=========+=========+=========+

function jcup_isPutOK(data_type, comp_id) result(res)
  use jcup_config, only : is_put_step_data
  use jcup_utils, only  :  put_log
  use jcup_data, only : varp_type, get_time, get_data_name
  use jcup_time, only : operator(==), is_exchange_step
  implicit none
  type(varp_type), pointer :: data_type
  integer, intent(IN)      :: comp_id
  integer :: j
  logical :: res
  
  res = .true.

  if (.not.data_type%sd%is_send) then
    call put_log("Data : "//trim(data_type%name)//", send_flag /= 1, put data skip", 1)
    res = .false.
    return
  end if

  if (data_type%sd%is_average) return 

  if (is_first_step()) return

  do j = 1, data_type%sd%num_of_my_recv_data
      if (is_exchange_step(comp_id, 1, data_type%sd%my_recv_conf(j)%interval)) then
          res = .true.
          return
       end if
  end do

  call put_log("Current time is not send step, put data skipped, data : "//trim(data_type%name), 1)
  res = .false.

end function jcup_isPutOK

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isPutOK_org(data_type, time)
  use jcup_config, only : isSendData, get_send_comp_id_from_data_name, is_mean_data, is_put_step_data
  use jcup_utils, only  :  put_log
  use jcup_data, only : varp_type, get_time, get_data_name
  use jcup_time, only : operator(==)
  implicit none
  type(varp_type), pointer :: data_type
  type(time_type), intent(IN) :: time
  character(len=STR_SHORT)    :: name
 
  name = get_data_name(data_type)

  jcup_isPutOK_org = .true.

  if (.not.isSendData(DATA_NAME = name)) then
    call put_log("Data : "//trim(name)//", send_flag /= 1, put data skip", 1)
    jcup_isPutOK_org = .false.
    return
  end if


  if (is_mean_data(get_send_comp_id_from_data_name(name), name)) return 

  if (is_first_step()) return

  jcup_isPutOK_org = .false.

  !!!if (time == get_time(data_type)) then
  !!!!  call put_log("This data has been put, get data skipped, data : "//trim(name), 1)
  !!!!  return
  !!!!end if

  if (.not.is_put_step_data(data_type%sd)) then
    call put_log("Current time is not send step, put data skipped, data : "//trim(name), 1)
    return
  end if

  jcup_isPutOK_org = .true.

end function jcup_isPutOK_org

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put 1d data
!> @param[in] data_type send data type
!> @param[in] data array of send data
! 2014/10/22 [MOD] get_current_time -> get_before_time
! 2014/10/28 [MOD] time%delta_t -> call get_delta_t(delta_t)
! 2015/04/01 [ADD] add data_scalar send
! 2018/02/07 [MOD] data_scalar -> data_vector
subroutine jcup_put_data_1d_double(data_type, data, data_vector)
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_buffer, only : put_send_data
  use jcup_config, only : is_mean_data, send_data_conf_type, get_send_data_conf_ptr, is_put_step_data, &
                          get_send_comp_id_from_data_name, set_current_conf, get_comp_exchange_type, &
                          get_average_data_name, get_send_data_id_mdf
  use jcup_time, only : time_type, get_before_time, cal_next_exchange_time, get_delta_t
  use jcup_data, only : varp_type, get_comp_id, &
                        get_data_name, &
                        is_data_defined
  use jcup_data, only : set_time
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), intent(IN) :: data(:)
  real(kind=8), optional, intent(IN) :: data_vector(:)

  type(time_type)   :: time, next_time
  character(STR_SHORT) :: average_data_name
  type(send_data_conf_type), pointer :: sd
  real(kind=8), pointer :: averaging_weight(:)
  integer :: delta_t
  integer :: my_comp_id
  integer :: i, di
  character(len=STR_SHORT) :: data_name
  logical :: is_average

  call put_log("--------------------------------- jcup_put_data ------------------------------------")

  if (.not.associated(data_type)) call error("jcup_put_data_1d_double", "data_type is not associated")

  !!!!!!call check_put_array_size(data_type, size(data,1), size(data,2), 1)

  my_comp_id = get_comp_id(data_type)

  call set_current_conf(my_comp_id)

  call get_before_time(my_comp_id, 1, time)

  data_name = get_data_name(data_type)

  !!!if (.not.is_data_defined(my_comp_id, data_name)) then
  !!!  call error("jcup_put_data_2d_double", "data : "//trim(data_name) &
  !!!                         //" is not defined (subroutine jcup_def_data is not called ")
  !!!end if

  sd => data_type%sd !get_send_data_conf_ptr(DATA_NAME = data_name)

  do i = 1, sd%num_of_my_recv_data

    if ((get_comp_exchange_type(my_comp_id, sd%my_recv_conf(i)%model_id) == IMMEDIATE_SEND_RECV).or. &
        (sd%my_recv_conf(i)%time_lag == 0)) then ! 2013.09.18 [ADD]
      call jcup_put_send_data_1d_double(sd, sd%my_recv_conf(i), data)
      if (sd%num_of_my_recv_data == 1) return
    end if
  end do

  if (.not.jcup_isPutOK(data_type, my_comp_id)) return

  allocate(averaging_weight(size(data)))
  averaging_weight(:) = 1.d0

  if (data_type%sd%is_average) then !if (is_mean_data(my_comp_id, data_name)) then
    do i = 1, sd%num_of_my_recv_data
      if (get_comp_exchange_type(my_comp_id, sd%my_recv_conf(i)%model_id) == IMMEDIATE_SEND_RECV) cycle ! skip immediate_send_recv

      is_average = sd%my_recv_conf(i)%is_average
      if (sd%my_recv_conf(i)%is_average) then
        if (.not.is_first_step()) then ! average data

          average_data_name = trim(get_average_data_name(data_name,sd%my_recv_conf(i)%model_id,sd%my_recv_conf(i)%name))

          call get_delta_t(my_comp_id, 1, delta_t) ! 2014/10/28 [ADD]

          averaging_weight(:) = dble(delta_t)/dble(sd%my_recv_conf(i)%interval) ! 2021/04/01 [MOD]

          do di = 1, size(data)
            if (data(di) == fill_value) averaging_weight(di) = 0.d0
          end do

          call cal_next_exchange_time(my_comp_id, 1, sd%my_recv_conf(i)%interval, next_time)
          
          call put_send_data(data, next_time, my_comp_id, &
                             !get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             get_send_data_id_mdf(sd%my_recv_conf(i)%model_id, sd%data_id, is_average), &
                             average_data_name, .true., fill_value, averaging_weight)

        else ! first step of average data
          call put_send_data(data, time, my_comp_id, &
                             !get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             get_send_data_id_mdf(sd%my_recv_conf(i)%model_id, sd%data_id, is_average), &
                             data_name, .false., fill_value, averaging_weight)
        end if
      else  ! non average data
        if ((is_first_step()).or.(is_put_step_data(sd))) then
           call put_send_data(data, time, my_comp_id, &
                              !get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                              get_send_data_id_mdf(sd%my_recv_conf(i)%model_id, sd%data_id, is_average), &
                              data_name, .false., fill_value, averaging_weight)
        end if
      end if
    end do
  else
    call put_send_data(data, time, my_comp_id, &
                       !get_send_data_id(0, data_name, .false.), &
                       get_send_data_id_mdf(0, sd%data_id, .false.), &
                       data_name, .false., fill_value, averaging_weight)
    !!!!!call put_send_data(data, time, current_comp_id, &
    !!!!!                   get_send_data_id(0, data_name, .false.), data_name, .false., 1.d0)
  end if

  deallocate(averaging_weight)

  if (present(data_vector)) then ! 2018/02/7
    call send_data_vector(data_type, data_vector)
  end if

  call set_time(data_type, time)

end subroutine jcup_put_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put 2.5d data
!> @param[in] data_type send data type
!> @param[in] data array of send data
! 2014/07/17 [MOD] delete num_of_data > @param[in] num_of_data number of 2.5D data
! 2014/10/22 [MOD] get_current_time -> get_before_time
! 2014/10/28 [MOD] time%delta_t -> call get_delta_t(delta_t)
! 2015/04/03 [ADD] add data_scalar
! 2015/07/15 [MOD] add immediate send process
! 2018/02/07 [ADD] data_scalar -> data_vector
subroutine jcup_put_data_25d_double(data_type, data, data_vector)
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_buffer, only : put_send_data
  use jcup_config, only : is_mean_data, send_data_conf_type, get_send_data_conf_ptr, is_put_step_data, &
                          get_send_comp_id_from_data_name, set_current_conf, get_comp_exchange_type, &
                          get_average_data_name, get_send_data_id_mdf
  use jcup_time, only : time_type, get_before_time, cal_next_exchange_time, get_delta_t
  use jcup_data, only : varp_type, is_data_defined, get_comp_id, get_data_name, set_time
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), intent(IN) :: data(:,:)
  real(kind=8), optional, intent(IN) :: data_vector(:)
  type(time_type)   :: time, next_time
  character(STR_SHORT) :: average_data_name
  type(send_data_conf_type), pointer :: sd
  real(kind=8), pointer :: averaging_weight(:,:)
  integer :: delta_t
  integer :: my_comp_id
  integer :: i, di, dj
  character(len=STR_SHORT) :: data_name
  logical :: is_average

  call put_log("--------------------------------- jcup_put_data ------------------------------------")

  if (.not.associated(data_type)) call error("jcup_put_data_25d_double", "data_type is not associated")

  !!!!call check_put_array_size(data_type, size(data,1), size(data,2), 1)

  my_comp_id = get_comp_id(data_type)
  call set_current_conf(my_comp_id)
  call get_before_time(my_comp_id, 1, time)

  data_name = get_data_name(data_type)

  if (.not.jcup_isPutOK(data_type, my_comp_id)) return

  !!!if (.not.is_data_defined(my_comp_id, data_name)) then
  !!!  call error("jcup_put_data_2d_double", "data : "//trim(data_name) &
  !!!                        //" is not defined (subroutine jcup_def_data is not called) ")
  !!!end if

  sd => data_type%sd !get_send_data_conf_ptr(DATA_NAME = data_name)

  do i = 1, sd%num_of_my_recv_data
    if ((get_comp_exchange_type(my_comp_id, sd%my_recv_conf(i)%model_id) == IMMEDIATE_SEND_RECV).or. &
         sd%my_recv_conf(i)%time_lag == 0) then
      call jcup_put_send_data_25d_double(sd, sd%my_recv_conf(i), data)
      if (sd%num_of_my_recv_data == 1) return
    end if
  end do

  allocate(averaging_weight(size(data,1), size(data,2)))
  averaging_weight(:,:) = 1.d0

  if (data_type%sd%is_average) then  !if (is_mean_data(my_comp_id, data_name)) then
    do i = 1, sd%num_of_my_recv_data

      is_average = sd%my_recv_conf(i)%is_average

      if (sd%my_recv_conf(i)%is_average) then
        if (.not.is_first_step()) then

          average_data_name = trim(get_average_data_name(data_name,sd%my_recv_conf(i)%model_id,sd%my_recv_conf(i)%name))

          call get_delta_t(my_comp_id, 1, delta_t) ! 2014/10/28 [ADD]

          !averaging_weight = dble(time%delta_t)/dble(sd%my_recv_conf(i)%interval)

          averaging_weight(:,:) = dble(delta_t)/dble(sd%my_recv_conf(i)%interval) ! 2021/04/01 [MOD]

          do dj = 1, size(data, 2)
             do di = 1, size(data, 1)
                if (data(di, dj) == fill_value) averaging_weight(di, dj) = 0.d0
             end do
          end do       

          call cal_next_exchange_time(my_comp_id, 1, sd%my_recv_conf(i)%interval, next_time)
          call put_send_data(data, next_time, my_comp_id, & ! 2014/11/19 [MOD] current_comp_id -> my_comp_id
                             !get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             get_send_data_id_mdf(sd%my_recv_conf(i)%model_id, sd%data_id, is_average), &
                             average_data_name, .true., fill_value, averaging_weight)

        else ! first step of average data
          call put_send_data(data, time, my_comp_id, &
                             !get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             get_send_data_id_mdf(sd%my_recv_conf(i)%model_id, sd%data_id, is_average), &
                             data_name, .false., fill_value, averaging_weight)
        end if
      else ! non average data
        if (is_put_step_data(sd)) then
           call put_send_data(data, time, my_comp_id, &
                              !get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                              get_send_data_id_mdf(sd%my_recv_conf(i)%model_id, sd%data_id, is_average), &
                              data_name, .false., fill_value, averaging_weight)
        end if
      end if
    end do
  else
     call put_send_data(data, time, my_comp_id, &
                        !get_send_data_id(0, data_name, .false.), &
                        get_send_data_id_mdf(0, sd%data_id, .false.), &
                        data_name, .false., fill_value, averaging_weight)
  end if

  deallocate(averaging_weight)

  if (present(data_vector)) then ! 2018/02/08
    call send_data_vector(data_type, data_vector)
  end if


  call set_time(data_type, time)

end subroutine jcup_put_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/10/22 [MOD] is_recv_step_data -> is_get_step_data
logical function jcup_isGetOK(data_type, time)
  use jcup_config, only : is_get_step_data, isRecvData
  use jcup_utils, only  : put_log, error
  use jcup_time, only : operator(==)
  use jcup_data, only : varg_type, get_data_name, get_time
  implicit none
  type(varg_type), pointer :: data_type
  type(time_type), intent(IN) :: time
  character(len=STR_SHORT) :: name


  jcup_isGetOK = .false.
  name = get_data_name(data_type)

  !!!if (time == get_time(data_type)) then
  !!!  call put_log("This data has been gotten, get data skipped, data : "//trim(name), 1)
  !!!  return
  !!!end if

  if (.not.isRecvData(DATA_NAME = name)) then
    call error("jcup_isGetOK", "Data : "//trim(name)//", recv_flag /= 1, invalid subroutine call jcup_get_data")
    return
  end if

  if (.not.is_get_step_data(name)) then
    call put_log("Current time is not recv step, get data skipped, data : "//trim(name), 1)
    return
  end if

  jcup_isGetOK = .true.

end function jcup_isGetOK

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get 1d data
!> @param[in] data_type recv data type
!> @param[in] data array of recv data
! 2014/10/22 [MOD] get_current_time -> get_before_time
! 2015/04/02 [ADD] add data_scalar
! 2018/02/07 [ADD] data_scalar -> data_vector
subroutine jcup_get_data_1d_double(data_type, data, data_vector, is_recv_ok)
  use jcup_utils, only : error, put_log
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_buffer, only : get_recv_data
  use jcup_time, only : time_type, get_before_time, operator(==)
  use jcup_config, only : recv_data_conf_type, get_send_comp_id_from_data_name, get_send_data_name, is_mean_data, &
                          get_recv_data_id_from_data_name, get_recv_comp_id_from_data_name, &
                          set_current_conf, get_recv_data_conf_ptr, get_comp_exchange_type, isRecvData
  use jcup_data, only : varg_type, get_comp_id, get_data_name, set_time
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), intent(INOUT) :: data(:)
  real(kind=8), intent(OUT), optional :: data_vector(:)
  logical, intent(OUT), optional :: is_recv_ok
  type(time_type) :: time
  logical :: is_data_reset
  integer :: my_comp_id
  character(len=STR_SHORT) :: send_data_name
  character(len=STR_SHORT) :: data_name
  type(recv_data_conf_type), pointer :: rd
  real(kind=8) :: scalar_data
  
  call put_log("--------------------------------- jcup_get_data ------------------------------------")

  if (.not.associated(data_type)) call error("jcup_get_data_1d_double", "data_type is not associated")

  my_comp_id = get_comp_id(data_type)

  call set_current_conf(my_comp_id)
  call get_before_time(my_comp_id, 1, time)
  
  if (present(is_recv_ok)) is_recv_ok = .false.

  data_name = get_data_name(data_type)

  if (.not.isRecvData(DATA_NAME = data_name)) then
    call error("jcup_isGetOK", "Data : "//trim(data_name)//", recv_flag /= 1, invalid subroutine call jcup_get_data")
    return
  end if

  rd => get_recv_data_conf_ptr(data_name)

  if ((get_comp_exchange_type(my_comp_id, rd%send_model_id) == IMMEDIATE_SEND_RECV).or. &
      (rd%time_lag == 0)) then ! 2013.09.18 [ADD]
    !write(0,*) "jcup_get_data_1d_double, immediate recv start"

    if (present(is_recv_ok)) is_recv_ok = .true.
    call jcup_recv_get_data_1d_double(rd%send_model_id, rd, data)
    return
  end if

  if (.not.jcup_isGetOK(data_type, time)) return 

  if (present(is_recv_ok)) is_recv_ok = .true.

  send_data_name = get_send_data_name(data_name)
  is_data_reset = .not.is_mean_data(get_send_comp_id_from_data_name(send_data_name), send_data_name)

  !!!my_comp_id = current_comp_id ! get_recv_comp_id_from_data_name(data_name)
  call get_recv_data(data, time, my_comp_id, get_recv_data_id_from_data_name(data_name), data_name, is_data_reset)

  if (present(data_vector)) then ! 2018/02/07
     call recv_data_vector(data_type, data_vector)
  end if

  call set_time(data_type, time)

end subroutine jcup_get_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get 2.5d data
!> @param[in] data_type recv data type
!> @param[in] data array of recv data
! 2014/07/17 [MOD] delete num_of_data > @param[in] num_of_data number of 2.5d data
! 2014/10/22 [MOD] get_current_time -> get_before_time
! 2015/04/03 [ADD] add data_scalar
! 2015/07/15 [MOD] add immediate recv process
! 2018/02/07 [MOD] data_scalar -> data_vector
subroutine jcup_get_data_25d_double(data_type, data, data_vector, is_recv_ok)
  use jcup_utils, only : error, put_log
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_buffer, only : get_recv_data 
  use jcup_time, only : time_type, get_before_time
  use jcup_config, only : recv_data_conf_type, get_send_comp_id_from_data_name, get_send_data_name, is_mean_data, &
                          get_recv_data_id_from_data_name, get_recv_comp_id_from_data_name, &
                          set_current_conf, get_recv_data_conf_ptr, get_comp_exchange_type
  use jcup_data, only : varg_type, get_comp_id, get_data_name, set_time
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), intent(INOUT) :: data(:,:)
  real(kind=8), intent(OUT), optional :: data_vector(:)
  logical, intent(OUT), optional :: is_recv_ok
  type(time_type) :: time
  logical :: is_data_reset
  integer :: my_comp_id
  character(len=STR_SHORT) :: send_data_name
  character(len=STR_SHORT) :: data_name
  type (recv_data_conf_type), pointer :: rd
  real(kind=8) :: scalar_data
  
  call put_log("--------------------------------- jcup_get_data ------------------------------------")

  if (.not.associated(data_type)) call error("jcup_get_data_25d_double", "data_type is not associated")


  my_comp_id = get_comp_id(data_type)
  call set_current_conf(my_comp_id)
  call get_before_time(my_comp_id, 1, time)

  if (present(is_recv_ok)) is_recv_ok = .false.

  if (.not.jcup_isGetOK(data_type, time)) return 

  data_name = get_data_name(data_type)

  rd => get_recv_data_conf_ptr(data_name)

  if ((get_comp_exchange_type(my_comp_id, rd%send_model_id) == IMMEDIATE_SEND_RECV).or. &
      (rd%time_lag == 0)) then ! 2013.09.18 [ADD]
    !write(0,*) "jcup_get_data_1d_double, immediate recv start"

    if (present(is_recv_ok)) is_recv_ok = .true.
    call jcup_recv_get_data_25d_double(rd%send_model_id, rd, data)
    return
  end if

  if (present(is_recv_ok)) is_recv_ok = .true.

  send_data_name = get_send_data_name(data_name)
  is_data_reset = .not.is_mean_data(get_send_comp_id_from_data_name(send_data_name), send_data_name)

  !my_comp_id = current_comp_id !get_recv_comp_id_from_data_name(data_name)
  call get_recv_data(data, time, current_comp_id, get_recv_data_id_from_data_name(data_name), data_name, is_data_reset)

  !call recv_data_scalar(data_type, scalar_data)
  if (present(data_vector)) then ! 2015/04/03
     call recv_data_vector(data_type, data_vector)
  end if

  call set_time(data_type, time)

end subroutine jcup_get_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_put_send_data_1d_double(sd, rd, data)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_config, only : send_data_conf_type, recv_data_conf_type, GetMappingTag, get_recv_data_id_from_data_name, &
                          get_average_data_name, get_send_data_id
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, send_data_1d, exchange_data_comp, get_data, interpolate_data_1d
  use jcup_comp, only : get_component_relation, is_my_component
  use jcup_data, only : varp_type, get_comp_id, get_data_name, is_data_defined, set_time
  use jcup_buffer, only : put_send_data, put_recv_data
  implicit none
  type(send_data_conf_type), pointer :: sd
  type(recv_data_conf_type)          :: rd
  real(kind=8), intent(IN) :: data(:)
  integer :: send_comp_id
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  character(len=STR_SHORT) :: data_name
  character(len=STR_SHORT) :: recv_data_name
  type(time_type) :: time
  integer :: exchange_tag(1)
  real(kind=8), pointer :: averaging_weight(:)

  data_name = sd%name

  if (.not.jcup_isSendOK(trim(data_name))) return

  recv_comp_id = rd%model_id
  current_comp_id = sd%model_id
  send_comp_id = current_comp_id

  !if (recv_model_id == current_comp_id) then
  !  call error("jcup_SendData2D_double","dest_model_name : "//trim(dest_model_name)//" error")
  !end if

  allocate(averaging_weight(size(data)))
  averaging_weight(:) = 1.d0

  call set_current_mapping_tag(sd%model_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name))

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
  ni = ie-is+1

  select case(get_component_relation(current_comp_id, recv_comp_id))
  case (COMP_PARALLEL)
    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

    buffer_double1d(1:ni,1) = data(1:ni)

    call set_data(buffer_double1d)    
    call send_data_1d(sd%model_id, recv_comp_id, send_mapping_tag(sd%model_id, recv_comp_id), &
                      DOUBLE_DATA, 1, rd%data_id)

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

  case (COMP_SERIAL, COMP_SUBSET)

    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

    time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

    call put_send_data(data, time, current_comp_id, &
                       get_send_data_id(0, data_name, rd%is_average), data_name, .false., fill_value, averaging_weight)

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

  case (COMP_SUPERSET)

    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

    buffer_double1d(1:ni,1) = data(1:ni)

    call set_data(buffer_double1d)    

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, 1, rd%data_id, DATA_2D)
    
    if (is_my_component(recv_comp_id)) then
      exchange_tag(1) = rd%exchange_tag
      call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                               DOUBLE_DATA, 1, exchange_tag)

      buffer_double1d = 0.d0

      call get_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                    buffer_double1d(:,:), 1)

      call get_my_local_area(recv_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

      ni = ie-is+1

      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      recv_data_name = rd%name
  
      call put_recv_data(buffer_double1d(1:ni,1), time, recv_comp_id, &
                         get_recv_data_id_from_data_name(recv_data_name), recv_data_name)

    end if

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

  case (COMP_OVERLAP)
    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

    if (is_my_component(recv_comp_id)) then
      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call put_send_data(data, time, current_comp_id, &
                         get_send_data_id(0, data_name, rd%is_average), data_name, .false., fill_value, averaging_weight)
    else
      buffer_double1d(1:ni,1) = data(1:ni)

      call set_data(buffer_double1d)    
      call send_data_1d(sd%model_id, recv_comp_id, send_mapping_tag(sd%model_id, recv_comp_id), &
                        DOUBLE_DATA, 1, rd%data_id)
    end if

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

  case default
    call error("jcup_put_send_data_1d_double", "immediate exchange parameter error")
  end select

  deallocate(averaging_weight)

end subroutine jcup_put_send_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2015/07/15 [NEW]
subroutine jcup_put_send_data_25d_double(sd, rd, data)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_config, only : send_data_conf_type, recv_data_conf_type, GetMappingTag, get_recv_data_id_from_data_name, &
                          get_average_data_name, get_send_data_id
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, send_data_1d, exchange_data_comp, get_data, interpolate_data_1d
  use jcup_comp, only : get_component_relation, is_my_component
  use jcup_data, only : varp_type, get_comp_id, get_data_name, is_data_defined, set_time
  use jcup_buffer, only : put_send_data, put_recv_data
  implicit none
  type(send_data_conf_type), pointer :: sd
  type(recv_data_conf_type)          :: rd
  real(kind=8), intent(IN) :: data(:,:)
  integer :: send_comp_id
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  character(len=STR_SHORT) :: data_name
  character(len=STR_SHORT) :: recv_data_name
  type(time_type) :: time
  integer :: exchange_tag(1)
  integer :: num_of_data
  real(kind=8), pointer :: averaging_weight(:,:)

  data_name = sd%name

  if (.not.jcup_isSendOK(trim(data_name))) return

  recv_comp_id = rd%model_id
  current_comp_id = sd%model_id
  send_comp_id = current_comp_id

  !if (recv_model_id == current_comp_id) then
  !  call error("jcup_SendData2D_double","dest_model_name : "//trim(dest_model_name)//" error")
  !end if

  allocate(averaging_weight(size(data,1), size(data,2)))
  averaging_weight(:,:) = 1.d0

  call set_current_mapping_tag(sd%model_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name))

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
  ni = ie-is+1

  num_of_data = size(data, 2)

  select case(get_component_relation(current_comp_id, recv_comp_id))
  case (COMP_PARALLEL)
    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)
    
    buffer_double1d(1:ni,1:num_of_data) = data(1:ni,1:num_of_data)

    call set_data(buffer_double1d)

    call send_data_1d(sd%model_id, recv_comp_id, send_mapping_tag(sd%model_id, recv_comp_id), &
                      DOUBLE_DATA, num_of_data, rd%data_id)

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

  case (COMP_SERIAL, COMP_SUBSET)

    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

    time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

    call put_send_data(data, time, current_comp_id, &
                       get_send_data_id(0, data_name, rd%is_average), data_name, .false., fill_value, averaging_weight)

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

  case (COMP_SUPERSET)

    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

    buffer_double1d(1:ni,:) = data(1:ni,:)

    call set_data(buffer_double1d)    

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, size(data,2), rd%data_id, DATA_2D)
    
    if (is_my_component(recv_comp_id)) then
      exchange_tag(1) = rd%exchange_tag
      call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                               DOUBLE_DATA, size(data, 2), exchange_tag)

      buffer_double1d = 0.d0

      call get_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                    buffer_double1d(:,:), size(data, 2))

      call get_my_local_area(recv_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

      ni = ie-is+1

      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      recv_data_name = rd%name
  
      call put_recv_data(buffer_double1d(1:ni,:), time, recv_comp_id, &
                         get_recv_data_id_from_data_name(recv_data_name), recv_data_name)

    end if

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

  case (COMP_OVERLAP)
    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

    if (is_my_component(recv_comp_id)) then
      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call put_send_data(data, time, current_comp_id, &
                         get_send_data_id(0, data_name, rd%is_average), data_name, .false., fill_value, averaging_weight)
    else
      buffer_double1d(1:ni,:) = data(1:ni,:)

      call set_data(buffer_double1d)    
      call send_data_1d(sd%model_id, recv_comp_id, send_mapping_tag(sd%model_id, recv_comp_id), &
                        DOUBLE_DATA, size(data, 2), rd%data_id)
    end if

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

  case default
    call error("jcup_put_send_data_25d_double", "immediate exchange parameter error")
  end select

  deallocate(averaging_weight)

end subroutine jcup_put_send_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_recv_get_data_1d_double(send_comp_id, rd, data)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_config, only : send_data_conf_type, recv_data_conf_type, GetRecvMappingTag, &
                          get_average_data_name, get_send_data_id
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, get_data, recv_data_1d, interpolate_data_1d, exchange_data_comp
  use jcup_comp, only : get_component_relation, is_my_component
  use jcup_data, only : varp_type, get_comp_id, get_data_name, is_data_defined, set_time
  use jcup_buffer, only : get_send_data
  use jcup_config, only : is_my_recv_data, isRecvData, is_recv_step_data
  implicit none
  integer, intent(IN) :: send_comp_id
  type(recv_data_conf_type)          :: rd
  real(kind=8), intent(INOUT) :: data(:)
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: data_name
  character(len=STR_SHORT) :: send_data_name
  type(time_type) :: time

  data_name = rd%name
  send_data_name = rd%send_data


  !if (.not.is_Initialize_completed) then
  !   call jcup_abnormal_end("jcup_isRecvOK", "jcup_SetMappigTable not called")
  !end if

  if (.not.is_my_recv_data(trim(data_name))) then
    call error("jcup_isRecvOK", &
           "Data "//trim(data_name)//" is not listed in coupler.conf file. Check your code and file")
  end if
  
  if (.not.isRecvData(DATA_NAME = data_name)) then
    call put_log("Data : "//trim(data_name)//", recv_flag /= 1, recv skip", 1)
    return
  end if

  !if (.not.jcup_isRecvOK(trim(data_name))) return

  recv_comp_id = rd%model_id

  current_comp_id = recv_comp_id

  !if (recv_model_id == current_comp_id) then
  !  call error("jcup_SendData2D_double","dest_model_name : "//trim(dest_model_name)//" error")
  !end if

  call set_current_mapping_tag(send_comp_id, recv_comp_id, GetRecvMappingTag(recv_comp_id, data_name))

  select case(get_component_relation(send_comp_id, recv_comp_id))
  case (COMP_PARALLEL)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

    call recv_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                   DOUBLE_DATA, 1, rd%data_id)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, 1, exchange_tag)

    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

  case(COMP_SERIAL, COMP_SUBSET)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

    if (is_my_component(send_comp_id)) then

      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call get_my_local_area(send_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
      ni = ie-is+1

      call get_send_data(buffer_double1d(1:ni,1), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, send_data_name, rd%is_average), &
                         trim(send_data_name))

      call set_data(buffer_double1d)    

    end if

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, 1, rd%data_id, DATA_2D)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, 1, exchange_tag)


    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

  case (COMP_SUPERSET)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)
    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

    ! do nothing

  case (COMP_OVERLAP)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

    if (is_my_component(send_comp_id)) then
      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call get_my_local_area(send_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
      ni = ie-is+1

      call get_send_data(buffer_double1d(1:ni,1), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, send_data_name, rd%is_average), &
                         trim(send_data_name))

      call set_data(buffer_double1d)    
    end if

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, 1, rd%data_id, DATA_2D)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, 1, exchange_tag)

    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

  case default
    call error("jcup_recv_get_data_1d_double", "immediate exchange parameter error")
  end select


  buffer_double1d = 0.d0

  call get_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                buffer_double1d(:,:), 1)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
  ni = ie-is+1

  data(1:ni) = buffer_double1d(1:ni,1)


end subroutine jcup_recv_get_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_recv_get_data_25d_double(send_comp_id, rd, data)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_config, only : send_data_conf_type, recv_data_conf_type, GetRecvMappingTag, &
                          get_average_data_name, get_send_data_id
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, get_data, recv_data_1d, interpolate_data_1d, exchange_data_comp
  use jcup_comp, only : get_component_relation, is_my_component
  use jcup_data, only : varp_type, get_comp_id, get_data_name, is_data_defined, set_time
  use jcup_buffer, only : get_send_data
  use jcup_config, only : is_my_recv_data, isRecvData, is_recv_step_data
  implicit none
  integer, intent(IN) :: send_comp_id
  type(recv_data_conf_type)          :: rd
  real(kind=8), intent(INOUT) :: data(:,:)
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  character(len=STR_SHORT) :: data_name
  character(len=STR_SHORT) :: send_data_name
  type(time_type) :: time
  integer :: num_of_data

  data_name = rd%name
  send_data_name = rd%send_data

  num_of_data = size(data, 2)

  !if (.not.is_Initialize_completed) then
  !   call jcup_abnormal_end("jcup_isRecvOK", "jcup_SetMappigTable not called")
  !end if

  if (.not.is_my_recv_data(trim(data_name))) then
    call error("jcup_isRecvOK", &
           "Data "//trim(data_name)//" is not listed in coupler.conf file. Check your code and file")
  end if
  
  if (.not.isRecvData(DATA_NAME = data_name)) then
    call put_log("Data : "//trim(data_name)//", recv_flag /= 1, recv skip", 1)
    return
  end if

  !if (.not.jcup_isRecvOK(trim(data_name))) return

  recv_comp_id = rd%model_id

  current_comp_id = recv_comp_id

  !if (recv_model_id == current_comp_id) then
  !  call error("jcup_SendData2D_double","dest_model_name : "//trim(dest_model_name)//" error")
  !end if

  call set_current_mapping_tag(send_comp_id, recv_comp_id, GetRecvMappingTag(recv_comp_id, data_name))

  select case(get_component_relation(send_comp_id, recv_comp_id))
  case (COMP_PARALLEL)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

    call recv_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                   DOUBLE_DATA, num_of_data, rd%data_id)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, num_of_data, exchange_tag)

    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

  case(COMP_SERIAL, COMP_SUBSET)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

    if (is_my_component(send_comp_id)) then

      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call get_my_local_area(send_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
      ni = ie-is+1

      call get_send_data(buffer_double1d(1:ni,:), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, send_data_name, rd%is_average), &
                         trim(send_data_name))

      call set_data(buffer_double1d)    

    end if

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, num_of_data, rd%data_id, DATA_2D)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, num_of_data, exchange_tag)


    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

  case (COMP_SUPERSET)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)
    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

    ! do nothing

  case (COMP_OVERLAP)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

    if (is_my_component(send_comp_id)) then
      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call get_my_local_area(send_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
      ni = ie-is+1

      call get_send_data(buffer_double1d(1:ni,:), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, send_data_name, rd%is_average), &
                         trim(send_data_name))

      call set_data(buffer_double1d)    
    end if

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, num_of_data, rd%data_id, DATA_2D)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, num_of_data, exchange_tag)

    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

  case default
    call error("jcup_recv_get_data_1d_double", "immediate exchange parameter error")
  end select


  buffer_double1d = 0.d0

  call get_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                buffer_double1d(:,:), num_of_data)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
  ni = ie-is+1

  data(1:ni,:) = buffer_double1d(1:ni,:)


end subroutine jcup_recv_get_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_put_array_size(data_type, s1, s2, s3)
  use jcup_utils, only : error
  use jcup_data, only : varp_type, data_array_size_ok
  implicit none
  type(varp_type), pointer :: data_type
  integer, intent(IN) :: s1, s2, s3

  if (.not.data_array_size_ok(data_type, s1, s2, s3)) then
    call error("check_put_array_size", "jcup_put_data, array size mismatch")
  end if

end subroutine check_put_array_size

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> send scalar data 
!> @param[in] data_name name of data
!> @param[in] data 
! 2015/04/01 [NEW]
! 2015/05/18 [MOD] add call jml_ReduceMeanLocal
! 2015/06/22 [MOD] jml_ReduceMeanLocal -> jml_ReduceSumLocal
subroutine send_data_scalar(varp, data)
  use jcup_constant, only : STR_SHORT
  use jcup_data, only : varp_type, get_data_name
  use jcup_config, only : send_data_conf_type, get_send_data_conf_ptr
  use jcup_mpi_lib, only : jml_SendLeader, jml_isLocalLeader, jml_ReduceSumLocal
  use jcup_time, only : is_exchange_step
  use jcup_utils, only : put_log
  implicit none
  type(varp_type), pointer :: varp
  real(kind=8), intent(IN) :: data
  type(send_data_conf_type), pointer :: sd
  character(len=STR_SHORT) :: data_name
  real(kind=8) :: data_array(1)
  integer :: dest
  integer :: tag
  integer :: i

  data_name = get_data_name(varp)

  sd => get_send_data_conf_ptr(DATA_NAME = data_name)

  !!!!!!if (.not.jml_isLocalLeader(sd%model_id)) return ! 2015/06/17

  data_array(1) = data
  !data_buffer(1) = data
  do i = 1, sd%num_of_my_recv_data  
    if (.not.is_exchange_step(sd%model_id, 1, sd%my_recv_conf(i)%interval)) then
      !write(0,*) "send_data_scalar skipped ", sd%my_recv_conf(i)%interval
      cycle
    end if
   
    !call jml_ReduceMeanLocal(sd%model_id , data, data_array(1)) ! cal. mean value
    call jml_ReduceSumLocal(sd%model_id , data, data_array(1)) ! cal. sum value 2015/06/22 [MOD]

    dest = sd%my_recv_conf(i)%model_id - 1
    tag  = sd%my_recv_conf(i)%data_id
    call put_log("-------------------------------------------------------------------------------")
    call put_log("send gmean data, data name = "//trim(data_name))
    call jml_SendLeader(data_array, 1, 1, dest, tag)
  end do

end subroutine send_data_scalar

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> send scalar data 
!> @param[in] data_name name of data
!> @param[in] data 
! 2018/02/07 [NEW]
subroutine send_data_vector(varp, data)
  use jcup_constant, only : STR_SHORT
  use jcup_data, only : varp_type, get_data_name
  use jcup_config, only : send_data_conf_type, get_send_data_conf_ptr
  use jcup_mpi_lib, only : jml_SendLeader, jml_isLocalLeader, jml_ReduceSumLocal
  use jcup_time, only : is_exchange_step
  use jcup_utils, only : put_log
  implicit none
  type(varp_type), pointer :: varp
  real(kind=8), intent(IN) :: data(:)
  type(send_data_conf_type), pointer :: sd
  character(len=STR_SHORT) :: data_name
  integer :: dest
  integer :: tag
  integer :: i

  data_name = get_data_name(varp)

  sd => get_send_data_conf_ptr(DATA_NAME = data_name)

  !!!!!!if (.not.jml_isLocalLeader(sd%model_id)) return ! 2015/06/17

  do i = 1, sd%num_of_my_recv_data  
    if (.not.is_exchange_step(sd%model_id, 1, sd%my_recv_conf(i)%interval)) then
      !write(0,*) "send_data_scalar skipped ", sd%my_recv_conf(i)%interval
      cycle
    end if
   
    !call jml_ReduceMeanLocal(sd%model_id , data, data_array(1)) ! cal. mean value
    !call jml_ReduceSumLocal(sd%model_id , data, data_array(1)) ! cal. sum value 2015/06/22 [MOD]

    dest = sd%my_recv_conf(i)%model_id - 1
    tag  = sd%my_recv_conf(i)%data_id
    call put_log("-------------------------------------------------------------------------------")
    call put_log("send vector data, data name = "//trim(data_name))
    call jml_SendLeader(data, 1, size(data), dest, tag)
 end do

end subroutine send_data_vector

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> recv scalar data 
!> @param[in] data_name name of data
!> @param[in] data 
! 2015/04/01 [NEW]
subroutine recv_data_scalar(varg, data)
  use jcup_constant, only : STR_SHORT, NO_DATA
  use jcup_mpi_lib, only : jml_ProbeLeader, jml_RecvLeader
  use jcup_data, only : varg_type, get_data_name
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr
  use jcup_utils, only : put_log
  implicit none
  type(varg_type), pointer :: varg
  real(kind=8), intent(INOUT) :: data
  character(len=STR_SHORT) :: data_name
  real(kind=8) :: data_array(1)
  type(recv_data_conf_type), pointer :: rd
  integer :: source, tag

  data_name = get_data_name(varg)
  rd => get_recv_data_conf_ptr(DATA_NAME = data_name)

  source = rd%send_model_id - 1
  tag    = rd%data_id

  if (jml_ProbeLeader(source, tag)) then
    !write(0,*) "recv_data_scalar 1 ", source, tag
    call put_log("-------------------------------------------------------------------------------")
    call put_log("recv gmean data, data name = "//trim(data_name))
    call jml_RecvLeader(data_array, 1, 1, source, tag)
    !call jml_BcastLocal(rd%model_id, data_array, 1, 1)
    data = data_array(1)
  else
    data = NO_DATA
    !write(0,*) "recv_data_scalar 2 ", source, tag
  end if

end subroutine recv_data_scalar

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> recv vector data
!> @param[in] data_name name of data
!> @param[in] data 
! 2018/02/07 [NEW]
subroutine recv_data_vector(varg, data)
  use jcup_constant, only : STR_SHORT, NO_DATA
  use jcup_mpi_lib, only : jml_ProbeLeader, jml_RecvLeader
  use jcup_data, only : varg_type, get_data_name
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr
  use jcup_utils, only : put_log
  implicit none
  type(varg_type), pointer :: varg
  real(kind=8), intent(INOUT) :: data(:)
  character(len=STR_SHORT) :: data_name
  type(recv_data_conf_type), pointer :: rd
  integer :: source, tag

  data_name = get_data_name(varg)
  rd => get_recv_data_conf_ptr(DATA_NAME = data_name)

  source = rd%send_model_id - 1
  tag    = rd%data_id

  if (jml_ProbeLeader(source, tag)) then
    call put_log("-------------------------------------------------------------------------------")
    call put_log("recv vector data, data name = "//trim(data_name))
    call jml_RecvLeader(data, 1, size(data), source, tag)
    !call jml_BcastLocal(rd%model_id, data_array, 1, 1)
  else
    data = NO_DATA
    !write(0,*) "recv_data_scalar 2 ", source, tag
  end if

end subroutine recv_data_vector

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> write gmean data for restart
! 2015/04/06 [NEW]
subroutine write_all_scalar_data(fid, comp_id)
  use jcup_utils, only : put_log, IntToStr
  use jcup_constant, only : STR_SHORT
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name
  use jcup_config, only : recv_data_conf_type, get_num_of_recv_data, get_recv_data_conf_ptr_from_id
  use jcup_mpi_lib, only : jml_GetLeaderRank, jml_ProbeLeader, jml_RecvLeader
  implicit none
  integer, intent(IN) :: fid
  integer, intent(IN) :: comp_id
  type(recv_data_conf_type), pointer :: rd
  real(kind=8) :: data_array(1)
  character(len=STR_SHORT) :: comp_name
  integer :: source, tag
  integer :: j

    do j = 1, get_num_of_recv_data(comp_id)
       rd => get_recv_data_conf_ptr_from_id(comp_id, j)
       source = rd%send_model_id - 1
       tag    = rd%data_id
       if (jml_ProbeLeader(source, tag)) then
         call jml_RecvLeader(data_array, 1, 1, source, tag)
         comp_name = trim(get_component_name(rd%send_model_id))
         write(fid, *) comp_name
         write(fid, *) tag
         write(fid, *) data_array(1)
         call put_log("write restart gmean data to "//trim(comp_name)//", data tag = "//trim(IntToStr(tag)))
       end if
    end do  


end subroutine write_all_scalar_data

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> clean up remained data
! 2015/04/03 [NEW]
! 2015/06/17 [MOD]
subroutine recv_all_scalar_data()
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_config, only : recv_data_conf_type, get_num_of_recv_data, get_recv_data_conf_ptr_from_id
  use jcup_mpi_lib, only : jml_GetLeaderRank, jml_ProbeLeader, jml_RecvLeader, &
                           jml_ProbeAll, jml_RecvAll
  use mpi
  implicit none
  type(recv_data_conf_type), pointer :: rd
  real(kind=8) :: data_array(1)
  integer :: source, tag
  logical :: recv_flag
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (.not.is_my_component(i)) cycle
    do j = 1, get_num_of_recv_data(i)
         rd => get_recv_data_conf_ptr_from_id(i, j)
         source = rd%send_model_id - 1
         do  while(jml_ProbeAll(source))
             call jml_RecvAll(source) !Leader(data_array, 1, 1, source, MPI_ANY_TAG)
         end do
    end do  
  end do

  !do i = 1, get_num_of_total_component()
  !  if (.not.is_my_component(i)) cycle
  !  write(0, *) "recv_all_scalar_data of component loop ", i
  !  do 
  !    recv_flag = .false.
  !    do j = 1, get_num_of_recv_data(i)
  !       rd => get_recv_data_conf_ptr_from_id(i, j)
  !       source = rd%send_model_id - 1
  !       tag    = rd%data_id
  !      write(0,*) "recv_all_scalar_data ", i, j, source, tag
  !       if (jml_ProbeLeader(source, tag)) then
  !         write(0,*) "recv_all_scalar_data recv leader ", data_array(1)
  !         call jml_RecvLeader(data_array, 1, 1, source, tag)
  !         recv_flag = .true.
  !       end if
  !    end do  
  !    if (.not.recv_flag) exit
  !  end do
  !end do

end subroutine recv_all_scalar_data


end module jcup_exchange
