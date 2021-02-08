!====================================================================================================

module jal_time
  use jcup_constant, only : STR_SHORT, STR_LONG
  use jcup_time, only : time_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!
  
  public :: jal_time_init
  public :: jal_set_time
  public :: jal_time_end
  public :: jal_get_my_comp_id
  public :: jal_get_before_time
  public :: jal_get_current_time

!--------------------------------   private  ---------------------------------!

  integer, parameter :: STATUS_NOSR = 0
  integer, parameter :: STATUS_SEND = 1
  integer, parameter :: STATUS_RECV = 2
  integer, parameter :: STATUS_SR   = 3
  integer, parameter :: STATUS_RR   = 4
  
  integer :: win_comm
  integer :: ierror

  integer :: my_comp_id

  type comp_time_type
    character(len=STR_SHORT) :: comp_name
    type(time_type), pointer :: current_time
    type(time_type), pointer :: before_time
    integer(kind=8) :: current_sec
    integer(kind=4) :: current_delt
    integer(kind=8) :: before_sec
    integer(kind=4) :: before_delt
    integer(kind=8) :: nstep 
    integer         :: sr_status
  end type comp_time_type

  integer :: num_of_total_comp
  type(comp_time_type), pointer :: comp_time(:)
  integer(kind=8), pointer :: send_sec(:) ! send second of target component
  integer(kind=8), pointer :: recv_sec(:) ! recv second of target component
  
  logical, allocatable :: send_data_flag(:)
  logical, allocatable :: recv_data_flag(:)
  integer, allocatable :: num_of_recv(:)
  logical :: is_first_step = .true.

  integer :: step_counter = 0

  logical, allocatable :: is_srr(:)
  
contains

!====================================================================================================

subroutine jal_time_init()
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name
  use jal_mpi_lib, only : jal_init_window, jal_set_window_data, jal_get_window_data
  implicit none
  integer :: i
  integer, pointer :: data(:)
  
  num_of_total_comp = get_num_of_total_component()

  allocate(comp_time(num_of_total_comp))
  allocate(send_data_flag(num_of_total_comp))
  allocate(recv_data_flag(num_of_total_comp))
  allocate(num_of_recv(num_of_total_comp))
  allocate(send_sec(num_of_total_comp))
  allocate(recv_sec(num_of_total_comp))
  allocate(is_srr(num_of_total_comp))
  is_srr = .false.
    
  is_first_step = .true.
  
  do i = 1, num_of_total_comp

    if (is_my_component(i)) my_comp_id = i

    allocate(comp_time(i)%current_time)
    allocate(comp_time(i)%before_time)
    comp_time(i)%current_time%ss = 0
    comp_time(i)%before_time%ss = 0
    comp_time(i)%current_sec = 0
    comp_time(i)%current_delt = 0
    comp_time(i)%before_sec = 0
    comp_time(i)%before_delt = 0
    comp_time(i)%nstep = 0 
    comp_time(i)%comp_name = get_component_name(i)

    send_sec(i) = -1
    recv_sec(i) = -1
    
  end do
  
  call jal_init_window(my_comp_id, 1)

  allocate(data(1))
  data(1) = 0
  call jal_set_window_data(data, 1)

  do i = 1, num_of_total_comp
     if (.not.is_my_component(i)) then
       call jal_get_window_data(i-1, 0, data, 1)
     end if
  end do

  deallocate(data)
  
  
end subroutine jal_time_init

!====================================================================================================

subroutine jal_set_time(delta_t)
  use jcup_mpi_lib, only : jcup_get_myrank_global => jml_GetMyrankGlobal
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_array => recv_array_from_send_model
  use jcup_comp, only : is_my_component
  use jcup_utils, only : put_log
  use jal_buffer, only : jal_remove_past_send_data, jal_remove_past_recv_data
  use jal_exchange, only : jal_init_exchange, jal_send_data, jal_recv_data, jal_interpolate_time, jal_is_exchange, &
                           jal_send_array_nowait, jal_recv_array
  use jcup_mpi_lib, only : jml_send_waitall
  use jcup_config, only : get_comp_exchange_type
  use jcup_constant, only : ASSYNC_SEND_RECV
  implicit none
  integer(kind=4), intent(IN) :: delta_t
  integer :: i, j
  real(kind=8) :: time_array(2), send_time_array(2)
  integer :: loop_length
  logical :: recv_flag
  character(len=STR_LONG) :: log_str

  if (is_first_step) call jal_init_exchange(my_comp_id)

  
  step_counter = step_counter + 1
  
  call put_log("------------------------------------------------------------------------------------")
  call put_log("--------------------------------- jal_set_time  ------------------------------------")
  call put_log("------------------------------------------------------------------------------------")

  call jal_remove_past_send_data(jal_get_before_time(my_comp_id), my_comp_id)

  comp_time(my_comp_id)%before_delt = comp_time(my_comp_id)%current_delt
  comp_time(my_comp_id)%current_delt = delta_t
  comp_time(my_comp_id)%nstep = comp_time(my_comp_id)%nstep + 1

  time_array(1) = comp_time(my_comp_id)%current_sec
  time_array(2) = delta_t

  write(log_str, *) "current time = ", time_array(1), ", delta t = ", time_array(2)
  call put_log(trim(log_str))

  send_data_flag(:) = .false.

  num_of_recv(:) = 0

  !if (is_first_step) then
  !   loop_length = my_comp_id-1
  !else
     loop_length = num_of_total_comp
  !end if
  
  do i = 1, num_of_total_comp
     if (is_my_component(i)) cycle
     if (get_comp_exchange_type(my_comp_id, i) /= ASSYNC_SEND_RECV) cycle
     !if (is_send_time(comp_time(my_comp_id)%before_sec, &
     !                 comp_time(my_comp_id)%current_sec, &
     !                 comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
     !                 comp_time(i)%current_sec, comp_time(i)%current_sec + comp_time(i)%current_delt, send_sec(i))) then
     !   send_data_flag(i) = .true.
     !end if
     send_data_flag(i) = is_send_time(comp_time(my_comp_id)%before_sec, &
                                      comp_time(my_comp_id)%current_sec, &
                                      comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
                                      comp_time(i)%before_sec, &
                                      comp_time(i)%current_sec, &
                                      comp_time(i)%current_sec + comp_time(i)%current_delt, &
                                      recv_sec(i), send_sec(i))

     if (.not.jal_is_exchange(i)) send_data_flag(i) = .false.

  end do

  
  do i = 1, loop_length
    if (is_my_component(i)) cycle
    if (get_comp_exchange_type(my_comp_id, i) /= ASSYNC_SEND_RECV) cycle

    if (send_data_flag(i)) then
      call put_log("--------------------------------- send data start  ---------------------------------")
      write(log_str, *) "send = "//trim(comp_time(i)%comp_name)//",",&
                                     comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
      call put_log(trim(log_str))
      !call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
      send_time_array = time_array
      call jal_send_array_nowait(i, send_time_array)
      call jal_send_data(comp_time(i)%comp_name, jal_get_before_time(my_comp_id), &
                         jal_get_current_time(my_comp_id))
    end if

  end do

  do i = 1, num_of_total_comp


    if (is_my_component(i)) cycle

    if (get_comp_exchange_type(my_comp_id, i) /= ASSYNC_SEND_RECV) cycle

    do 
    recv_flag = is_recv_time(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
                             comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
                             comp_time(i)%before_sec, comp_time(i)%current_sec, &
                             comp_time(i)%current_sec + comp_time(i)%current_delt, &
                             recv_sec(i))

    if (.not.jal_is_exchange(i)) recv_flag = .false. 

    if (.not.recv_flag) exit 
    !num_of_recv(i) = cal_num_of_recv(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
    !                                 comp_time(i)%current_sec, comp_time(i)%current_sec + comp_time(i)%current_delt, &
    !                                 recv_sec(i))


    !do j = 1,  cal_num_of_recv(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
    !                           comp_time(i)%current_sec, comp_time(i)%current_sec + comp_time(i)%current_delt)
    !do j = 1,  num_of_recv(i)

       call put_log("--------------------------------- recv data start  ---------------------------------")
       write(log_str, *) "recv = "//trim(comp_time(i)%comp_name)//",", &
                                       comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
       call put_log(trim(log_str))
        
       !call jcup_recv_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
       call jal_recv_array(i, time_array)

       write(log_str, *) "recv_time = ", time_array
       call put_log(trim(log_str))

       recv_sec(i)  = time_array(1)
       
       comp_time(i)%before_delt = comp_time(i)%current_delt
       comp_time(i)%current_delt = time_array(2)
       comp_time(i)%nstep = comp_time(i)%nstep + 1
       comp_time(i)%before_sec  = comp_time(i)%current_sec
       comp_time(i)%current_sec = time_array(1)
       comp_time(i)%before_time = comp_time(i)%current_time
       comp_time(i)%current_time%ss = time_array(1)

       call jal_recv_data(comp_time(i)%comp_name, jal_get_before_time(i), &
                          jal_get_current_time(i))
       

       call jal_remove_past_recv_data(jal_get_before_time(i),my_comp_id, i) ! remove past recv data
       
    end do

  end do

  call jml_send_waitall()

  
  do i = 1, num_of_total_comp
     if (is_my_component(i)) cycle

     if (get_comp_exchange_type(my_comp_id, i) /= ASSYNC_SEND_RECV) cycle

     if (.not.jal_is_exchange(i)) cycle
     call put_log("-------------------------------- time intpl start  ---------------------------------")
     write(log_str, '(A,I5,I5,A,I5)') "source = "//trim(comp_time(i)%comp_name) &
                                       //",",comp_time(i)%before_sec, comp_time(i)%current_sec, &
                       ",  my time = ", comp_time(my_comp_id)%current_sec
     call put_log(trim(log_str))
     call jal_interpolate_time(comp_time(my_comp_id)%current_time, &
                               i, comp_time(i)%before_time, comp_time(i)%current_time)
  end do

  comp_time(my_comp_id)%before_sec = comp_time(my_comp_id)%current_sec 
  comp_time(my_comp_id)%current_sec = comp_time(my_comp_id)%current_sec + delta_t
  comp_time(my_comp_id)%before_time = comp_time(my_comp_id)%current_time
  comp_time(my_comp_id)%current_time%ss = comp_time(my_comp_id)%current_time%ss + delta_t

  is_first_step = .false.

  call put_log("------------------------------------------------------------------------------------")
  call put_log("------------------------------ jal_set_time finish ---------------------------------")
  call put_log("------------------------------------------------------------------------------------")

  return
  

end subroutine jal_set_time


!====================================================================================================

subroutine jal_time_end()
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model, &
                                           jcup_recv_array => recv_array_from_send_model
  use jcup_comp, only : is_my_component
  use jcup_utils, only : put_log
  use jal_exchange, only : jal_is_exchange, jal_send_data, jal_recv_data, jal_send_array_nowait
  use jcup_mpi_lib, only : jml_send_waitall
  use jal_mpi_lib, only : jal_set_window_data, jal_get_window_data, jal_free_window
  use jcup_config, only : get_comp_exchange_type
  use jcup_constant, only : ASSYNC_SEND_RECV
  implicit none
  real(kind=8) :: time_array(2)
  integer :: i, j
  integer :: data(1)
  character(len=STR_LONG) :: log_str
  
  write(log_str,*) "/////////////////////////////// jal_time_end start //////////////////////////////// ", &
             my_comp_id, step_counter
  call put_log(trim(log_str))

  data(1) = 1
  call jal_set_window_data(data, 1)
  
  do i = 1, num_of_total_comp

    data(1) = 0
      
    if (is_my_component(i)) cycle

    if (get_comp_exchange_type(my_comp_id, i) /= ASSYNC_SEND_RECV) cycle

    if (.not.jal_is_exchange(i)) cycle
    
    call jal_get_window_data(i-1, 0, data, 1)

    if (data(1) == 1) cycle
    
    !write(0, *) "final step time ", comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
    !     comp_time(i)%before_sec, comp_time(i)%current_sec
    
    if (is_send_time(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
                     comp_time(my_comp_id)%current_sec, &
                     comp_time(i)%before_sec, comp_time(i)%current_sec, comp_time(i)%current_sec, recv_sec(i), send_sec(i))) then

      write(log_str, *) "send final step data, ", trim(comp_time(my_comp_id)%comp_name)//","//trim(comp_time(i)%comp_name)//",",&
                                     comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec

      call put_log(trim(log_str))
      
      time_array(1) = comp_time(my_comp_id)%current_sec
      time_array(2) = comp_time(my_comp_id)%current_delt
      !call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
      call jal_send_array_nowait(i, time_array)
      call jal_send_data(comp_time(i)%comp_name, jal_get_before_time(my_comp_id), &
                         jal_get_current_time(my_comp_id))
      call jml_send_waitall()
   end if
  end do

 
  !!!!!call jal_free_window() ! comment out 20210108

  write(log_str,*) "/////////////////////////////// jal_time_end finish /////////////////////////////// "

  call put_log(trim(log_str))

end subroutine jal_time_end

!====================================================================================================

function is_send_time_org(my_before_sec, my_current_sec, my_next_sec,  &
                     target_before_sec, target_current_sec, target_next_sec, recv_sec, send_sec) result(res)
  use jcup_utils, only : put_log
  implicit none
  integer(kind=8), intent(IN) :: my_before_sec
  integer(kind=8), intent(IN) :: my_current_sec
  integer(kind=8), intent(IN) :: my_next_sec
  integer(kind=8), intent(IN) :: target_before_sec
  integer(kind=8), intent(IN) :: target_current_sec
  integer(kind=8), intent(IN) :: target_next_sec
  integer(kind=8), intent(IN) :: recv_sec
  integer(kind=8), intent(INOUT) :: send_sec
  logical :: res
  character(len=STR_LONG) :: log_str

  write(log_str, *) "is_send_time ", my_before_sec, my_current_sec, my_next_sec, &
           target_before_sec, target_current_sec, target_next_sec, recv_sec, send_sec

  call put_log(trim(log_str))

  if (send_sec < 0) then ! first step
     res = .true.
     send_sec = my_current_sec
     return
  end if

  if ((my_before_sec == target_current_sec).and.(my_next_sec == target_next_sec)) then
     res = .false.
     return
  end if
  
  if (send_sec < target_current_sec) then
     if (my_next_sec > target_current_sec) then
       res = .true.
       send_sec = my_current_sec
       return
    end if
 end if

 if (my_before_sec < target_current_sec) then
    if (my_current_sec > target_current_sec) then
       res = .true.
       send_sec = my_current_sec
       return
    end if
 end if
 
  !if (my_next_sec < recv_sec) then
  !   res = .false.
  !   return
  !end if

  if (my_next_sec < target_next_sec) then
     res = .false.
     return
  end if
  
  res = .true.
  send_sec = my_current_sec
  
end function is_send_time_org

!====================================================================================================

function is_send_time(my_before_sec, my_current_sec, my_next_sec,  &
                     target_before_sec, target_current_sec, target_next_sec, recv_sec, send_sec) result(res)
  use jcup_utils, only : put_log
  implicit none
  integer(kind=8), intent(IN) :: my_before_sec
  integer(kind=8), intent(IN) :: my_current_sec
  integer(kind=8), intent(IN) :: my_next_sec
  integer(kind=8), intent(IN) :: target_before_sec
  integer(kind=8), intent(IN) :: target_current_sec
  integer(kind=8), intent(IN) :: target_next_sec
  integer(kind=8), intent(IN) :: recv_sec
  integer(kind=8), intent(INOUT) :: send_sec
  logical :: res
  character(len=STR_LONG) :: log_str

  write(log_str, *) "is_send_time ", my_before_sec, my_current_sec, my_next_sec, &
           target_before_sec, target_current_sec, target_next_sec, recv_sec, send_sec

  call put_log(trim(log_str))

  if (send_sec < 0) then ! first step
     res = .true.
     send_sec = my_current_sec
     return
  end if

  !if ((my_before_sec >= target_current_sec).and.(my_next_sec <= target_next_sec)) then
  !   res = .false.
  !   return
  !end if

  !if ((my_before_sec >= target_before_sec).and.(my_next_sec <= target_current_sec)) then
  !   res = .false.
  !   return
  !end if

  if (my_before_sec == target_current_sec) then
     if (my_next_sec <= target_next_sec) then
        res = .false.
        return
     end if
  else
     if (my_next_sec <= target_current_sec) then
        res = .false.
        return
     end if
  end if
  
  res = .true.
  send_sec = my_current_sec
  
end function is_send_time

!====================================================================================================

logical function is_recv_time(my_before_sec, my_current_sec, my_next_sec,  &
                              target_before_sec, target_current_sec, target_next_sec, recv_sec)
  use jcup_utils, only : put_log
  implicit none
  integer(kind=8), intent(IN) :: my_before_sec
  integer(kind=8), intent(IN) :: my_current_sec
  integer(kind=8), intent(IN) :: my_next_sec
  integer(kind=8), intent(IN) :: target_before_sec
  integer(kind=8), intent(IN) :: target_current_sec
  integer(kind=8), intent(IN) :: target_next_sec
  integer(kind=8), intent(INOUT) :: recv_sec
  character(len=STR_LONG) :: log_str

  write(log_str, *) "is_recv_time ", my_before_sec, my_current_sec, my_next_sec, &
           target_before_sec, target_current_sec, target_next_sec, recv_sec
  call put_log(trim(log_str))

  is_recv_time = .false.

  if (recv_sec < my_current_sec) then
     is_recv_time = .true.
     recv_sec = target_next_sec
  end if
  
end function is_recv_time

!====================================================================================================

integer function cal_num_of_recv(my_before_sec, my_current_sec, target_current_sec, target_next_sec, recv_sec)
  use jcup_utils, only : put_log
  implicit none
  integer(kind=8), intent(IN) :: my_before_sec
  integer(kind=8), intent(IN) :: my_current_sec
  integer(kind=8), intent(IN) :: target_current_sec
  integer(kind=8), intent(IN) :: target_next_sec
  integer(kind=8), intent(INOUT) :: recv_sec
  character(len=STR_LONG) :: log_str
  
  write(log_str, *) "cal_num_of_recv ", my_before_sec, my_current_sec, target_current_sec, target_next_sec, recv_sec
  call put_log(trim(log_str))
  
  if (recv_sec == my_current_sec) then
     cal_num_of_recv = 0
     return
  end if
  
  if (my_current_sec < target_current_sec) then
    cal_num_of_recv = 0
    return
  end if

  if ((my_before_sec <= target_current_sec).and.(my_current_sec > target_next_sec)) then
    cal_num_of_recv = 2
    recv_sec = target_next_sec
    return
  end if

  cal_num_of_recv = 1
  recv_sec = target_next_sec
  
end function cal_num_of_recv

!====================================================================================================

logical function is_send_data(my_sec, my_delt, target_sec)
  implicit none
  integer(kind=8), intent(IN) :: my_sec
  integer(kind=4), intent(IN) :: my_delt
  integer(kind=8), intent(IN) :: target_sec

  
  is_send_data = ((my_sec <= target_sec).and.(my_sec + my_delt > target_sec))

end function is_send_data

!====================================================================================================

function jal_get_my_comp_id() result(res)
  implicit none
  integer :: res

  res = my_comp_id

end function jal_get_my_comp_id

!====================================================================================================

function jal_get_before_time(comp_id) result(res)
  implicit none
  integer, intent(IN) :: comp_id
  type(time_type), pointer :: res
  res => comp_time(comp_id)%before_time

end function jal_get_before_time

!====================================================================================================

function jal_get_current_time(comp_id) result(res)
  implicit none
  integer, intent(IN) :: comp_id
  type(time_type), pointer :: res
  res => comp_time(comp_id)%current_time

end function jal_get_current_time

!====================================================================================================

end module jal_time
