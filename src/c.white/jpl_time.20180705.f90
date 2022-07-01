!====================================================================================================

module jpl_time
  use jcup_constant, only : NAME_LEN, STRING_LEN
  use jcup_time, only : time_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!
  
  public :: jpl_time_init
  public :: jpl_set_time
  public :: jpl_time_end
  public :: jpl_get_my_comp_id
  public :: jpl_get_before_time
  public :: jpl_get_current_time

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
    character(len=NAME_LEN) :: comp_name
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
  
contains

!====================================================================================================

subroutine jpl_time_init()
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name
  use jpl_mpi_lib, only : jpl_init_window, jpl_set_window_data, jpl_get_window_data
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

  
  call jpl_init_window(my_comp_id, 1)

  allocate(data(1))
  data(1) = 0
  call jpl_set_window_data(data, 1)

  do i = 1, num_of_total_comp
     if (.not.is_my_component(i)) then
       call jpl_get_window_data(i-1, 0, data, 1)
     end if
  end do

  deallocate(data)
  
  
end subroutine jpl_time_init

!====================================================================================================

subroutine jpl_set_time_org(delta_t)
  use jcup_mpi_lib, only : jcup_get_myrank_global => jml_GetMyrankGlobal
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_array => recv_array_from_send_model
  use jcup_comp, only : is_my_component
  use jcup_utils, only : put_log
  use jpl_exchange, only : jpl_init_exchange, jpl_send_data, jpl_recv_data, jpl_time_interpolation, jpl_is_exchange
  implicit none
  integer(kind=4), intent(IN) :: delta_t
  integer :: i, j
  real(kind=8) :: time_array(2)
  integer :: loop_length
  logical :: recv_flag
  character(len=STRING_LEN) :: log_str

  if (is_first_step) call jpl_init_exchange(my_comp_id)
  
  step_counter = step_counter + 1
  
  call put_log("------------------------------------------------------------------------------------")
  call put_log("--------------------------------- jpl_set_time  ------------------------------------")
  call put_log("------------------------------------------------------------------------------------")
  comp_time(my_comp_id)%before_delt = comp_time(my_comp_id)%current_delt
  comp_time(my_comp_id)%current_delt = delta_t
  comp_time(my_comp_id)%nstep = comp_time(my_comp_id)%nstep + 1

  time_array(1) = comp_time(my_comp_id)%current_sec
  time_array(2) = delta_t

  write(log_str, *) "current time = ", time_array(1), ", delta t = ", time_array(2)
  call put_log(trim(log_str))

  send_data_flag(:) = .false.

  num_of_recv(:) = 0

  if (is_first_step) then
     loop_length = my_comp_id-1
  else
     loop_length = num_of_total_comp
  end if
  
  do i = 1, num_of_total_comp
     if (is_my_component(i)) cycle
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

     if (.not.jpl_is_exchange(i)) send_data_flag(i) = .false.

  end do

  
  do i = 1, loop_length
    if (is_my_component(i)) cycle
    if (send_data_flag(i)) then
      call put_log("--------------------------------- send data start  ---------------------------------")
      write(log_str, *) "send = "//trim(comp_time(i)%comp_name)//",",&
                                     comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
      call put_log(trim(log_str))
      call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
      call jpl_send_data(comp_time(i)%comp_name, jpl_get_before_time(my_comp_id), &
                         jpl_get_current_time(my_comp_id))
    end if

  end do

  do i = 1, num_of_total_comp


    if (is_my_component(i)) cycle

    do 
    recv_flag = is_recv_time(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
                             comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
                             comp_time(i)%before_sec, comp_time(i)%current_sec, &
                             comp_time(i)%current_sec + comp_time(i)%current_delt, &
                             recv_sec(i))

    if (.not.jpl_is_exchange(i)) recv_flag = .false. 

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
        
       call jcup_recv_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)

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

       call jpl_recv_data(comp_time(i)%comp_name, jpl_get_before_time(i), &
                          jpl_get_current_time(i))
       
    !end do
    end do

  end do

  if (is_first_step) then
    time_array(1) = comp_time(my_comp_id)%current_sec
    time_array(2) = delta_t

  
    do i = my_comp_id + 1, num_of_total_comp
      if (is_my_component(i)) cycle
      if (send_data_flag(i)) then
        call put_log("--------------------------------- send data start  ---------------------------------")
        write(log_str, *) "send = "//trim(comp_time(i)%comp_name)//",",&
                                       comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
        call put_log(trim(log_str))
        call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
        call jpl_send_data(comp_time(i)%comp_name, jpl_get_before_time(my_comp_id), &
                           jpl_get_current_time(my_comp_id))
     end if
    end do
  end if
 

  do i = 1, num_of_total_comp
     if (is_my_component(i)) cycle
     if (.not.jpl_is_exchange(i)) cycle
     call put_log("-------------------------------- time intpl start  ---------------------------------")
     write(log_str, '(A,I5,I5,A,I5)') "source = "//trim(comp_time(i)%comp_name) &
                                       //",",comp_time(i)%before_sec, comp_time(i)%current_sec, &
                       ",  my time = ", comp_time(my_comp_id)%current_sec
     call put_log(trim(log_str))
     call jpl_time_interpolation(comp_time(my_comp_id)%current_time, &
                                 i, comp_time(i)%before_time, comp_time(i)%current_time)
  end do

  comp_time(my_comp_id)%before_sec = comp_time(my_comp_id)%current_sec 
  comp_time(my_comp_id)%current_sec = comp_time(my_comp_id)%current_sec + delta_t
  comp_time(my_comp_id)%before_time = comp_time(my_comp_id)%current_time
  comp_time(my_comp_id)%current_time%ss = comp_time(my_comp_id)%current_time%ss + delta_t

  is_first_step = .false.

  call put_log("------------------------------------------------------------------------------------")
  call put_log("------------------------------ jpl_set_time finish ---------------------------------")
  call put_log("------------------------------------------------------------------------------------")

  return
  

end subroutine jpl_set_time_org

!====================================================================================================

subroutine jpl_set_time(delta_t)
  use jcup_mpi_lib, only : jcup_get_myrank_global => jml_GetMyrankGlobal
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_array => recv_array_from_send_model
  use jcup_comp, only : is_my_component
  use jcup_utils, only : put_log
  use jpl_exchange, only : jpl_init_exchange, jpl_send_data, jpl_recv_data, jpl_time_interpolation, jpl_is_exchange
  implicit none
  integer(kind=4), intent(IN) :: delta_t
  integer :: i, j
  real(kind=8) :: time_array(2)
  integer :: loop_length
  logical :: recv_flag
  character(len=STRING_LEN) :: log_str
  integer :: my_send_recv(2)
  integer :: target_send_recv(2)
  
  if (is_first_step) call jpl_init_exchange(my_comp_id)
  
  step_counter = step_counter + 1
  
  call put_log("------------------------------------------------------------------------------------")
  call put_log("--------------------------------- jpl_set_time  ------------------------------------")
  call put_log("------------------------------------------------------------------------------------")
  comp_time(my_comp_id)%before_delt = comp_time(my_comp_id)%current_delt
  comp_time(my_comp_id)%current_delt = delta_t
  comp_time(my_comp_id)%nstep = comp_time(my_comp_id)%nstep + 1

  time_array(1) = comp_time(my_comp_id)%current_sec
  time_array(2) = delta_t

  write(log_str, *) "current time = ", time_array(1), ", delta t = ", time_array(2)
  call put_log(trim(log_str))


  if (is_first_step) then
     loop_length = my_comp_id-1
  else
     loop_length = num_of_total_comp
  end if
  
  do i = 1, num_of_total_comp
     if (is_my_component(i)) cycle

     send_data_flag(:) = .false.
     recv_data_flag(:) = .false.
     num_of_recv(:) = 0

     send_data_flag(i) = is_send_time(comp_time(my_comp_id)%before_sec, &
                                      comp_time(my_comp_id)%current_sec, &
                                      comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
                                      comp_time(i)%before_sec, &
                                      comp_time(i)%current_sec, &
                                      comp_time(i)%current_sec + comp_time(i)%current_delt, &
                                      recv_sec(i), send_sec(i))
     if (is_first_step) send_data_flag(i) = .true.
     if (.not.jpl_is_exchange(i)) send_data_flag(i) = .false.

     recv_data_flag(i) = is_recv_time(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
                             comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
                             comp_time(i)%before_sec, comp_time(i)%current_sec, &
                             comp_time(i)%current_sec + comp_time(i)%current_delt, &
                             recv_sec(i))
     if (is_first_step) recv_data_flag(i) = .true.
     if (.not.jpl_is_exchange(i)) recv_data_flag(i) = .false. 

  end do

  do i = 1, num_of_total_comp
     
     if ((.not.send_data_flag(i)).and.(.not.recv_data_flag(i))) cycle  ! no send recv

     write(log_str, *) "my_send_recv_flag = ", send_data_flag(i), recv_data_flag(i)
     call put_log(trim(log_str))

     call set_sr_status(send_data_flag(i), recv_data_flag(i), comp_time(my_comp_id)%sr_status)

     call exchange_send_recv_info(comp_time(my_comp_id)%sr_status, comp_time(i)%sr_status)

     if ((comp_time(my_comp_id)%sr_status == STATUS_SR).and.(comp_time(i)%sr_status == STATUS_SR)) then ! send recv
         if (my_comp_id < i) then
            call send_to_target()
            comp_time(my_comp_id)%sr_status = STATUS_RECV
            call exchange_send_recv_info(comp_time(my_comp_id)%sr_status, comp_time(i)%sr_status)
            call recv_from_target()
         else
            call recv_from_target()
            comp_time(my_comp_id)%sr_status = STATUS_SEND
            call exchange_send_recv_info(comp_time(my_comp_id)%sr_status, comp_time(i)%sr_status)
           call send_to_target()
         end if
        comp_time(my_comp_id)%sr_status = STATUS_NOSR ! send recv finish
        comp_time(i)%sr_status = STATUS_NOSR
     end if

      if (comp_time(i)%sr_status == STATUS_RR) then
         call send_to_target()
         call exchange_send_recv_info(comp_time(my_comp_id)%sr_status, comp_time(i)%sr_status)
         call recv_from_target()
         comp_time(my_comp_id)%sr_status = STATUS_NOSR
      end if
      
      if (recv_data_flag(i)) then
        if (is_next_recv()) then
          comp_time(my_comp_id)%sr_status = STATUS_RR
          call exchange_send_recv_info(comp_time(my_comp_id)%sr_status, comp_time(i)%sr_status)
          call recv_from_target()
          comp_time(my_comp_id)%sr_status = STATUS_NOSR ! send recv finish at this time step
        end if
      end if

  end do

  !do i = 1, num_of_total_comp



  !  recv_flag = is_recv_time(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
  !                           comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
  !                           comp_time(i)%before_sec, comp_time(i)%current_sec, &
  !                           comp_time(i)%current_sec + comp_time(i)%current_delt, &
  !                           recv_sec(i))

  !  if (.not.jpl_is_exchange(i)) recv_flag = .false. 

   ! if (.not.recv_flag) exit 

   !    call put_log("--------------------------------- recv data start  ---------------------------------")
   !    write(log_str, *) "recv = "//trim(comp_time(i)%comp_name)//",", &
   !                                    comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
   !    call put_log(trim(log_str))
        
    !   call jcup_recv_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)

    !   write(log_str, *) "recv_time = ", time_array
    !   call put_log(trim(log_str))

    !   recv_sec(i)  = time_array(1)
       
    !   comp_time(i)%before_delt = comp_time(i)%current_delt
    !   comp_time(i)%current_delt = time_array(2)
    !   comp_time(i)%nstep = comp_time(i)%nstep + 1
    !   comp_time(i)%before_sec  = comp_time(i)%current_sec
    !   comp_time(i)%current_sec = time_array(1)
    !   comp_time(i)%before_time = comp_time(i)%current_time
    !   comp_time(i)%current_time%ss = time_array(1)

    !   call jpl_recv_data(comp_time(i)%comp_name, jpl_get_before_time(i), &
    !                      jpl_get_current_time(i))
       


  do i = 1, num_of_total_comp
     if (is_my_component(i)) cycle
     if (.not.jpl_is_exchange(i)) cycle
     call put_log("-------------------------------- time intpl start  ---------------------------------")
     write(log_str, '(A,I5,I5,A,I5)') "source = "//trim(comp_time(i)%comp_name) &
                                       //",",comp_time(i)%before_sec, comp_time(i)%current_sec, &
                       ",  my time = ", comp_time(my_comp_id)%current_sec
     call put_log(trim(log_str))
     call jpl_time_interpolation(comp_time(my_comp_id)%current_time, &
                                 i, comp_time(i)%before_time, comp_time(i)%current_time)
  end do


  comp_time(my_comp_id)%before_sec = comp_time(my_comp_id)%current_sec 
  comp_time(my_comp_id)%current_sec = comp_time(my_comp_id)%current_sec + delta_t
  comp_time(my_comp_id)%before_time = comp_time(my_comp_id)%current_time
  comp_time(my_comp_id)%current_time%ss = comp_time(my_comp_id)%current_time%ss + delta_t

  is_first_step = .false.

  call put_log("------------------------------------------------------------------------------------")
  call put_log("------------------------------ jpl_set_time finish ---------------------------------")
  call put_log("------------------------------------------------------------------------------------")

  return
  
contains

  subroutine set_sr_status(send_flag, recv_flag, sr)
    implicit none
    logical, intent(IN) :: send_flag, recv_flag
    integer, intent(OUT) :: sr

    if ((send_flag).and.(recv_flag)) then
       sr = STATUS_SR
       return
    end if
    if ((send_flag).and.(.not.recv_flag)) then
       sr = STATUS_SEND
       return
    end if
    if ((.not.send_flag).and.(recv_flag)) then
       sr = STATUS_RECV
       return
    end if
    sr = STATUS_NOSR
    
  end subroutine set_sr_status
  
  subroutine exchange_send_recv_info(my_status, target_status)
    integer, intent(IN) :: my_status
    integer, intent(INOUT) :: target_status
    integer :: send_buffer(1), recv_buffer(1)

    send_buffer(1) = my_status
  
     call put_log("--------------------------------- exchange info  ---------------------------------")
     if (my_comp_id < i) then
        call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, send_buffer)
        call jcup_recv_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, recv_buffer)
     else
        call jcup_recv_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, recv_buffer)
        call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, send_buffer)
     end if
     write(log_str, *) "target = "//trim(comp_time(i)%comp_name)//",",&
                                     recv_buffer(1)
     call put_log(trim(log_str))
     target_status = recv_buffer(1)
   end subroutine exchange_send_recv_info
   
  subroutine send_to_target()
          call put_log("--------------------------------- send data start  ---------------------------------")
          write(log_str, *) "send = "//trim(comp_time(i)%comp_name)//",",&
                                     comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
          call put_log(trim(log_str))
          time_array(1) = comp_time(my_comp_id)%current_sec
          time_array(2) = delta_t
          call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
          call jpl_send_data(comp_time(i)%comp_name, jpl_get_before_time(my_comp_id), &
                             jpl_get_current_time(my_comp_id))

  end subroutine send_to_target        

  subroutine recv_from_target()
          call put_log("--------------------------------- recv data start  ---------------------------------")
          write(log_str, *) "recv = "//trim(comp_time(i)%comp_name)//",", &
                                           comp_time(my_comp_id)%current_sec, comp_time(i)%current_sec
          call put_log(trim(log_str))
        
          call jcup_recv_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)

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

          call jpl_recv_data(comp_time(i)%comp_name, jpl_get_before_time(i), &
                             jpl_get_current_time(i))

        end subroutine recv_from_target

    logical function is_next_recv()
      call put_log("--------------------------------- is_next_recv  ---------------------------------")
      is_next_recv = is_recv_time(comp_time(my_comp_id)%before_sec, comp_time(my_comp_id)%current_sec, &
                             comp_time(my_comp_id)%current_sec + comp_time(my_comp_id)%current_delt, &
                             comp_time(i)%before_sec, comp_time(i)%current_sec, &
                             comp_time(i)%current_sec + comp_time(i)%current_delt, &
                             recv_sec(i))

      if (.not.jpl_is_exchange(i)) is_next_recv = .false. 
    end function is_next_recv

end subroutine jpl_set_time

!====================================================================================================

subroutine jpl_time_end()
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model, &
                                           jcup_recv_array => recv_array_from_send_model
  use jcup_comp, only : is_my_component
  use jcup_utils, only : put_log
  use jpl_exchange, only : jpl_is_exchange, jpl_send_data, jpl_recv_data
  use jpl_mpi_lib, only : jpl_set_window_data, jpl_get_window_data
  implicit none
  real(kind=8) :: time_array(2)
  integer :: i, j
  integer :: data(1)
  character(len=STRING_LEN) :: log_str
  
  write(log_str,*) "/////////////////////////////// jpl_time_end start //////////////////////////////// ", &
             my_comp_id, step_counter
  call put_log(trim(log_str))
  
  data(1) = 1
  call jpl_set_window_data(data, 1)
  
  do i = 1, num_of_total_comp

     data(1) = 0
     
    if (is_my_component(i)) cycle

    if (.not.jpl_is_exchange(i)) cycle
    
    call jpl_get_window_data(i-1, 0, data, 1)

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
      call jcup_send_array(comp_time(my_comp_id)%comp_name, comp_time(i)%comp_name, time_array)
      call jpl_send_data(comp_time(i)%comp_name, jpl_get_before_time(my_comp_id), &
                         jpl_get_current_time(my_comp_id))
    end if
  end do

  write(log_str,*) "/////////////////////////////// jpl_time_end finish /////////////////////////////// "

  call put_log(trim(log_str))

end subroutine jpl_time_end

!====================================================================================================

function is_send_time_org(my_before_sec, my_current_sec, my_next_sec, &
                                  target_current_sec, target_next_sec, send_sec) result(res)
  use jcup_utils, only : put_log
  implicit none
  integer(kind=8), intent(IN) :: my_before_sec
  integer(kind=8), intent(IN) :: my_current_sec
  integer(kind=8), intent(IN) :: my_next_sec
  integer(kind=8), intent(IN) :: target_current_sec
  integer(kind=8), intent(IN) :: target_next_sec
  integer(kind=8), intent(INOUT) :: send_sec
  logical :: res
  character(len=STRING_LEN) :: log_str

  write(log_str, *) "is_send_time ", my_before_sec, my_current_sec, my_next_sec, target_current_sec, target_next_sec, send_sec
  call put_log(trim(log_str))
  
  
  !if (send_sec == my_next_sec) then
  !   is_send_time = .false.
  !   return
  !end if
  
  if ((my_before_sec >= target_current_sec).and. &
      (my_next_sec   <= target_next_sec)) then
    res = .false.
  else
    res = .true.
    send_sec = my_current_sec
  end if

  if (my_next_sec <= target_current_sec) then
     res = .false.
  end if
  
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
  character(len=STRING_LEN) :: log_str

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
  character(len=STRING_LEN) :: log_str

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
  character(len=STRING_LEN) :: log_str
  
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

function jpl_get_my_comp_id() result(res)
  implicit none
  integer :: res

  res = my_comp_id

end function jpl_get_my_comp_id

!====================================================================================================

function jpl_get_before_time(comp_id) result(res)
  implicit none
  integer, intent(IN) :: comp_id
  type(time_type), pointer :: res
  res => comp_time(comp_id)%before_time

end function jpl_get_before_time

!====================================================================================================

function jpl_get_current_time(comp_id) result(res)
  implicit none
  integer, intent(IN) :: comp_id
  type(time_type), pointer :: res
  res => comp_time(comp_id)%current_time

end function jpl_get_current_time

!====================================================================================================

end module jpl_time
