module jpl_exchange
!--------------------------------   public  ----------------------------------!

  public :: jpl_init_exchange        ! subroutine (my_comp)  
  public :: jpl_init_exchange_buffer ! subrouitne (grid_size)
  public :: jpl_is_exchange          ! logical function (target_comp_id)
  public :: jpl_is_send              ! logical function (target_comp_id)
  public :: jpl_is_recv              ! logical function (target_comp_id)
  public :: jpl_send_data  ! subroutine (my_comp, target_comp)
  public :: jpl_recv_data  ! subroutine (my_comp, target_comp)
  public :: jpl_time_interpolation ! subroutine(my_comp, my_time, target_comp, before_time, current_time)
  
!--------------------------------   private  ---------------------------------!

  real(kind=8), pointer :: buffer1(:,:), buffer2(:,:), buffer3(:,:)

  integer :: my_comp_id
  integer :: num_of_total_comp

  type exchange_info_type
     logical :: is_exchange = .false.
     integer :: num_of_send_data = 0
     integer :: num_of_recv_data = 0
  end type exchange_info_type

  type(exchange_info_type), pointer :: exchange_info(:)
  
contains

!====================================================================================================

  subroutine jpl_init_exchange(my_comp)
    use jcup_comp, only : get_comp_id_from_name, get_num_of_total_component, is_my_component
    use jcup_utils, only : put_log
    implicit none
    integer, intent(IN) :: my_comp
    integer :: i
    character(len=128) :: log_str
    
    my_comp_id = my_comp
    num_of_total_comp = get_num_of_total_component()

    allocate(exchange_info(num_of_total_comp))

    do i = 1, num_of_total_comp
       if (is_my_component(i)) cycle
       exchange_info(i)%num_of_send_data = jpl_get_num_of_send_data(i)
       exchange_info(i)%num_of_recv_data = jpl_get_num_of_recv_data(i)
       if ((exchange_info(i)%num_of_send_data == 0).and.(exchange_info(i)%num_of_recv_data == 0)) then
          exchange_info(i)%is_exchange = .false.
       else
          exchange_info(i)%is_exchange = .true.
       end if
       write(log_str, "(A,I5,I5,I5)") "jpl_init_exchange, exchange info ", &
            i, exchange_info(i)%num_of_send_data, exchange_info(i)%num_of_recv_data
       call put_log(trim(log_str))
    end do
    
      
  end subroutine jpl_init_exchange
  
!====================================================================================================

  function jpl_get_num_of_send_data(target_comp_id) result(res)
    use jcup_config, only : get_num_of_send_data, send_data_conf_type, get_send_data_conf_ptr_from_id
    use jcup_constant, only : DOUBLE_DATA
    implicit none
    integer, intent(IN) :: target_comp_id
    type(send_data_conf_type), pointer :: send_conf_ptr
    integer :: res
    integer :: i, j

    res = 0
    
    do i = 1, get_num_of_send_data(my_comp_id)
       send_conf_ptr => get_send_data_conf_ptr_from_id(my_comp_id, i)
       do j = 1, send_conf_ptr%num_of_my_recv_data
          if (send_conf_ptr%my_recv_conf(j)%model_id == target_comp_id) then
            res = res + 1
          end if
       end do
    end do
    
  end function jpl_get_num_of_send_data
  
!====================================================================================================

  function jpl_get_num_of_recv_data(target_comp_id) result(res)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_constant, only : DOUBLE_DATA
    implicit none
    integer, intent(IN) :: target_comp_id
    type(recv_data_conf_type), pointer :: recv_conf_ptr
    integer :: res
    integer :: i, j

    res = 0
    
    do i = 1, get_num_of_recv_data(my_comp_id)
       recv_conf_ptr => get_recv_data_conf_ptr_from_id(my_comp_id, i)
       if (recv_conf_ptr%send_model_id == target_comp_id) then
          res = res + 1
       end if
    end do
    
  end function jpl_get_num_of_recv_data
  
!====================================================================================================

  logical function jpl_is_exchange(target_comp_id)
    implicit none
    integer, intent(IN) :: target_comp_id

    jpl_is_exchange = exchange_info(target_comp_id)%is_exchange

  end function jpl_is_exchange
  
!====================================================================================================

  logical function jpl_is_send(target_comp_id)
    implicit none
    integer, intent(IN) :: target_comp_id

    jpl_is_send = (exchange_info(target_comp_id)%num_of_send_data > 0)

  end function jpl_is_send
  
!====================================================================================================

  logical function jpl_is_recv(target_comp_id)
    implicit none
    integer, intent(IN) :: target_comp_id

    jpl_is_recv = (exchange_info(target_comp_id)%num_of_recv_data > 0)

  end function jpl_is_recv
  
!====================================================================================================

  subroutine jpl_init_exchange_buffer(grid_size1, grid_size2)
    implicit none
    integer, intent(IN) :: grid_size1
    integer, optional, intent(IN) :: grid_size2

    if (present(grid_size2)) then
      allocate(buffer1(grid_size1, grid_size2))
      allocate(buffer2(grid_size1, grid_size2))
      allocate(buffer3(grid_size1, grid_size2))
    else
      allocate(buffer1(grid_size1, 20))
      allocate(buffer2(grid_size1, 20))
      allocate(buffer3(grid_size1, 20))
   end if

  end subroutine jpl_init_exchange_buffer
  
!====================================================================================================

  subroutine jpl_send_data(target_comp, before_time, current_time)
    use jcup_config, only : get_num_of_send_data, send_data_conf_type, get_send_data_conf_ptr_from_id
    use jcup_comp, only : get_comp_id_from_name
    use jpl_buffer, only : jpl_get_send_data
    use jcup_constant, only : DOUBLE_DATA
    use jcup_time, only : time_type
    use jcup_grid, only : set_data, send_data_1d
    implicit none
    character(len=*), intent(IN) :: target_comp
    type(time_type), pointer :: before_time, current_time
    type(send_data_conf_type), pointer :: send_conf_ptr
    integer :: target_comp_id
    integer :: i, j

    target_comp_id = get_comp_id_from_name(target_comp)

    if (.not.jpl_is_send(target_comp_id)) return
    
    !!!write(0, *) "jpl_send_data start", get_num_of_send_data(my_comp), trim(my_comp)
    
    do i = 1, get_num_of_send_data(my_comp_id)
       send_conf_ptr => get_send_data_conf_ptr_from_id(my_comp_id, i)
       call jpl_get_send_data(buffer1(:,:), before_time, my_comp_id, send_conf_ptr%data_id, send_conf_ptr%name)
       call set_data(buffer1)
       do j = 1, send_conf_ptr%num_of_my_recv_data
          if (send_conf_ptr%my_recv_conf(j)%model_id == target_comp_id) then
            call send_data_1d(my_comp_id, target_comp_id, 1, DOUBLE_DATA, 1, send_conf_ptr%my_recv_conf(j)%data_id)
          end if
       end do
    end do
    
    !!!write(0, *) "jpl_send_data finish", get_num_of_send_data(my_comp), trim(my_comp)

  end subroutine jpl_send_data
  
!====================================================================================================

  subroutine jpl_recv_data(target_comp, before_time, current_time)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_comp, only : get_comp_id_from_name
    use jpl_buffer, only : jpl_put_recv_data
    use jcup_constant, only : DOUBLE_DATA
    use jcup_time, only : time_type
    use jcup_grid, only : recv_data, interpolate_data_1d, get_data_double_1d
    use jcup_utils, only : put_log
    implicit none
    character(len=*), intent(IN) :: target_comp
    type(time_type), pointer :: before_time, current_time
    type(recv_data_conf_type), pointer :: recv_conf_ptr
    integer :: target_comp_id
    integer :: exchange_tag(8)
    integer :: i
    character(len=255) :: log_str
    
    target_comp_id = get_comp_id_from_name(trim(target_comp))

    if (.not.jpl_is_recv(target_comp_id)) return

    do i = 1, get_num_of_recv_data(my_comp_id)
       recv_conf_ptr => get_recv_data_conf_ptr_from_id(my_comp_id, i)
       
       if (target_comp_id /= recv_conf_ptr%send_model_id) cycle

       call recv_data(my_comp_id, target_comp_id, 1, DOUBLE_DATA, 1, recv_conf_ptr%data_id)
       exchange_tag(:) = recv_conf_ptr%exchange_tag
       call interpolate_data_1d(my_comp_id, target_comp_id,  recv_conf_ptr%mapping_tag, DOUBLE_DATA, &
            recv_conf_ptr%num_of_data, exchange_tag)
       call get_data_double_1d(my_comp_id, target_comp_id, recv_conf_ptr%mapping_tag, buffer1, recv_conf_ptr%num_of_data)
       call jpl_put_recv_data(buffer1(:,:), current_time, my_comp_id, recv_conf_ptr%data_id, recv_conf_ptr%name)
       write(log_str,*) "jpl_recv_data ", buffer1(1,1)
       call put_log(trim(log_str))
       
       !call set_data(buffer1)
    end do
    
    !!!write(0, *) "jpl_recv_data finish", get_num_of_recv_data(my_comp), trim(my_comp)

  end subroutine jpl_recv_data
  
!====================================================================================================

  subroutine jpl_time_interpolation(my_time, target_comp_id, before_time, current_time)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_comp, only : get_comp_id_from_name
    use jcup_time, only : time_type
    use jpl_buffer, only : jpl_get_recv_data, jpl_put_recv_data
    implicit none
    type(time_type), intent(IN)  :: my_time
    integer, intent(IN) :: target_comp_id
    type(time_type), intent(IN)  :: before_time, current_time
    type(recv_data_conf_type), pointer :: recv_conf_ptr
    integer :: sec1, sec2, sec3
    integer :: i
    
    do i = 1, get_num_of_recv_data(my_comp_id)
       recv_conf_ptr => get_recv_data_conf_ptr_from_id(my_comp_id, i)
       if (recv_conf_ptr%send_model_id /= target_comp_id) cycle

       call jpl_get_recv_data(buffer1, before_time, my_comp_id, recv_conf_ptr%data_id, recv_conf_ptr%name)

       call jpl_get_recv_data(buffer2, current_time, my_comp_id, recv_conf_ptr%data_id, recv_conf_ptr%name)
       
       call time_interpolation_linear(buffer1, before_time%ss, buffer2, current_time%ss, buffer3, my_time%ss)

       call jpl_put_recv_data(buffer3, my_time,  my_comp_id, recv_conf_ptr%data_id, recv_conf_ptr%name)
    end do

  end subroutine jpl_time_interpolation
  
!====================================================================================================

!====================================================================================================

subroutine time_interpolation_linear(data1, sec1, data2, sec2, data3, sec3)
  use jcup_utils, only : put_log
  implicit none
  real(kind=8), pointer :: data1(:,:)
  integer(kind=8), intent(IN) :: sec1
  real(kind=8), pointer :: data2(:,:)
  integer(kind=8), intent(IN) :: sec2
  real(kind=8), pointer :: data3(:,:)
  integer(kind=8), intent(IN) :: sec3
  integer :: ni, nj
  integer :: i, j
  character(len=256) :: log_str

  write(log_str, *) "time_interpolation_linear ", sec1, data1(1,1), sec2, data2(1,1), sec3
  call put_log(trim(log_str))

  if (sec1 == sec2) then ! initial data
     data3 = data1
  else
    ni = size(data1, 1)
    nj = size(data1, 2)

    do j = 1, nj
       do i = 1, ni
          data3(i,j) = (data2(i,j)*(sec3-sec1)+data1(i,j)*(sec2-sec3))/(sec2-sec1)
       end do
    end do
 end if
 
end subroutine time_interpolation_linear

end module jpl_exchange

!====================================================================================================

