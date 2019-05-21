module jal_exchange
  use jcup_constant, only : DATA_1D, DATA_25D
  private
  
!--------------------------------   public  ----------------------------------!

  public :: jal_init_exchange        ! subroutine (my_comp)  
  public :: jal_init_exchange_buffer ! subrouitne (grid_size)
  public :: jal_is_exchange          ! logical function (target_comp_id)
  public :: jal_is_send              ! logical function (target_comp_id)
  public :: jal_is_recv              ! logical function (target_comp_id)
  public :: jal_send_array_nowait    ! subroutine (target_comp_id, array)
  public :: jal_recv_array           ! subroutine (target_comp_id, array)
  public :: jal_send_data  ! subroutine (my_comp, target_comp)
  public :: jal_recv_data  ! subroutine (my_comp, target_comp)
  public :: jal_interpolate_time ! subroutine(my_comp, my_time, target_comp, before_time, current_time)
  
!--------------------------------   private  ---------------------------------!

  real(kind=8), pointer :: buffer1(:,:), buffer2(:,:), buffer3(:,:)

  integer :: my_comp_id
  integer :: num_of_total_comp

  type cluster_info_type
     integer :: data_tag
     integer :: data_type ! DATA_1D or DATA_25D
     integer :: num_of_data
     integer, pointer :: exchange_tag(:)
     integer, pointer :: data_id(:)
     character(len=32), pointer :: data_name(:)
  end type cluster_info_type
  
  type exchange_info_type
     logical :: is_exchange = .false.
     integer :: num_of_send_data = 0
     integer :: num_of_recv_data = 0
     integer :: num_of_send_cluster
     type(cluster_info_type), pointer :: send_info(:)
     integer :: num_of_recv_cluster
     type(cluster_info_type), pointer :: recv_info(:)
     integer :: num_of_time_cluster
     type(cluster_info_type), pointer :: time_info(:)
     
     real(kind=8), pointer :: send_buffer(:,:)
  end type exchange_info_type

  type(exchange_info_type), pointer :: exchange_info(:)

  
contains

!====================================================================================================

  subroutine jal_init_exchange(my_comp)
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
       exchange_info(i)%num_of_send_data = jal_get_num_of_send_data(i)
       exchange_info(i)%num_of_recv_data = jal_get_num_of_recv_data(i)
       if ((exchange_info(i)%num_of_send_data == 0).and.(exchange_info(i)%num_of_recv_data == 0)) then
          exchange_info(i)%is_exchange = .false.
       else
          exchange_info(i)%is_exchange = .true.
       end if

       call jal_set_send_cluster(i, exchange_info(i))
       call jal_set_recv_cluster(i, exchange_info(i))
       call jal_set_time_cluster(i, exchange_info(i))
       write(log_str, "(A,I5,I5,I5,I5,I5, I5)") "jal_init_exchange, exchange info ", &
            i, exchange_info(i)%num_of_send_data, exchange_info(i)%num_of_send_cluster, &
            exchange_info(i)%num_of_recv_data, exchange_info(i)%num_of_recv_cluster, &
                                               exchange_info(i)%num_of_time_cluster
       call put_log(trim(log_str))
    end do
    
    
  end subroutine jal_init_exchange
  
!====================================================================================================

  function jal_get_num_of_send_data(target_comp_id) result(res)
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
    
  end function jal_get_num_of_send_data
  
!====================================================================================================

  function jal_get_num_of_recv_data(target_comp_id) result(res)
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
    
  end function jal_get_num_of_recv_data
  
!====================================================================================================

  subroutine jal_set_send_cluster(target_comp_id, exchange_info)
    use jcup_config, only : get_num_of_send_data, send_data_conf_type, get_send_data_conf_ptr_from_id
    use jcup_grid, only : get_send_data_buffer_size
    implicit none
    integer, intent(IN) :: target_comp_id
    type(exchange_info_type), intent(INOUT) :: exchange_info
    type(send_data_conf_type), pointer :: send_conf_ptr
    integer, pointer :: data_tag(:)
    integer, pointer :: exchange_tag(:)
    integer, pointer :: send_data_id(:)
    integer, pointer :: recv_data_id(:)
    character(len=32), pointer :: send_data_name(:)
    integer, pointer :: num_of_layer(:)
    integer, pointer :: tag_temp(:)
    integer, pointer :: data_id(:)
    integer :: i, j
    integer :: counter
    
    allocate(data_tag(get_num_of_send_data(my_comp_id)))

    allocate(exchange_tag(size(data_tag)))
    allocate(send_data_id(size(data_tag)))
    allocate(recv_data_id(size(data_tag)))
    allocate(send_data_name(size(data_tag)))
    
    allocate(num_of_layer(get_num_of_send_data(my_comp_id)))
    allocate(tag_temp(size(data_tag)))
    allocate(data_id(size(data_tag)))
    
    data_tag(:) = 0
    num_of_layer(:) = 0
    
    do i = 1, get_num_of_send_data(my_comp_id)
       send_conf_ptr => get_send_data_conf_ptr_from_id(my_comp_id, i)
       do j = 1, send_conf_ptr%num_of_my_recv_data
          if (send_conf_ptr%my_recv_conf(j)%model_id == target_comp_id) then
            data_tag(i) = send_conf_ptr%my_recv_conf(j)%exchange_tag
            num_of_layer(i) = send_conf_ptr%my_recv_conf(j)%num_of_data
            send_data_id(i) = send_conf_ptr%data_id
            recv_data_id(i) = send_conf_ptr%my_recv_conf(j)%data_id
            exchange_tag(i) = send_conf_ptr%my_recv_conf(j)%exchange_tag
            send_data_name(i) = send_conf_ptr%name
         end if
       end do
    end do

    tag_temp = data_tag
    counter = 0
    do i = 1, size(tag_temp)
       if (tag_temp(i) == 0) cycle
       counter = counter + 1
       data_id(counter) = tag_temp(i)
       do j = i + 1, size(tag_temp)
         if (tag_temp(j) == 0) cycle
         if (tag_temp(j) == tag_temp(i)) tag_temp(j) = 0
      end do
      tag_temp(i) = 0
   end do

   exchange_info%num_of_send_cluster = counter

   allocate(exchange_info%send_info(counter))

   do i = 1, exchange_info%num_of_send_cluster
      exchange_info%send_info(i)%data_tag = data_id(i)
      exchange_info%send_info(i)%num_of_data = 0
      
      call jal_set_cluster_info(exchange_info%send_info(i), data_tag, num_of_layer, &
                                exchange_tag, send_data_id, send_data_name)

   end do

    allocate(exchange_info%send_buffer(get_send_data_buffer_size(my_comp_id, target_comp_id), exchange_info%num_of_send_cluster))

    deallocate(data_tag)

    deallocate(exchange_tag)
    deallocate(send_data_id)
    deallocate(recv_data_id)
    deallocate(send_data_name)
    
    deallocate(num_of_layer)
    deallocate(tag_temp)
    deallocate(data_id)
    
    
  end subroutine jal_set_send_cluster
  
!====================================================================================================

  subroutine jal_set_recv_cluster(target_comp_id, exchange_info)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_grid, only : get_send_data_buffer_size
    implicit none
    integer, intent(IN) :: target_comp_id
    type(exchange_info_type), intent(INOUT) :: exchange_info
    type(recv_data_conf_type), pointer :: recv_conf_ptr
    integer, pointer :: data_tag(:)
    integer, pointer :: exchange_tag(:)
    integer, pointer :: send_data_id(:)
    integer, pointer :: recv_data_id(:)
    character(len=32), pointer :: recv_data_name(:)
    integer, pointer :: num_of_layer(:)
    integer, pointer :: tag_temp(:)
    integer, pointer :: data_id(:)
    integer :: i, j
    integer :: counter
    
    allocate(data_tag(get_num_of_recv_data(my_comp_id)))

    allocate(exchange_tag(size(data_tag)))
    allocate(send_data_id(size(data_tag)))
    allocate(recv_data_id(size(data_tag)))
    allocate(recv_data_name(size(data_tag)))
    
    allocate(num_of_layer(get_num_of_recv_data(my_comp_id)))
    allocate(tag_temp(size(data_tag)))
    allocate(data_id(size(data_tag)))
    
    data_tag(:) = 0
    data_id(:) = 0
    num_of_layer(:) = 0
    
    do i = 1, get_num_of_recv_data(my_comp_id)
       recv_conf_ptr => get_recv_data_conf_ptr_from_id(my_comp_id, i)
       if (recv_conf_ptr%send_model_id == target_comp_id) then
            data_tag(i) = recv_conf_ptr%exchange_tag
            num_of_layer(i) = recv_conf_ptr%num_of_data
            recv_data_id(i) = recv_conf_ptr%data_id
            exchange_tag(i) = recv_conf_ptr%exchange_tag
            recv_data_name(i) = recv_conf_ptr%name
       end if
    end do

    tag_temp = data_tag
    counter = 0
    do i = 1, size(tag_temp)
       if (tag_temp(i) == 0) cycle
       counter = counter + 1
       data_id(counter) = tag_temp(i)
       do j = i + 1, size(tag_temp)
         if (tag_temp(j) == 0) cycle
         if (tag_temp(j) == tag_temp(i)) tag_temp(j) = 0
      end do
      tag_temp(i) = 0
   end do

   exchange_info%num_of_recv_cluster = counter

   allocate(exchange_info%recv_info(counter))

   do i = 1, exchange_info%num_of_recv_cluster
      exchange_info%recv_info(i)%data_tag = data_id(i)
      exchange_info%recv_info(i)%num_of_data = 0

      call jal_set_cluster_info(exchange_info%recv_info(i), data_tag, num_of_layer, &
                                exchange_tag, recv_data_id, recv_data_name)

   end do

    deallocate(data_tag)

    deallocate(exchange_tag)
    deallocate(send_data_id)
    deallocate(recv_data_id)
    deallocate(recv_data_name)
    
    deallocate(num_of_layer)
    deallocate(tag_temp)
    deallocate(data_id)
    
    
  end subroutine jal_set_recv_cluster

!====================================================================================================

  subroutine jal_set_time_cluster(target_comp_id, exchange_info)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_grid, only : get_send_data_buffer_size
    implicit none
    integer, intent(IN) :: target_comp_id
    type(exchange_info_type), intent(INOUT) :: exchange_info
    type(recv_data_conf_type), pointer :: recv_conf_ptr
    integer, pointer :: data_tag(:)
    integer, pointer :: exchange_tag(:)
    integer, pointer :: send_data_id(:)
    integer, pointer :: recv_data_id(:)
    character(len=32), pointer :: recv_data_name(:)
    integer, pointer :: num_of_layer(:)
    integer, pointer :: tag_temp(:)
    integer, pointer :: data_id(:)
    integer :: i, j
    integer :: counter
    
    allocate(data_tag(get_num_of_recv_data(my_comp_id)))

    allocate(exchange_tag(size(data_tag)))
    allocate(send_data_id(size(data_tag)))
    allocate(recv_data_id(size(data_tag)))
    allocate(recv_data_name(size(data_tag)))
    
    allocate(num_of_layer(get_num_of_recv_data(my_comp_id)))
    allocate(tag_temp(size(data_tag)))
    allocate(data_id(size(data_tag)))
    
    data_tag(:) = 0
    data_id(:) = 0
    num_of_layer(:) = 0
    
    do i = 1, get_num_of_recv_data(my_comp_id)
       recv_conf_ptr => get_recv_data_conf_ptr_from_id(my_comp_id, i)
       if (recv_conf_ptr%send_model_id == target_comp_id) then
            data_tag(i) = recv_conf_ptr%time_intpl_tag
            num_of_layer(i) = recv_conf_ptr%num_of_data
            recv_data_id(i) = recv_conf_ptr%data_id
            exchange_tag(i) = recv_conf_ptr%time_intpl_tag
            recv_data_name(i) = recv_conf_ptr%name
       end if
    end do

    counter = 888
    do i = 1, size(data_tag)
       if (data_tag(i) < 0) then
          counter = counter + 1
          data_tag(i) = counter
       end if
    end do
    exchange_tag(:) = data_tag(:)

    tag_temp = data_tag
    counter = 0
    do i = 1, size(tag_temp)
       if (tag_temp(i) == 0) cycle
       counter = counter + 1
       data_id(counter) = tag_temp(i)
       do j = i + 1, size(tag_temp)
         if (tag_temp(j) == 0) cycle
         if (tag_temp(j) == tag_temp(i)) tag_temp(j) = 0
      end do
      tag_temp(i) = 0
   end do

   exchange_info%num_of_time_cluster = counter

   allocate(exchange_info%time_info(counter))

   do i = 1, exchange_info%num_of_time_cluster
      exchange_info%time_info(i)%data_tag = data_id(i)
      exchange_info%time_info(i)%num_of_data = 0

      call jal_set_cluster_info(exchange_info%time_info(i), data_tag, num_of_layer, &
                                exchange_tag, recv_data_id, recv_data_name)

   end do

    deallocate(data_tag)

    deallocate(exchange_tag)
    deallocate(send_data_id)
    deallocate(recv_data_id)
    deallocate(recv_data_name)
    
    deallocate(num_of_layer)
    deallocate(tag_temp)
    deallocate(data_id)
    
    
  end subroutine jal_set_time_cluster
  
!====================================================================================================

  subroutine jal_set_cluster_info(info, data_tag, num_of_layer, exchange_tag, data_id, data_name)
    implicit none
    type(cluster_info_type), intent(INOUT) :: info
    integer, intent(IN) :: data_tag(:)
    integer, intent(IN) :: num_of_layer(:)
    integer, intent(IN) :: exchange_tag(:)
    integer, intent(IN) :: data_id(:)
    character(len=32), intent(IN) :: data_name(:)
    integer :: j
    integer :: counter

    
      do j = 1, size(data_tag)
         if (data_tag(j) == 0) cycle
         if (info%data_tag == data_tag(j)) then
            info%num_of_data = info%num_of_data + 1
            info%data_type = DATA_1D
            if (num_of_layer(j) > 1) then
               info%num_of_data = num_of_layer(j)
               info%data_type = DATA_25D
            end if
         end if
      end do

      if (info%data_type == DATA_1D) then ! 1D data
        allocate(info%exchange_tag(info%num_of_data))
        allocate(info%data_id(info%num_of_data))
        allocate(info%data_name(info%num_of_data))
      else
        allocate(info%exchange_tag(1))
        allocate(info%data_id(1))
        allocate(info%data_name(1))
      end if

      counter = 0
      do j = 1, size(data_tag)
        if (data_tag(j) == 0) cycle
        if (info%data_tag == data_tag(j)) then
           counter = counter + 1
           info%exchange_tag(counter) = exchange_tag(j)
           info%data_id(counter) = data_id(j)
           info%data_name(counter) = data_name(j)
        end if
      end do
     

  end subroutine jal_set_cluster_info
  
!====================================================================================================

  logical function jal_is_exchange(target_comp_id)
    implicit none
    integer, intent(IN) :: target_comp_id

    jal_is_exchange = exchange_info(target_comp_id)%is_exchange

  end function jal_is_exchange
  
!====================================================================================================

  logical function jal_is_send(target_comp_id)
    implicit none
    integer, intent(IN) :: target_comp_id

    jal_is_send = (exchange_info(target_comp_id)%num_of_send_data > 0)

  end function jal_is_send
  
!====================================================================================================

  logical function jal_is_recv(target_comp_id)
    implicit none
    integer, intent(IN) :: target_comp_id

    jal_is_recv = (exchange_info(target_comp_id)%num_of_recv_data > 0)

  end function jal_is_recv
  
!====================================================================================================

  subroutine jal_init_exchange_buffer(grid_size1, grid_size2)
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

  end subroutine jal_init_exchange_buffer
  
!====================================================================================================

  subroutine jal_send_array_nowait(target_comp_id, array)
    use jcup_mpi_lib, only : jml_ISendModel, jml_isLocalLeader
    use jcup_utils, only : put_log
    implicit none
    integer, intent(IN) :: target_comp_id
    real(kind=8), target, intent(IN) :: array(:)
    real(kind=8), pointer :: data_ptr
    character(len=255) :: log_str

    if (jml_isLocalLeader(my_comp_id)) then
      data_ptr => array(1)
    
      call jml_ISendModel(my_comp_id, data_ptr, 1, size(array), target_comp_id, 0, 0)

      write(log_str,'(A,I5,I5)') "jal_send_array, send time = ", int(array(1)), int(array(2))
      call put_log(trim(log_str))
   end if
   
  end subroutine jal_send_array_nowait
  
!====================================================================================================
  subroutine jal_recv_array(target_comp_id, array)
    use jcup_mpi_lib, only : jml_IRecvModel, jml_recv_waitall, jml_isLocalLeader, jml_BcastLocal
    use jcup_utils, only : put_log
    integer, intent(IN) :: target_comp_id
    real(kind=8), target, intent(INOUT) :: array(:)
    real(kind=8), pointer :: data_ptr
    character(len=255) :: log_str
    
    if (jml_isLocalLeader(my_comp_id)) then
      data_ptr => array(1)
      call jml_IRecvModel(my_comp_id, data_ptr, 1, size(array), target_comp_id, 0, 0)
      call jml_recv_waitall()
   end if

    call jml_BcastLocal(my_comp_id, array, 1, size(array), 0)
    
    write(log_str,'(A,I5,I5)') "jal_recv_array, recv time = ", int(array(1)), int(array(2))
    call put_log(trim(log_str))
  end subroutine jal_recv_array
  
!====================================================================================================

  subroutine jal_send_data(target_comp, before_time, current_time)
    use jcup_config, only : get_num_of_send_data, send_data_conf_type, get_send_data_conf_ptr_from_id
    use jcup_comp, only : get_comp_id_from_name
    use jal_buffer, only : jal_get_send_data
    use jcup_constant, only : DOUBLE_DATA
    use jcup_time, only : time_type
    use jcup_grid, only : set_data, send_data_1d_nowait
    implicit none
    character(len=*), intent(IN) :: target_comp
    type(time_type), pointer :: before_time, current_time
    integer :: target_comp_id
    integer :: i, j

    target_comp_id = get_comp_id_from_name(target_comp)

    if (.not.jal_is_send(target_comp_id)) return
    
    do i = 1, exchange_info(target_comp_id)%num_of_send_cluster
       if (exchange_info(target_comp_id)%send_info(i)%data_type == DATA_25D) then
          call jal_get_send_data(buffer1(:,:), before_time, my_comp_id, exchange_info(target_comp_id)%send_info(i)%data_id(1), &
                 exchange_info(target_comp_id)%send_info(i)%data_name(1))
       else
         do j = 1, exchange_info(target_comp_id)%send_info(i)%num_of_data
            call jal_get_send_data(buffer1(:,j), before_time, my_comp_id, exchange_info(target_comp_id)%send_info(i)%data_id(j), &
                 exchange_info(target_comp_id)%send_info(i)%data_name(j))
         end do
       end if
       call set_data(buffer1)

       call send_data_1d_nowait(my_comp_id, target_comp_id, 1, DOUBLE_DATA, &
                                exchange_info(target_comp_id)%send_info(i)%num_of_data, &
                                exchange_info(target_comp_id)%send_info(i)%exchange_tag(1), &
                                exchange_info(target_comp_id)%send_buffer(:,i))
    end do
    
  end subroutine jal_send_data
  
!====================================================================================================

  subroutine get_send_data_from_buffer(target_comp_id, before_time)
    use jcup_config, only : get_num_of_send_data, get_send_data_conf_ptr_from_id, send_data_conf_type
    use jcup_time, only : time_type
    implicit none
    integer, intent(IN) :: target_comp_id
    type(time_type), intent(IN) :: before_time
    type(send_data_conf_type), pointer :: send_conf_ptr
    integer :: i
    
    do i = 1, get_num_of_send_data(my_comp_id)
       send_conf_ptr => get_send_data_conf_ptr_from_id(my_comp_id, i)
    end do
    
  end subroutine get_send_data_from_buffer
  
!====================================================================================================

  subroutine jal_recv_data(target_comp, before_time, current_time)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_comp, only : get_comp_id_from_name
    use jal_buffer, only : jal_put_recv_data
    use jcup_constant, only : DOUBLE_DATA
    use jcup_time, only : time_type
    use jcup_grid, only : recv_data, interpolate_data_1d, get_data_double_1d
    use jcup_utils, only : put_log
    implicit none
    character(len=*), intent(IN) :: target_comp
    type(time_type), pointer :: before_time, current_time
    type(cluster_info_type), pointer :: recv_conf_ptr
    integer :: target_comp_id
    integer :: exchange_tag(8)
    integer :: i, j
    character(len=255) :: log_str
    
    target_comp_id = get_comp_id_from_name(trim(target_comp))

    if (.not.jal_is_recv(target_comp_id)) return

    write(log_str, *) "jal_recv_data ", exchange_info(target_comp_id)%num_of_recv_cluster
    call put_log(log_str)
    
    do i = 1, exchange_info(target_comp_id)%num_of_recv_cluster
       recv_conf_ptr => exchange_info(target_comp_id)%recv_info(i)
       
       call recv_data(my_comp_id, target_comp_id, 1, DOUBLE_DATA, &
            recv_conf_ptr%num_of_data, &
            recv_conf_ptr%exchange_tag(1))
       exchange_tag(:) = recv_conf_ptr%exchange_tag(1)
       call interpolate_data_1d(my_comp_id, target_comp_id,  1, DOUBLE_DATA, &
            recv_conf_ptr%num_of_data, exchange_tag)
       call get_data_double_1d(my_comp_id, target_comp_id, 1, buffer1, recv_conf_ptr%num_of_data)

       if (recv_conf_ptr%data_type == DATA_1D) then
         do j = 1, recv_conf_ptr%num_of_data
            call jal_put_recv_data(buffer1(:,j), current_time, my_comp_id, target_comp_id, &
                                   recv_conf_ptr%data_id(j), recv_conf_ptr%data_name(j))
         end do
       else
          call jal_put_recv_data(buffer1(:,:), current_time, my_comp_id, target_comp_id, &
                                 recv_conf_ptr%data_id(1), recv_conf_ptr%data_name(1))
       end if 
       write(log_str,*) "jal_recv_data ", buffer1(1,1)
       call put_log(trim(log_str))
       
    end do
    
  end subroutine jal_recv_data
  
!====================================================================================================

  subroutine jal_interpolate_time(my_time, target_comp_id, before_time, current_time)
    use jcup_config, only : get_num_of_recv_data, recv_data_conf_type, get_recv_data_conf_ptr_from_id
    use jcup_comp, only : get_comp_id_from_name, get_component_name
    use jcup_time, only : time_type
    use jal_buffer, only : jal_get_recv_data, jal_put_recv_data
    use jal_time_interpolation, only : time_interpolation
    implicit none
    type(time_type), intent(IN)  :: my_time
    integer, intent(IN) :: target_comp_id
    type(time_type), intent(IN)  :: before_time, current_time
    type(cluster_info_type), pointer :: recv_conf_ptr
    integer :: i,j
    
    do i = 1, exchange_info(target_comp_id)%num_of_time_cluster
       recv_conf_ptr => exchange_info(target_comp_id)%time_info(i)
       if (recv_conf_ptr%data_type == DATA_1D) then
         do j = 1, recv_conf_ptr%num_of_data
            call jal_get_recv_data(buffer1(:,j), before_time, my_comp_id, target_comp_id, &
                                   recv_conf_ptr%data_id(j), recv_conf_ptr%data_name(j))
            call jal_get_recv_data(buffer2(:,j), current_time, my_comp_id, target_comp_id, &
                                   recv_conf_ptr%data_id(j), recv_conf_ptr%data_name(j))
         end do
       else       
          call jal_get_recv_data(buffer1(:,:), before_time, my_comp_id, target_comp_id, &
                                 recv_conf_ptr%data_id(1), recv_conf_ptr%data_name(1))
          call jal_get_recv_data(buffer2(:,:), current_time, my_comp_id, target_comp_id, &
                                 recv_conf_ptr%data_id(1), recv_conf_ptr%data_name(1))
       end if

       call time_interpolation(get_component_name(my_comp_id), my_time%ss, before_time%ss, current_time%ss, &
            size(buffer1,1), recv_conf_ptr%num_of_data, size(buffer3,2), &
            buffer1, buffer2, buffer3, recv_conf_ptr%exchange_tag(1))
       
       if (recv_conf_ptr%data_type == DATA_1D) then
         do j = 1, recv_conf_ptr%num_of_data
            call jal_put_recv_data(buffer3(:,j), my_time,  my_comp_id, target_comp_id, &
                                   recv_conf_ptr%data_id(j), recv_conf_ptr%data_name(j))
         end do
       else
          call jal_put_recv_data(buffer3(:,:), my_time,  my_comp_id, target_comp_id, &
                                 recv_conf_ptr%data_id(1), recv_conf_ptr%data_name(1))
       end if  
     end do
    
  end subroutine jal_interpolate_time
  
!====================================================================================================

end module jal_exchange

!====================================================================================================

