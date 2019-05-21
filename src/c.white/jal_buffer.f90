!====================================================================================================

module jal_buffer
  use jcup_time_buffer, only : time_buffer_type
  use jcup_time, only : time_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: jal_init_buffer     ! subroutine ()
  public :: jal_finalize_buffer ! subroutine ()
  public :: jal_put_send_data ! subroutine (dt, time, component_id, data_id, name, is_mean, weight)
  public :: jal_get_send_data ! subroutine (dt, time, component_id, data_id, name)
  public :: jal_put_recv_data ! subroutine (dt, time, component_id, data_id, name)
  public :: jal_get_recv_data ! subroutine (dt, time, component_id, data_id, name)
  public :: jal_remove_past_send_data ! subroutine (time, component_id)
  public :: jal_remove_past_recv_data ! subrouitne (time, component_id)
  
!--------------------------------   private  ---------------------------------!

  interface jal_put_send_data
    module procedure jal_put_send_data_1d, jal_put_send_data_2d
  end interface

  interface jal_get_send_data
     module procedure jal_get_send_data_1d, jal_get_send_data_2d
  end interface jal_get_send_data

  interface jal_put_recv_data
     module procedure jal_put_recv_data_1d, jal_put_recv_data_2d
  end interface jal_put_recv_data

  interface jal_get_recv_data
     module procedure jal_get_recv_data_1d, jal_get_recv_data_2d
  end interface jal_get_recv_data
  

  type(time_buffer_type), save, pointer :: send_buffer
  type recv_buffer_type
     type(time_buffer_type), pointer :: recv_buffer_ptr
  end type recv_buffer_type
  type (recv_buffer_type), save, allocatable :: recv_buffer(:)

contains

!====================================================================================================

subroutine jal_init_buffer()
  use jcup_buffer_base, only : init_buffer_base
  use jcup_comp, only : get_num_of_total_component
  implicit none
  type(time_type) :: time
  integer :: i
  
  time%yyyy = 1000 ; time%mo = 1 ; time%dd = 1 ; time%hh = 1  ; time%mm = 0 ; time%ss = 0
  time%delta_t = 0.d0

  call init_buffer_base(send_buffer)
  allocate(recv_buffer(get_num_of_total_component()))
  do i = 1, size(recv_buffer)
     call init_buffer_base(recv_buffer(i)%recv_buffer_ptr)
  end do

end subroutine jal_init_buffer

!====================================================================================================

subroutine jal_finalize_buffer()
  use jcup_time_buffer, only : destruct_time_buffer
  implicit none
  integer :: i
  
  call destruct_time_buffer(send_buffer)
  do i = 1, size(recv_buffer)
    call destruct_time_buffer(recv_buffer(i)%recv_buffer_ptr)
  end do
  deallocate(recv_buffer)
end subroutine jal_finalize_buffer

!====================================================================================================

subroutine jal_put_send_data_1d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("put_send_data_double_1d : put data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))
  call put_data_base(send_buffer, dt, time, component_id, data_id, name, is_mean, weight)

end subroutine jal_put_send_data_1d

!====================================================================================================

subroutine jal_put_send_data_2d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("put_send_data_double_2d : put data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))
  call put_data_base(send_buffer, dt, time, component_id, data_id, name, is_mean, weight)

end subroutine jal_put_send_data_2d

!====================================================================================================

subroutine jal_get_send_data_1d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(OUT) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
       & "get_send_data_double_1d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))&
       //", time = "//trim(time_str))

  call get_data_base(send_buffer, dt, time, component_id, data_id, name, .false., .false.)

end subroutine jal_get_send_data_1d

!====================================================================================================

subroutine jal_get_send_data_2d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(OUT) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
       & "get_send_data_double_2d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))&
       //", time = "//trim(time_str))

  call get_data_base(send_buffer, dt, time, component_id, data_id, name, .false., .false.)

end subroutine jal_get_send_data_2d

!====================================================================================================

subroutine jal_get_send_data_3d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(OUT) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
       & "get_send_data_double_3d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))&
       //", time = "//trim(time_str))

  call get_data_base(send_buffer, dt, time, component_id, data_id, name, .false., .false.)

end subroutine jal_get_send_data_3d

!====================================================================================================

subroutine jal_put_recv_data_1d(dt, time, my_comp_id, target_comp_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: my_comp_id, target_comp_id, data_id
  character(len=*), intent(IN) :: name
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  !call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("put_recv_data_double_1d : put data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(my_comp_id))//", data id = "//trim(IntToStr(data_id)))
  call put_data_base(recv_buffer(target_comp_id)%recv_buffer_ptr, dt, time, &
                     my_comp_id, data_id, name, .false., 0.d0)

end subroutine jal_put_recv_data_1d

!====================================================================================================

subroutine jal_put_recv_data_2d(dt, time, my_comp_id, target_comp_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: my_comp_id, target_comp_id, data_id
  character(len=*), intent(IN) :: name
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  !call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("put_recv_data_double_2d : put data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(my_comp_id))//", data id = "//trim(IntToStr(data_id)))
  call put_data_base(recv_buffer(target_comp_id)%recv_buffer_ptr, dt, time, &
                     my_comp_id, data_id, name, .false., 0.d0)

end subroutine jal_put_recv_data_2d

!====================================================================================================

subroutine jal_put_recv_data_3d(dt, time, my_comp_id, target_comp_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: my_comp_id, target_comp_id, data_id
  character(len=*), intent(IN) :: name
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  !call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("put_send_data_double_3d : put data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(my_comp_id))//", data id = "//trim(IntToStr(data_id)))
  call put_data_base(recv_buffer(target_comp_id)%recv_buffer_ptr, dt, time, &
                     my_comp_id, data_id, name, .false., 0.d0)

end subroutine jal_put_recv_data_3d

!====================================================================================================

subroutine jal_get_recv_data_1d(dt, time, my_comp_id, target_comp_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: my_comp_id, target_comp_id, data_id
  character(len=*), intent(IN) :: name
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  !call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("get_recv_data_double_1d : get data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(my_comp_id))//", data id = "//trim(IntToStr(data_id)))
  call get_data_base(recv_buffer(target_comp_id)%recv_buffer_ptr, dt, time, &
                     my_comp_id, data_id, name, .false., .false.)

end subroutine jal_get_recv_data_1d

!====================================================================================================

subroutine jal_get_recv_data_2d(dt, time, my_comp_id, target_comp_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: my_comp_id, target_comp_id, data_id
  character(len=*), intent(IN) :: name
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  !call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("get_recv_data_double_2d : get data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(my_comp_id))//", data id = "//trim(IntToStr(data_id)))
  call get_data_base(recv_buffer(target_comp_id)%recv_buffer_ptr, dt, time, &
                     my_comp_id, data_id, name, .false., .false.)

end subroutine jal_get_recv_data_2d

!====================================================================================================

subroutine jal_get_recv_data_3d(dt, time, my_comp_id, target_comp_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: my_comp_id, target_comp_id, data_id
  character(len=*), intent(IN) :: name
  character(len=6) :: weight_str

  !write(weight_str, '(F)') weight
  !call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)), 1) !//", weight= "//trim(weight_str),1)
  call put_log("get_recv_data_double_2d : get data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(my_comp_id))//", data id = "//trim(IntToStr(data_id)))
  call get_data_base(recv_buffer(target_comp_id)%recv_buffer_ptr, dt, time, &
                     my_comp_id, data_id, name, .false., .false.)

end subroutine jal_get_recv_data_3d

!====================================================================================================

subroutine jal_remove_past_send_data(current_time, component_id)
  use jcup_buffer_base, only : remove_past_data
  use jcup_time, only : time_type
  implicit none
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: component_id

  call remove_past_data(send_buffer, current_time, component_id)
  
end subroutine jal_remove_past_send_data

!====================================================================================================

subroutine jal_remove_past_recv_data(current_time, my_comp_id, target_comp_id)
  use jcup_buffer_base, only : remove_past_data
  use jcup_time, only : time_type
  implicit none
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: my_comp_id, target_comp_id

  call remove_past_data(recv_buffer(target_comp_id)%recv_buffer_ptr, current_time, my_comp_id)
  
end subroutine jal_remove_past_recv_data

!====================================================================================================

end module jal_buffer
