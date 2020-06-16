!=======+=========+=========+=========+=========+=========+=========+=========+

!---------------------        module jcup_buffer        ----------------------!

!=======+=========+=========+=========+=========+=========+=========+=========+

module jcup_buffer
  use jcup_time_buffer, only : time_buffer_type
  use jcup_time, only : time_type
  use jcup_buffer_base, only : get_num_of_time

!--------------------------------   public  ----------------------------------!

  public :: init_buffer
  public :: destruct_buffer
  public :: get_send_data_type
  public :: put_send_data
  public :: get_send_data
  public :: remove_send_data
  public :: remove_send_time
  public :: remove_past_send_data
  public :: remove_past_recv_data
  public :: get_recv_data_type
  public :: put_recv_data
  public :: get_recv_data
  public :: remove_recv_data
  public :: remove_recv_time
  public :: buffer_check_write
  public :: get_num_of_time         ! integer function (time_buffer_ptr) 2013.06.07 [ADD]
  public :: get_send_buffer_ptr     ! function () result (send_buffer_ptr) 2013.06.07 [ADD]
  public :: dump_buffer             ! subroutine (fid)
  public :: restore_buffer          ! subroutine (fid)
  !public :: restore_buffer          ! subroutine (dt, time, component_id, data_id, name, data_type, data_dim) 2013.06.13 [ADD]

!--------------------------------   private  ---------------------------------!

  private

  interface put_send_data
    module procedure put_send_data_double_1d
    module procedure put_send_data_double_2d
    module procedure put_send_data_double_3d
  end interface

  interface get_send_data
    module procedure get_send_data_double_1d
    module procedure get_send_data_double_2d
    module procedure get_send_data_double_3d
  end interface

  interface put_recv_data
    module procedure put_recv_data_double_1d
    module procedure put_recv_data_double_2d
    module procedure put_recv_data_double_3d
  end interface

  interface get_recv_data
    module procedure get_recv_data_double_1d
    module procedure get_recv_data_double_2d
    module procedure get_recv_data_double_3d
  end interface

  type(time_buffer_type), pointer :: send_buffer
  type(time_buffer_type), pointer :: recv_buffer

  private :: put_send_data_double_2d
  private :: put_send_data_double_3d
  private :: get_send_data_double_2d
  private :: get_send_data_double_3d
  private :: put_recv_data_double_2d
  private :: put_recv_data_double_3d
  private :: get_recv_data_double_2d
  private :: get_recv_data_double_3d
  
contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_buffer()
  use jcup_buffer_base, only : init_buffer_base
  implicit none

  call init_buffer_base(send_buffer)
  call init_buffer_base(recv_buffer)

end subroutine init_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_buffer()
  use jcup_buffer_base, only : destruct_buffer_base
  implicit none

  call destruct_buffer_base(send_buffer, recv_buffer)
  
end subroutine destruct_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_data_type(time, data_id)
  use jcup_buffer_base, only : get_data_type
  implicit none
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: data_id

  get_send_data_type =  get_data_type(send_buffer, time, data_id)

end function get_send_data_type

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_recv_data_type(time, data_id)
  use jcup_buffer_base, only : get_data_type
  implicit none
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: data_id

  get_recv_data_type = get_data_type(recv_buffer, time, data_id)

end function get_recv_data_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_send_data_double_1d(dt, time, component_id, data_id, name, is_mean, weight)
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

end subroutine put_send_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_send_data_double_2d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)

  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call put_log("put_send_data_double_2d : put data : name = "//trim(name)//", comp id = " &
               //trim(IntToStr(component_id))//", data id = "//trim(IntToStr(data_id)))
  call put_data_base(send_buffer, dt, time, component_id, data_id, name, is_mean, weight)

end subroutine put_send_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_send_data_double_3d(dt, time, component_id, data_id, name, is_mean, weight)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  logical, intent(IN) :: is_mean
  real(kind=8), intent(IN) :: weight ! weight for data average (delta_t/interval)

  call put_log("Put data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call put_log("put_send_data_double_3d : put data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))

  call put_data_base(send_buffer, dt, time, component_id, data_id, name, is_mean, weight)

end subroutine put_send_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_data_double_1d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
       & "get_send_data_double_1d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))&
          //", time = "//trim(time_str))
  call get_data_base(send_buffer, dt, time, component_id, data_id, name, .false., .false.)

end subroutine get_send_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_data_double_2d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
       & "get_send_data_double_1d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))&
         //", time = "//trim(time_str))
  call get_data_base(send_buffer, dt, time, component_id, data_id, name, .false., .false.)

end subroutine get_send_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_data_double_3d(dt, time, component_id, data_id, name)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : DateToTimeStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  character(len=20) :: time_str

  call DateToTimeStr(time_str, time)
  call put_log(&
       & "get_send_data_double_3d : get data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id))&
              //", time = "//trim(time_str))
  call get_data_base(send_buffer, dt, time, component_id, data_id, name, .false., .false.)

end subroutine get_send_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_recv_data_double_1d(dt, time, component_id, data_id, name)
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call put_data_base(recv_buffer, dt, time, component_id, data_id, name, .false., 1.d0)

end subroutine put_recv_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_recv_data_double_2d(dt, time, component_id, data_id, name)
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call put_data_base(recv_buffer, dt, time, component_id, data_id, name, .false., 1.d0)

end subroutine put_recv_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_recv_data_double_3d(dt, time, component_id, data_id, name)
  use jcup_buffer_base, only : put_data_base
  implicit none
  real(kind=8), intent(IN) :: dt(:,:,:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call put_data_base(recv_buffer, dt, time, component_id, data_id, name, .false., 1.d0)

end subroutine put_recv_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_data_double_1d(dt, time, component_id, data_id, name, is_reset_data)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:)
  integer, intent(IN) :: component_id, data_id
  type(time_type), intent(IN) :: time  
  character(len=*), intent(IN) :: name
  logical, optional, intent(IN) :: is_reset_data

  call put_log("Get data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call get_data_base(recv_buffer, dt, time, component_id, data_id, name, .true.,  is_reset_data)

end subroutine get_recv_data_double_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_data_double_2d(dt, time, component_id, data_id, name, is_reset_data)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:)
  integer, intent(IN) :: component_id, data_id
  type(time_type), intent(IN) :: time  
  character(len=*), intent(IN) :: name
  logical, optional, intent(IN) :: is_reset_data

  call put_log("Get data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call get_data_base(recv_buffer, dt, time, component_id, data_id, name, .true.,  is_reset_data)

end subroutine get_recv_data_double_2d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_data_double_3d(dt, time, component_id, data_id, name, is_reset_data)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : get_data_base
  implicit none
  real(kind=8), intent(INOUT) :: dt(:,:,:)
  integer, intent(IN) :: component_id, data_id
  type(time_type), intent(IN) :: time  
  character(len=*), intent(IN) :: name
  logical, optional, intent(IN) :: is_reset_data

  call put_log("Get data, data name = "//trim(name)//", data id = "//trim(IntToStr(data_id)),1)
  call get_data_base(recv_buffer, dt, time, component_id, data_id, name, .true.,  is_reset_data)

end subroutine get_recv_data_double_3d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_send_data(time, data_id)
  use jcup_buffer_base, only : remove_data
  use jcup_time_buffer, only : search_time_buffer, get_start_data_ptr
  implicit none 
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: data_id

  call remove_data(send_buffer, time, data_id)

end subroutine remove_send_data 
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_recv_data(time, component_id, data_id, name)
  use jcup_buffer_base, only : remove_data
  implicit none 
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name

  call remove_data(recv_buffer, time, data_id)

end subroutine remove_recv_data 
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_send_time(time, component_id)
  use jcup_buffer_base, only : remove_time
  implicit none
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id
 
  call remove_time(send_buffer, time, component_id)

end subroutine remove_send_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_recv_time(time, component_id)
  use jcup_buffer_base, only : remove_time
  implicit none
  type(time_type), intent(IN) :: time
  integer, intent(IN) :: component_id

  call remove_time(recv_buffer, time, component_id)

end subroutine remove_recv_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_past_send_data(current_time, component_id)
  use jcup_utils, only : put_log
  use jcup_buffer_base, only : remove_past_data
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: component_id

  call put_log("remove past send data")
  call remove_past_data(send_buffer, current_time, component_id)

end subroutine remove_past_send_data

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine remove_past_recv_data(current_time, component_id)
  use jcup_utils, only : put_log
  use jcup_buffer_base, only : remove_past_data
  type(time_type), intent(IN) :: current_time
  integer, intent(IN) :: component_id

  call put_log("remove past recv data")
  call remove_past_data(recv_buffer, current_time, component_id)

end subroutine remove_past_recv_data

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine buffer_check_write()
  use jcup_utils, only : put_log
  use jcup_buffer_base, only : check_write_buffer
  implicit none

  call put_log("Check send buffer")
  call check_write_buffer(send_buffer)

  call put_log("Check recv buffer")
  call check_write_buffer(recv_buffer)
  
end subroutine buffer_check_write

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_send_buffer_ptr() result (send_buffer_ptr)
  implicit none
  type(time_buffer_type), pointer :: send_buffer_ptr

  send_buffer_ptr => send_buffer

end function get_send_buffer_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine restore_buffer_org(dt, time, component_id, data_id, name, data_type, data_dim)
  use jcup_utils, only : put_log, IntToStr
  use jcup_buffer_base, only : restore_buffer_base_org
  implicit none
  real(kind=8), intent(IN) :: dt(:)
  type(time_type), intent(IN) :: time  
  integer, intent(IN) :: component_id, data_id
  character(len=*), intent(IN) :: name
  integer, intent(IN) :: data_type
  integer, intent(IN) :: data_dim

  call put_log("restore send data : name = "//trim(name)//", data id = "//trim(IntToStr(data_id)))
  call restore_buffer_base_org(send_buffer, dt, time, component_id, data_id, name, data_type, data_dim)

end subroutine restore_buffer_org

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2020/06/15
subroutine dump_buffer(fid)
  use jcup_buffer_base, only : dump_buffer_base
  implicit none
  integer, intent(IN) :: fid

  call dump_buffer_base(send_buffer, fid)
  call dump_buffer_base(recv_buffer, fid)
  
end subroutine dump_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2020/06/15
subroutine restore_buffer(fid)
  use jcup_utils, only : put_log
  use jcup_buffer_base, only : restore_buffer_base
  implicit none
  integer, intent(IN) :: fid

  call put_log("------------------------------  restore  buffer   ----------------------------------")
  call put_log("------------------------------   send  buffer     ----------------------------------")
  call restore_buffer_base(send_buffer, fid)
  call put_log("------------------------------   recv  buffer     ----------------------------------")
  call restore_buffer_base(recv_buffer, fid)
  
end subroutine restore_buffer

end module

!program test
!  use jcup_data_container
!  implicit none

  
!  call init_data_buffer()
!  call set_step(1)
!  call set_step(2)

!end program
