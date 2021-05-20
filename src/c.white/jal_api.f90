!====================================================================================================

module jal_api
  use jcup_constant, only : STR_SHORT
  use jal_time, only : jal_set_time
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: jal_init                    ! subroutine (comp_name)
  public :: jal_set_time                ! subroutine (delta_t)
  public :: jal_finish                  ! subroutine ()
  public :: jal_put_data                ! subroutine (varp, data)
  public :: jal_get_data                ! subroutine (varg, data)
  public :: jal_set_final_exchange_flag ! subroutine (exchange_flag)

!--------------------------------   private  ---------------------------------!

  interface jal_put_data
    module procedure jal_put_data_1d, jal_put_data_2d
  end interface

  interface jal_get_data
     module procedure jal_get_data_1d, jal_get_data_2d
  end interface jal_get_data
  
  character(len=STR_SHORT) :: my_name
  integer :: my_comp_id

  logical :: is_init_exchange  = .false.
  logical :: exchange_final_data = .true.

contains

!====================================================================================================

subroutine jal_init(comp_name)
  use jal_time, only : jal_time_init, jal_get_my_comp_id
  use jal_buffer, only : jal_init_buffer
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer :: log_level

  my_name = comp_name

  call jal_time_init()
  call jal_init_buffer()

  my_comp_id = jal_get_my_comp_id()

end subroutine jal_init

!====================================================================================================

subroutine jal_set_final_exchange_flag(flag)
  implicit none
  logical, intent(IN) :: flag

  exchange_final_data = flag

end subroutine jal_set_final_exchange_flag
 
!====================================================================================================

subroutine jal_finish()
  use jal_time, only : jal_time_end
  implicit none

  if (exchange_final_data) then
    call jal_time_end()
  end if

end subroutine jal_finish

!====================================================================================================

subroutine jal_put_data_1d(varp, dt)
  use jcup_utils, only : put_log
  use jcup_utils, only : put_log
  use jcup_data, only : varp_type
  use jal_buffer, only : jal_put_send_data 
  use jal_exchange, only : jal_init_exchange_buffer
  use jal_time, only : jal_get_before_time
  use jcup_time, only : time_type
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA
  implicit none
  type(varp_type), pointer :: varp
  real(kind=8), intent(IN) :: dt(:)
  character(len=STR_SHORT) :: data_name
  type(time_type), pointer :: time_ptr
  real(kind=8), pointer    :: weight(:)

  if (.not.is_init_exchange) then
     call jal_init_exchange_buffer(size(dt), NUM_OF_EXCHANGE_DATA)
     is_init_exchange = .true.
  end if
  
  call put_log("---------------------------------- put data start  ---------------------------------")

  data_name = varp%sd%name

  time_ptr => jal_get_before_time(my_comp_id)

  allocate(weight(size(dt)))
  weight(:) = 1.d0

  call jal_put_send_data(dt, time_ptr, my_comp_id, &
                         varp%sd%data_id, data_name, .false., 0.d0, weight)

  deallocate(weight)

end subroutine jal_put_data_1d

!====================================================================================================

subroutine jal_put_data_2d(varp, dt)
  use jcup_utils, only : put_log
  use jcup_data, only : varp_type
  use jcup_config, only : get_send_data_id
  use jal_buffer, only : jal_put_send_data 
  use jal_exchange, only : jal_init_exchange_buffer
  use jal_time, only : jal_get_before_time
  use jcup_time, only : time_type
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA
  implicit none
  type(varp_type), pointer :: varp
  real(kind=8), intent(IN) :: dt(:,:)
  character(len=STR_SHORT) :: data_name
  type(time_type), pointer :: time_ptr
  real(kind=8), pointer    :: weight(:,:)

  if (.not.is_init_exchange) then
     call jal_init_exchange_buffer(size(dt,1), NUM_OF_EXCHANGE_DATA)
     is_init_exchange = .true.
  end if

  call put_log("---------------------------------- put data start  ---------------------------------")

  data_name = varp%sd%name

  time_ptr => jal_get_before_time(my_comp_id)

  allocate(weight(size(dt,1), size(dt,2)))
  weight(:,:) = 1.d0

  call jal_put_send_data(dt, time_ptr, my_comp_id, &
                         varp%sd%data_id, data_name, .false., 0.d0, weight)

  deallocate(weight)

end subroutine jal_put_data_2d


!====================================================================================================

subroutine jal_get_data_1d(varg, dt)
  use jcup_utils, only : put_log
  use jcup_data, only : varg_type, get_data_name
  use jcup_config, only : get_send_data_id
  use jal_buffer, only : jal_get_recv_data 
  use jal_exchange, only : jal_init_exchange_buffer
  use jal_time, only : jal_get_before_time
  use jcup_time, only : time_type
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  type(varg_type), pointer :: varg
  real(kind=8), intent(INOUT) :: dt(:)
  character(len=STR_SHORT) :: data_name
  type(time_type), pointer :: time_ptr

  if (.not.is_init_exchange) then
     call jal_init_exchange_buffer(size(dt))
     is_init_exchange = .true.
  end if

  call put_log("---------------------------------- get data start  ---------------------------------")

  data_name = varg%rd%name

  time_ptr => jal_get_before_time(my_comp_id)

  call jal_get_recv_data(dt, time_ptr, my_comp_id, get_comp_id_from_name(trim(varg%send_model_name)), &
                         varg%rd%data_id, data_name)

end subroutine jal_get_data_1d

!====================================================================================================

subroutine jal_get_data_2d(varg, dt)
  use jcup_utils, only : put_log
  use jcup_data, only : varg_type, get_data_name
  use jcup_config, only : get_send_data_id
  use jal_buffer, only : jal_get_recv_data 
  use jal_exchange, only : jal_init_exchange_buffer
  use jal_time, only : jal_get_before_time
  use jcup_time, only : time_type
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  type(varg_type), pointer :: varg
  real(kind=8), intent(INOUT) :: dt(:,:)
  character(len=STR_SHORT) :: data_name
  type(time_type), pointer :: time_ptr

  if (.not.is_init_exchange) then
     call jal_init_exchange_buffer(size(dt))
     is_init_exchange = .true.
  end if
  
  call put_log("---------------------------------- get data start  ---------------------------------")

  data_name = varg%rd%name

  time_ptr => jal_get_before_time(my_comp_id)

  call jal_get_recv_data(dt, time_ptr, my_comp_id, get_comp_id_from_name(trim(varg%send_model_name)), &
                         varg%rd%data_id, data_name)

end subroutine jal_get_data_2d

!====================================================================================================

end module jal_api


