module mod_time_interpolation
  private
  
!--------------------------------   public  ----------------------------------!

  public :: time_interpolation_linear
  
!--------------------------------   private  ---------------------------------!

contains

!====================================================================================================

subroutine time_interpolation_linear(ni, nj, sec1, data1, sec2, data2, sec3, data3)
  use jcup_utils, only : put_log
  implicit none
  integer, intent(IN) :: ni, nj
  integer(kind=8), intent(IN) :: sec1
  real(kind=8), intent(IN)    :: data1(:,:)
  integer(kind=8), intent(IN) :: sec2
  real(kind=8), intent(IN)    :: data2(:,:)
  integer(kind=8), intent(IN) :: sec3
  real(kind=8), intent(INOUT) :: data3(:,:)
  integer :: i, j
  character(len=256) :: log_str

  write(log_str, *) "time_interpolation_linear ", sec1, data1(1,1), sec2, data2(1,1), sec3
  call put_log(trim(log_str))
  
  if (sec1 == sec2) then ! initial data
     data3 = data1
  else
    do j = 1, nj
       do i = 1, ni
          data3(i,j) = (data2(i,j)*(sec3-sec1)+data1(i,j)*(sec2-sec3))/(sec2-sec1)
       end do
    end do
 end if

end subroutine time_interpolation_linear

end module mod_time_interpolation

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine time_interpolation( &
       model_name,      &
       ctime,           &
       time1,           &
       time2,           &
       num_grids,       &
       num_data_i,      &
       num_data_m,      &
       data1,           &
       data2,           &
       cdata,           &
       tag              &
       )
  use mod_time_interpolation, only : time_interpolation_linear
  implicit none

    character(len=*),   intent(in)    :: model_name
    integer(8),         intent(in)    :: ctime
    integer(8),         intent(in)    :: time1
    integer(8),         intent(in)    :: time2
    integer,            intent(in)    :: num_grids
    integer,            intent(in)    :: num_data_i
    integer,            intent(in)    :: num_data_m
    real(8),            intent(in)    :: data1(num_grids,num_data_i)
    real(8),            intent(in)    :: data2(num_grids,num_data_i)
    real(8),            intent(inout) :: cdata(num_grids,num_data_m)
    integer,            intent(in)    :: tag

    real(8) :: factor
    integer :: n, m

    select case(tag)
    case default
       call time_interpolation_linear(num_grids, num_data_i, &
                                      time1, data1, time2, data2, ctime, cdata)
    end select
    
end subroutine time_interpolation

