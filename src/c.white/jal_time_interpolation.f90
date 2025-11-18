module jal_time_interpolation
  private
  
  public :: time_interpolation

  interface time_interpolation
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
    end subroutine time_interpolation
  end interface time_interpolation
  
end module jal_time_interpolation
