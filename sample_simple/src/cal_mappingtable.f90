module mapping_table
  private

  public :: make_mapping_table

contains

!======================================================================================================
 
subroutine make_mapping_table(file_id, nx_r, ny_r, nx_s, ny_s)
  implicit none
  integer, intent(IN) :: file_id
  integer, intent(IN) :: nx_r, ny_r, nx_s, ny_s
  real(kind=8) :: dx_r, dy_r, dx_s, dy_s
  real(kind=8) :: xr, yr, xs1, ys1, xs3, ys3
  real(kind=8) :: coef(4)
  integer :: is, js
  integer :: i, j, k

  dx_r = 360.d0/nx_r
  dy_r = 180.d0/(ny_r-1)
  dx_s = 360.d0/nx_s
  dy_s = 180.d0/(ny_s-1)

  do j = 1, ny_r
    yr = dy_r*(j-1)
    js = int(yr/dy_s) + 1
    ys1 = (js-1)*dy_s
    ys3 = ys1 + dy_s

    do i = 1, nx_r

      xr = dx_r*(i-1)
      is = int(xr/dx_s) + 1
      xs1 = (is-1)*dx_s
      xs3 = xs1 + dx_s
      
      coef(:) = 0.d0
      call cal_coef(xr, yr, xs1, ys1, xs3, ys3, coef)
      !write(0, *) xr, yr, xs1, ys1, xs3, ys3, coef(1)

      if (coef(1) > 0.d0) write(file_id, *) i, j, is               , js               , coef(1)
      if (coef(2) > 0.d0) write(file_id, *) i, j, mod(is, nx_s) + 1, js               , coef(2)
      if (coef(3) > 0.d0) write(file_id, *) i, j, mod(is, nx_s) + 1, mod(js, ny_s) + 1, coef(3)
      if (coef(4) > 0.d0) write(file_id, *) i, j, is               , mod(js, ny_s) + 1, coef(4)
    end do
  end do

end subroutine make_mapping_table

!======================================================================================================
!   (x4, y4)          (x3, y3)
!         _______________
!        |              |
!        |        x     |
!        |     (xc, yc) |
!        |______________|_
!    (x1, y1)           (x2, y2)
subroutine cal_coef(xc, yc, x1, y1, x3, y3, coef)
  implicit none
  real(kind=8), intent(IN) :: xc, yc
  real(kind=8), intent(IN) :: x1, y1, x3, y3
  real(kind=8), intent(OUT) :: coef(:) ! coef(4)
  real(kind=8) :: alpha1, alpha2, beta1, beta2

  alpha1 = (xc-x1)/(x3-x1)
  alpha2 = 1.d0 - alpha1
  beta1  = (yc-y1)/(y3-y1)
  beta2  = 1.d0 - beta1

  coef(1) = alpha2*beta2
  coef(2) = alpha1*beta2
  coef(3) = alpha1*beta1
  coef(4) = alpha2*beta1

end subroutine cal_coef

!======================================================================================================

end module mapping_table


program cal_mappingtable
  use common
  use mapping_table
  integer, parameter :: FID = 128
  
  open(unit=FID, file="mapping_table_2_to_1.txt")
  call make_mapping_table(FID, COMP1_NX, COMP1_NY, COMP2_NX, COMP2_NY)
  close(FID)

  open(unit=FID, file="mapping_table_1_to_2.txt")
  call make_mapping_table(FID, COMP2_NX, COMP2_NY, COMP1_NX, COMP1_NY)
  close(FID)

end program cal_mappingtable
