module common
  implicit none
  private

  integer, parameter, public :: NAME_LEN = 32
  integer, parameter, public :: STR_LEN = 128

  character(len=NAME_LEN), parameter, public :: COMP1_NAME = "comp1"
  character(len=NAME_LEN), parameter, public :: COMP1_GRID = "comp1_grid"
  character(len=NAME_LEN), parameter, public :: COMP2_NAME = "comp2"
  character(len=NAME_LEN), parameter, public :: COMP2_GRID = "comp2_grid"

  integer, parameter, public :: COMP1_NX = 180 ! 181 = 1
  integer, parameter, public :: COMP1_NY =  91 ! 1 = south pole, 91 = north pole
  integer, parameter, public :: COMP1_NZ =  10
  integer, parameter, public :: COMP2_NX = 120 ! 121 = 1
  integer, parameter, public :: COMP2_NY =  61 ! 1 = south pole, 61 = north pole
  integer, parameter, public :: COMP2_NZ = COMP1_NZ

  public :: set_grid_index ! subroutine (grind_index, NX, NY)
  public :: read_mapping_table ! subroutine (file_name, SNX, RNX, send_index, recv_index, coef)
  public :: set_test_data      ! subroutine ((data, nx, ny, nz)

contains

!=====================================================================================

subroutine set_grid_index(grid_index, NX, NY)
  implicit none
  integer, pointer, intent(INOUt) :: grid_index(:)
  integer, intent(IN) :: NX, NY
  integer :: i, j

  allocate(grid_index(NX*NY))

  do j = 1, NY
    do i = 1, NX
       grid_index(i + NX*(j-1)) = i + NX*(j-1)
    end do
  end do

end subroutine set_grid_index


!=====================================================================================

subroutine set_test_data(data, nx, ny, nz)
  implicit none
  real(kind=8), intent(INOUT) :: data(:,:) ! nx*ny, nz
  integer, intent(IN) :: nx, ny, nz
  real(kind=8), parameter :: PI = 3.141592653579d0
  integer :: i, j, k

  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
         data(i + nx*(j-1), k) = cos(2.d0*360.d0/nx*(i-1)/180.d0*PI)**2*abs(cos((-90.d0+180.d0/(ny-1)*(j-1))/180.d0*PI))
      end do
    end do
  end do

end subroutine set_test_data
 
!=====================================================================================

subroutine read_mapping_table(file_name, SNX, RNX, send_index, recv_index, coef)
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: SNX, RNX
  integer, pointer, intent(INOUT) :: send_index(:)
  integer, pointer, intent(INOUT) :: recv_index(:)
  real(kind=8), pointer, intent(INOUT) :: coef(:)
  integer, parameter :: FID = 225
  integer :: ri, rj, si, sj
  real(kind=8) ::  c
  integer :: counter

  open(unit=FID, file=trim(file_name))

  counter = 0
  do
    read(FID, *, end = 100) ri, rj, si, sj, c
    counter = counter + 1
  end do

  100 continue

  allocate(send_index(counter), recv_index(counter), coef(counter))

  rewind(FID)

  counter = 0
  do
    read(FID, *, end = 200) ri, rj, si, sj, c
    counter = counter + 1
    recv_index(counter) = ri + RNX*(rj-1)
    send_index(counter) = si + SNX*(sj-1)
    coef(counter) = c
  end do

  200 continue

  close(FID)
  
end subroutine read_mapping_table

!=====================================================================================
end module common
