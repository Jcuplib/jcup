!=======+=========+=========+=========+=========+=========+=========+=========+
module jcup_zlib
  use Iso_C_Binding
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: compress_array            ! subroutine (in_array, in_size, out_array, out_size)
  public :: uncompress_array          ! subroutine (in_array, in_size, out_array, out_size)

!--------------------------------   private  ---------------------------------!

  integer, parameter :: Z_OK = 0
  
  interface

    integer function compress(out_array, out_size, in_array, in_size) bind(C)
      import
      integer(C_long), intent(INOUT) :: out_size  ! out array size (byte)
      character,       intent(INOUT) :: out_array(out_size)
      integer(C_long), value         :: in_size   ! in array size (byte)         
      real(C_double),  intent(INOUT) :: in_array(in_size)
    end function  compress

    integer function compress2(out_array, out_size, in_array, in_size, deflate) bind(C)
      import
      integer(C_long), intent(INOUT) :: out_size  ! out array size (byte)
      character,       intent(INOUT) :: out_array(out_size)
      integer(C_long), value         :: in_size   ! in array size (byte)         
      real(C_double),  intent(INOUT) :: in_array(in_size)
      integer(C_long), value         :: deflate
    end function  compress2

    integer function uncompress(out_array, out_size, in_array, in_size) bind(C)
      import
      integer(C_long), intent(INOUT) :: out_size   ! out array size (byte)
      real(C_double),  intent(INOUT) :: out_array(out_size)
      integer(C_long), value         :: in_size    ! in array size (byte)
      character,       intent(INOUT) :: in_array(in_size)
    end function uncompress

  end interface

  interface compress_array
     module procedure compress_array_double
  end interface compress_array

  interface uncompress_array
    module procedure uncompress_array_double
  end interface uncompress_array

contains  

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine compress_array_double(in_array, in_size, out_array, out_size) bind(C)
  use mpi
  implicit none
  integer, intent(IN)    :: in_size           ! in data array size (byte/8)       
  real(kind=8), intent(INOUT)  :: in_array(in_size)       ! in data array (double precision)
  integer, intent(INOUT) :: out_size          ! size of compressed array (byte)
  character,    intent(INOUT)  :: out_array(out_size)      ! compressed byte array
  integer(kind=C_long)   :: in_size_c
  integer(kind=C_long)   :: out_size_c
  integer :: ret, ierror
  
  in_size_c  = in_size*8 ! double array size to byte array size
  out_size_c = out_size  !
  
  
  ret = 9999 ! compress2(out_array, out_size_c, in_array, in_size_c, 1)

  if (ret == -999) then
     write(0, *) "jcup_zlib, compress_array_double, Remove commentout"
     call MPI_Abort(MPI_COMM_WORLD, ret, ierror)
     stop 9999
  end if

  if (ret /= Z_OK) then
     write(0, *) "compress error, ret = ,", ret, ", in_size = ", in_size, ", out_size = ", out_size
     call MPI_Abort(MPI_COMM_WORLD, ret, ierror)
     stop 9999
  end if
  
  out_size = out_size_c
  
end subroutine compress_array_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine uncompress_array_double(in_array, in_size, out_array, out_size) bind(C)
  use mpi
  implicit none
  integer, intent(IN)    :: in_size           ! size of compressed array (byte)
  character,    intent(INOUT) :: in_array(in_size)       ! compressed array (character)
  integer, intent(INOUT) :: out_size          ! size of uncompressed array 
  real(kind=8), intent(INOUT) :: out_array(out_size)      ! uncompressed array (double precision)
  integer(kind=C_long)   :: in_size_c
  integer(kind=C_long)   :: out_size_c
  integer :: ret, ierror

  in_size_c  = in_size 
  out_size_c = out_size*8

  ret = 9999! uncompress(out_array, out_size_c, in_array, in_size_c)

  if (ret == -999) then
     write(0, *) "jcup_zlib, uncompress_array_double, Remove commentout"
     call MPI_Abort(MPI_COMM_WORLD, ret, ierror)
     stop 9999
  end if

  if (ret /= Z_OK) then
     write(0, *) "uncompress error, ret = ,", ret, ", in_size = ", in_size, ", out_size = ", out_size
     call MPI_Abort(MPI_COMM_WORLD, ret, ierror)
     stop 9999
  end if

  out_size = int(out_size_c/8)   ! byte size to double size
  
end subroutine uncompress_array_double

end module jcup_zlib
