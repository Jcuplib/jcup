!=======+=========+=========+=========+=========+=========+=========+=========+

!m+ module palmtime
!m+   time mesurement module
!m
!m    Description:
!m
!m    Public constants:
!m
!m    Public variables:
!m
!m    Public types:
!m
!m    Public procedures:
!m
!m    History:
!m      01/04/1999 : ARAKAWA Takashi  : version 1.00
!m

!=======+=========+=========+=========+=========+=========+=========+=========+
 module palmtime
   include "mpif.h"
!--------------------------------   public  ----------------------------------!

  ! public procedures
  public palm_TimeInit
  public palm_TimeFinalize
  public palm_StartTime
  public palm_EndTime
  public palm_TimeOutput
  public palm_SetTimeFileCode
  public palm_SetTimeFileNum
  public palm_GetTimeFileNum
  public palm_SetCompData
  public :: palm_OutStatistics
  public :: palm_OutStatistics2

!--------------------------------   private  ---------------------------------!

  ! private constants
  integer,private,parameter :: STRING_LENGTH = 1024!128
  integer,private,parameter :: FILE_NUMBER   =  60 ! default file number

  ! private variables
  character,private:: FileName*(STRING_LENGTH)
  integer,private:: FileNum

  ! private types
  private timedatatype

  type timedatatype
    real(kind=8) :: mpitimes ! start time
    real(kind=8) :: mpitime  ! total time
    integer(kind=8) :: counter
    logical isTimeStart,isTimeEnd ! wether TimeStart/TimeEnd is called or not
    character*(STRING_LENGTH) :: subname !
    type(timedatatype),pointer :: NextPtr
  end type timedatatype

  type(timedatatype),private,pointer:: StartPtr,NowPtr

  ! private variables
  integer, private :: my_rank

  ! private procedures
  private palm_CalMaxTime
  private palm_isNewName
  private palm_SetNewPtr


  integer, private, parameter :: NUM_OF_COMP_DATA = 10000
  integer, private :: comp_data_counter

  type comp_data_type
    real(kind=8) :: mpitime
    integer :: lev, id
    integer :: is, ie, js, je, ks, ke
    integer :: num_of_data
    character(len=STRING_LENGTH) :: pos_name
  end type

  type(comp_data_type) :: comp_data(NUM_OF_COMP_DATA)

contains


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_TimeInit
!b+   initialize palmtime
!b
!b    Description:
!b
!b    Arguments:
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

subroutine palm_TimeInit()
  implicit none
  integer :: i
  integer :: ierror

  call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)

  allocate(StartPtr)
  StartPtr%subname = "total time"
  StartPtr%mpitime = 0
  StartPtr%counter = 1
  StartPtr%mpitimes = MPI_WTIME()
  StartPtr%isTimeStart = .TRUE.

  nullify(StartPtr%NextPtr)
  NowPtr => StartPtr

  FileName='TM'

  ! default FileNum=FILE_NUMBER+PE num
  ! If you want to change FileNum, Use palm_SetTimeFileNum
  FileNum =FILE_NUMBER+my_rank

  comp_data_counter = 1

  do i = 1, NUM_OF_COMP_DATA
    comp_data(i)%is = 0
    comp_data(i)%ie = 0
    comp_data(i)%js = 0
    comp_data(i)%je = 0
    comp_data(i)%ks = 0
    comp_data(i)%ke = 0
  end do

end subroutine palm_TimeInit


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_TimeFinalize
!b+   finalize palmtime
!b
!b    Description:
!b
!b    Arguments:
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

subroutine palm_TimeFinalize()

end subroutine palm_TimeFinalize


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_TimeStart
!b+   start time mesurement
!b
!b    Description:
!b
!b    Arguments:
!b      character*(*),intent(IN) :: Name
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

subroutine palm_TimeStart(Name)
  implicit none
  character*(*),intent(IN) :: Name

  if (palm_isNewName(Name)) then
    NowPtr%isTimeStart = .TRUE.
    NowPtr%isTimeEnd   = .FALSE.
  else
    NowPtr%isTimeStart = .TRUE.
    NowPtr%isTimeEnd   = .FALSE.
  end if

  NowPtr%mpitimes=MPI_WTIME()

end subroutine palm_TimeStart


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_TimeEnd
!b+   stop time mesurement
!b
!b    Description:
!b
!b    Arguments:
!b      character*(*),intent(IN) :: Name
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

subroutine palm_TimeEnd(Name)
  implicit none
  character*(*),intent(IN) :: Name

  if (palm_isNewName(Name)) then
    NowPtr%isTimeStart = .FALSE.
  else
    NowPtr%isTimeStart = .TRUE.
  end if

  NowPtr%isTimeEnd   = .TRUE.
  NowPtr%mpitime=NowPtr%mpitime+MPI_WTIME()-NowPtr%mpitimes
  NowPtr%counter = NowPtr%counter+1

end subroutine palm_TimeEnd


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_SetTimeFileCode
!b+   set time file name
!b
!b    Description:
!b
!b    Arguments:
!b      character*(*),intent(IN):: fc
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

 subroutine palm_SetTimeFileCode(fc)
  implicit none
  character*(*),intent(IN):: fc
  FileName=fc
end subroutine palm_SetTimeFileCode

!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_SetTimeFileNum
!b+   set time file number
!b
!b    Description:
!b
!b    Arguments:
!b      integer,intent(IN)::fn
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

subroutine palm_SetTimeFileNum(fn)
  implicit none
  integer,intent(IN)::fn
  FileNum=fn
end subroutine palm_SetTimeFileNum


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ function palm_GetTimeFileNum
!b+   get time file number
!b
!b    Description:
!b
!b    Arguments:
!b
!b    Return value:
!b      integer :: FileNum
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

integer function palm_GetTimeFileNum()
  palm_GetTimeFileNum = FileNum
end function palm_GetTimeFileNum


!=======+=========+=========+=========+=========+=========+=========+=========+

!b+ subroutine palm_TimeOutput
!b+   output result
!b
!b    Description:
!b
!b    Arguments:
!b
!b    History:
!b      01/04/1999 : ARAKAWA Takashi  : version 1.00
!b

subroutine palm_TimeOutput()
  implicit none
  character fn*(STRING_LENGTH)
  character fc*(STRING_LENGTH)
  real(8) maxtime
  integer i

  ! cat total time
  StartPtr%mpitime   = MPI_WTIME()-StartPtr%mpitimes
  StartPtr%isTimeEnd = .TRUE.

  ! cal Max time
  maxtime=palm_CalMaxTime()

  ! set file name
  write(fc,'(A3,I4.4)') '.pe',my_rank
  fn=trim(FileName)//trim(fc)

  open(FileNum,FILE=fn)

  NowPtr => StartPtr

  do while(associated(NowPtr))
    if ((NowPtr%isTimeStart).AND.(NowPtr%isTimeEnd)) then
      write(FileNum,'(A32,I8,F14.6,F10.2)')  &
      NowPtr%subname,NowPtr%counter,NowPtr%mpitime,NowPtr%mpitime/maxtime*100
    else
      if (.NOT.NowPtr%isTimeStart) then
        write(FileNum,'(A10,A)') &
             NowPtr%subname," palm_TimeStart is not called"
      else
        write(FileNum,'(A10,A)') &
             NowPtr%subname," palm_TimeEnd is not called"
      end if
    end if
    NowPtr => NowPtr%NextPtr
  end do

  close(FileNum)


  !!!call palm_DataOutput()

end subroutine palm_TimeOutput

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2017/01/19 [NEW]
subroutine palm_OutStatistics(comp_name, my_comm, my_rank)
  use mpi
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer, intent(IN) :: my_comm ! local communicator
  integer, intent(IN) :: my_rank ! local rank
  character(len=STRING_LENGTH) :: file_name
  integer, parameter :: FID = 99
  integer :: my_size
  integer :: ierror
  real(kind=8), allocatable :: recv_buffer(:)
  real(kind=8) :: send_buffer(1)
  character(len=13) :: cfmt  = "(A32,xxF10.6)"
  character(len=16) :: cfmt2 = "(A32,xxI10,3A10)" 
  integer :: i

  call mpi_comm_size(my_comm, my_size, ierror)


  write(file_name, "(A,A)") "TM."//trim(comp_name)

  NowPtr => StartPtr%NextPtr

  write(cfmt(6:7),"(I2)") my_size+3
  write(cfmt2(6:7), "(I2)") my_size

  if (my_rank == 0) then

    open(unit=FID, file=trim(file_name), iostat = ierror)

    if (ierror /= 0) then
       write(0, *) "file open error "//trim(file_name)
    end if

    allocate(recv_buffer(my_size))

    write(FID, cfmt2) "block_name", (i, i=1, my_size), "MAX", "MIN","MEAN"

    do while(associated(NowPtr))
      if ((NowPtr%isTimeStart).AND.(NowPtr%isTimeEnd)) then
        send_buffer(1) = NowPtr%mpitime
        call MPI_Gather(send_buffer, 1, MPI_REAL8, recv_buffer, 1, MPI_REAL8, 0, my_comm, ierror)
        write(FID, cfmt)  NowPtr%subname,(recv_buffer(i), i=1, my_size), &
                          maxval(recv_buffer), minval(recv_buffer), sum(recv_buffer)/my_size
      else
        if (.NOT.NowPtr%isTimeStart) then
          write(FileNum,'(A10,A)') &
               NowPtr%subname," palm_TimeStart is not called"
        else
          write(FileNum,'(A10,A)') &
               NowPtr%subname," palm_TimeEnd is not called"
        end if
      end if
      NowPtr => NowPtr%NextPtr
    end do

    deallocate(recv_buffer)

    close(FID)

  else
    allocate(recv_buffer(1))
    do while(associated(NowPtr))
      if ((NowPtr%isTimeStart).and.(NowPtr%isTimeEnd)) then
        send_buffer(1) = NowPtr%mpitime
        call MPI_Gather(send_buffer, 1, MPI_REAL8, recv_buffer, 1, MPI_REAL8, 0, my_comm, ierror)
       end if
      NowPtr => NowPtr%Nextptr
    end do
    deallocate(recv_buffer)
  end if
  
end subroutine palm_OutStatistics


!=======+=========+=========+=========+=========+=========+=========+=========+
! 2017/01/26 [NEW]
subroutine palm_OutStatistics2(comp_name, my_comm, my_rank)
  use mpi
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer, intent(IN) :: my_comm ! local communicator
  integer, intent(IN) :: my_rank ! local rank
  character(len=STRING_LENGTH) :: file_name
  integer, parameter :: FID = 99
  integer :: my_size
  integer :: ierror
  real(kind=8), allocatable :: recv_buffer(:)
  real(kind=8) :: send_buffer(1)
  character(len=12) :: cfmt  = "(A32,3F10.6)"
  character(len=10) :: cfmt2 = "(A32,3A10)" 
  integer :: i

  call mpi_comm_size(my_comm, my_size, ierror)


  write(file_name, "(A,A)") "TM."//trim(comp_name)

  NowPtr => StartPtr%NextPtr

  if (my_rank == 0) then

    open(unit=FID, file=trim(file_name), iostat = ierror)

    if (ierror /= 0) then
       write(0, *) "file open error "//trim(file_name)
    end if

    allocate(recv_buffer(my_size))

    write(FID, cfmt2) "block_name", "MAX", "MIN","MEAN"

    do while(associated(NowPtr))
      if ((NowPtr%isTimeStart).AND.(NowPtr%isTimeEnd)) then
        send_buffer(1) = NowPtr%mpitime
        call MPI_Gather(send_buffer, 1, MPI_REAL8, recv_buffer, 1, MPI_REAL8, 0, my_comm, ierror)
        write(FID, cfmt)  NowPtr%subname, maxval(recv_buffer), minval(recv_buffer), sum(recv_buffer)/my_size
      else
        if (.NOT.NowPtr%isTimeStart) then
          write(FileNum,'(A10,A)') &
               NowPtr%subname," palm_TimeStart is not called"
        else
          write(FileNum,'(A10,A)') &
               NowPtr%subname," palm_TimeEnd is not called"
        end if
      end if
      NowPtr => NowPtr%NextPtr
    end do

    deallocate(recv_buffer)

    close(FID)

  else
    allocate(recv_buffer(1))
    do while(associated(NowPtr))
      if ((NowPtr%isTimeStart).and.(NowPtr%isTimeEnd)) then
        send_buffer(1) = NowPtr%mpitime
        call MPI_Gather(send_buffer, 1, MPI_REAL8, recv_buffer, 1, MPI_REAL8, 0, my_comm, ierror)
       end if
      NowPtr => NowPtr%Nextptr
    end do
    deallocate(recv_buffer)
  end if
  
end subroutine palm_OutStatistics2


!=======+=========+=========+=========+=========+=========+=========+=========+

!r+ function palm_CalMaxTime
!r+   compute max time
!r
!r    Description:
!r
!r    Arguments:
!r      real(kind=8) :: MaxTime
!r
!r    Return value:
!r      real(kind=8) :: MaxTime
!r
!r    History:
!r      01/04/1999 : ARAKAWA Takashi  : version 1.00
!r

function palm_CalMaxTime() result(MaxTime)
  implicit none

  real(kind=8) :: MaxTime

  MaxTime = -9999

  NowPtr =>StartPtr

  do while(associated(NowPtr))

    if (MaxTime<=NowPtr%mpitime) then
      MaxTime = NowPtr%mpitime
    end if

    NowPtr => NowPtr%NextPtr

  end do

end function palm_CalMaxTime


!=======+=========+=========+=========+=========+=========+=========+=========+

!r+ function palm_isNewName
!r+   return new name or not
!r
!r    Description:
!r
!r    Arguments:
!r    character*(*),intent(IN) :: Name
!r
!r    Return value:
!r      logical :: isNewName
!r
!r    History:
!r      01/04/1999 : ARAKAWA Takashi  : version 1.00
!r

function palm_isNewName(Name) result(isNewName)
  implicit none
  character*(*),intent(IN) :: Name

  type(timedatatype),pointer :: tempptr
  logical :: isNewName

  isNewName = .FALSE.
  NowPtr => StartPtr

  do while(associated(NowPtr))
    if (NowPtr%subname==Name) then
      return
    end if

    tempptr => NowPtr
    NowPtr => NowPtr%NextPtr

  end do

  isNewName = .TRUE.
  NowPtr => tempptr
  call palm_SetNewPtr(Name)

end function palm_isNewName


!=======+=========+=========+=========+=========+=========+=========+=========+

!r+ subroutine palm_SetNewPtr
!r+   set new pointer
!r
!r    Description:
!r
!r    Arguments:
!r      character*(*),intent(IN) :: Name
!r
!r    History:
!r      01/04/1999 : ARAKAWA Takashi  : version 1.00
!r

subroutine palm_SetNewPtr(Name)
  implicit none
  character*(*),intent(IN) :: Name

  allocate(NowPtr%NextPtr)

  NowPtr => NowPtr%NextPtr

  NowPtr%subname  = Name
  NowPtr%mpitime  = 0
  NowPtr%mpitimes = 0
  NowPtr%counter  = 0
  NowPtr%isTimeStart = .FALSE.
  NowPtr%isTimeEnd   = .FALSE.

  nullify(NowPtr%NextPtr)

end subroutine palm_SetNewPtr

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine palm_SetCompData(data_name, lev, id, is, ie, js, je, ks, ke)
  implicit none
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: lev, id
  integer, optional, intent(IN) :: is, ie, js, je, ks, ke

   comp_data(comp_data_counter)%pos_name = data_name
   comp_data(comp_data_counter)%mpitime = MPI_WTIME()
   comp_data(comp_data_counter)%lev = lev
   comp_data(comp_data_counter)%id = id
  if (present(is)) comp_data(comp_data_counter)%is = is
  if (present(ie)) comp_data(comp_data_counter)%ie = ie
  if (present(js)) comp_data(comp_data_counter)%js = js
  if (present(je)) comp_data(comp_data_counter)%je = je
  if (present(ks)) comp_data(comp_data_counter)%ks = ks
  if (present(ke)) comp_data(comp_data_counter)%ke = ke

  comp_data_counter = comp_data_counter+1

end subroutine palm_SetCompData

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine palm_DataOutput()
  implicit none
  character fn*(STRING_LENGTH)
  character fc*(STRING_LENGTH)
  integer :: f_num
  integer :: i

  ! set file name
  write(fc,'(A3,I4.4)') '.pe',my_rank
  fn="DC"//trim(fc)

  f_num = FileNum+2
  open(f_num,FILE=fn)


  do i=1, comp_data_counter-1
      write(f_num,'(A32,F,I4,I4,I4,I4,I4,I4,I4,I4)')  &
      comp_data(i)%pos_name,comp_data(i)%mpitime, &
      comp_data(i)%lev, comp_data(i)%id, &
      comp_data(i)%is, comp_data(i)%ie, comp_data(i)%js, comp_data(i)%je, &
      comp_data(i)%ks, comp_data(i)%ke
  end do

  close(f_num)

end subroutine palm_DataOutput


!=======+=========+=========+=========+=========+=========+=========+=========+


end module palmtime
