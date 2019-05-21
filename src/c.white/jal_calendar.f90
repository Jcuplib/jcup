!=======+=========+=========+=========+=========+=========+=========+=========+

module jal_calendar
  private
!--------------------------------   public  ----------------------------------!

  integer, public, parameter :: CALENDAR_NORMAL = 0
  integer, public, parameter :: CALENDAR_NOLEAPYEAR = 1
  integer, public, parameter :: CALENDAR_30360 = 2
  
  public :: init_calendar ! subroutine (calendar_type)
  public :: inc_calendar  ! subroutine (date, delta_t)
  public :: dec_calendar  ! subroutine (date, delta_t)
  public :: inc_month     ! subroutine (date, delta_m)
  public :: dec_month     ! subroutine (date, delta_m)
  public :: cal_date_diff ! subroutine (date1, date2, diff)
  public :: GetMonthDate  ! function (yyyy, mo)

!--------------------------------  private  ----------------------------------!

  integer :: calendar_type = CALENDAR_NORMAL

  integer, parameter, private :: BASIC_YEAR = 1200 ! mod(BASIC_YEAR, 400) == 0

  integer, parameter, private :: MONTH_DATE(12)            = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
  integer, parameter, private :: MONTH_DATE_SUM(12)        = (/ 0, 31, 59, 90,120,151,181,212,243,273,304,334/) 
  integer, parameter, private :: MONTH_DATE_LEAP(12)       = (/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
  integer, parameter, private :: MONTH_DATE_SUM_LEAP(12)   = (/ 0, 31, 60, 91,121,152,182,213,244,274,305,335/) 
  integer, parameter, private :: MONTH_DATE_30360(12)      = (/30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30/)
  integer, parameter, private :: MONTH_DATE_SUM_30360(12)  = (/ 0, 30, 60, 90,120,150,180,210,240,270,300,330/) 

  interface inc_calendar
    module procedure inc_calendar4, inc_calendar8
  end interface

  interface dec_calendar
    module procedure dec_calendar4, dec_calendar8
  end interface

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_calendar(type_calendar)
  implicit none
  integer, intent(IN) :: type_calendar

  select case(type_calendar)
  case(CALENDAR_NORMAL, CALENDAR_NOLEAPYEAR, CALENDAR_30360)
    calendar_type = type_calendar
  case default
     write(0,*) "init_calendar set error"
     stop 999
  end select

end subroutine init_calendar

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2016/05/23 [ADD] 

logical function is_leap_year(year)
  implicit none
  integer, intent(IN) :: year
  
  is_leap_year = .false.

  if ((calendar_type == CALENDAR_NOLEAPYEAR).or.(calendar_type == CALENDAR_30360)) return

  if (mod(year, 4) /= 0) return 

  if (mod(year, 400) == 0) then
    is_leap_year = .true.
    return
  end if

  if (mod(year, 100) == 0) return

  if (mod(year, 4) == 0) then
    is_leap_year = .true.
    return
  end if
    
end function is_leap_year

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function GetYearDate(yyyy)
  use jcup_utils, only : error
  implicit none
  integer,intent(IN) :: yyyy

  !if (yyyy<BASIC_YEAR) call jcup_error("GetYearDate","year : "//trim(IntToStr(yyyy))//" should be GE BASE_YEAR")

  select case(calendar_type)
  case(CALENDAR_30360)
    GetYearDate = 360*(yyyy-BASIC_YEAR)
  case(CALENDAR_NOLEAPYEAR)
    GetYearDate = 365*(yyyy-BASIC_YEAR)
  case(CALENDAR_NORMAL)
    GetYearDate = 365*(yyyy-BASIC_YEAR)+int((yyyy-BASIC_YEAR+3)/4) - int((yyyy-BASIC_YEAR-1)/100) + int((yyyy-BASIC_YEAR-1)/400)
  end select

end function GetYearDate

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function GetMonthDate(yyyy,mo)
  use jcup_utils, only : error
  implicit none
  integer,intent(IN) :: yyyy, mo

  !if ((mo<1).or.(mo>12)) call error("GetMonthDate","month should be 1<= <=12")
  if (calendar_type == CALENDAR_30360) then
    GetMonthDate = MONTH_DATE_SUM_30360(mo)
    return
  end if

  if (is_leap_year(yyyy)) then
    GetMonthDate = MONTH_DATE_SUM_LEAP(mo)
  else
    GetMonthDate = MONTH_DATE_SUM(mo)
  end if

end function GetMonthDate

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine time2sec(time, sec)
  implicit none
  integer,intent(IN) :: time(6)
  integer(kind=8), intent(INOUT) :: sec
  integer :: d_sec

  d_sec = 60*60*24

  sec = (GetYearDate(time(1))+GetMonthDate(time(1),time(2))+time(3)-1)*d_sec &
                 +time(4)*3600+time(5)*60+time(6)

end subroutine time2sec

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine sec2time(time, sec)
  implicit none
  integer, intent(INOUT) :: time(6)
  integer(kind=8),intent(IN) :: sec
  integer :: yyyy,mo,dd,hh,mm,ss
  integer(kind=8) :: y_mod, d_mod
  integer(kind=8) :: d_sec, day

   d_sec =60*60*24

   day = int(sec/d_sec)+1 ; d_mod = mod(sec,d_sec) ; hh = int(d_mod/3600) 
   mm = int((d_mod-hh*3600)/60) ; ss = d_mod-hh*3600-mm*60

   yyyy = BASIC_YEAR+1
   do 
     if (day<=GetYearDate(yyyy)) exit
     yyyy = yyyy+1
   end do
    
   yyyy = yyyy - 1
  
   y_mod = day - GetYearDate(yyyy)

   do mo = 1, 12
     if (y_mod<=GetMonthDate(yyyy,mo)) exit
   end do

   mo = mo-1

   if (mo>1) then 
     dd = y_mod-GetMonthDate(yyyy,mo) ! 2004/01/19
   else  
     dd = y_mod
   end if

   time(1) = yyyy ; time(2) = mo ; time(3) = dd 
   time(4) = hh   ; time(5) = mm ; time(6) = ss

end subroutine sec2time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine inc_calendar4(current_date, delta_t)
  implicit none
  integer, intent(INOUT) :: current_date(6)
  integer(kind=4), intent(IN) :: delta_t
  integer(kind=8) :: time_sec

  call time2sec(current_date, time_sec)
  time_sec = time_sec + delta_t
  call sec2time(current_date, time_sec)

end subroutine inc_calendar4

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine inc_calendar8(current_date, delta_t)
  implicit none
  integer, intent(INOUT) :: current_date(6)
  integer(kind=8), intent(IN) :: delta_t
  integer(kind=8) :: time_sec

  call time2sec(current_date, time_sec)
  time_sec = time_sec + delta_t
  call sec2time(current_date, time_sec)

end subroutine inc_calendar8

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine dec_calendar4(current_date, delta_t)
  implicit none
  integer, intent(INOUT) :: current_date(6)
  integer(kind=4), intent(IN) :: delta_t
  integer(kind=8) :: time_sec

  call time2sec(current_date, time_sec)
  time_sec = time_sec - delta_t
  call sec2time(current_date, time_sec)

end subroutine dec_calendar4

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine dec_calendar8(current_date, delta_t)
  implicit none
  integer, intent(INOUT) :: current_date(6)
  integer(kind=8), intent(IN) :: delta_t
  integer(kind=8) :: time_sec

  call time2sec(current_date, time_sec)
  time_sec = time_sec - delta_t
  call sec2time(current_date, time_sec)

end subroutine dec_calendar8

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine inc_month(current_date, delta_m)
  implicit none
  integer, intent(INOUT) :: current_date(6)
  integer, intent(IN)    :: delta_m

  current_date(1) = current_date(1) + int((current_date(2) + delta_m -1)/12)
  current_date(2) = mod(current_date(2) + delta_m, 12)
  if (current_date(2) == 0) current_date(2) = 12
end subroutine inc_month

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine dec_month(current_date, delta_m)
  implicit none
  integer, intent(INOUT) :: current_date(6)
  integer, intent(IN)    :: delta_m

  if ((current_date(2) - delta_m) <= 0) then
    current_date(1) = current_date(1) + int((current_date(2)-delta_m)/12)-1
    current_date(2) = current_date(2) - mod(delta_m, 12)
    if (current_date(2) == 0) current_date(2) = 12
    if (current_date(2) < 0) current_date(2) = 12 + current_date(2)
  else
    current_date(2) = current_date(2) - delta_m
  end if

end subroutine dec_month

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_date_diff(date1, date2, diff)
  implicit none
  integer, intent(IN) :: date1(6)
  integer, intent(IN) :: date2(6)
  integer, intent(OUT) :: diff
  integer(kind=8) :: sec1, sec2

  call time2sec(date1, sec1)
  call time2sec(date2, sec2)

  diff = int(sec1-sec2)

end subroutine cal_date_diff

end module jal_calendar

