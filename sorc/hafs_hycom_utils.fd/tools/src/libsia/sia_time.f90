module sia_time
  implicit none
  private

  integer(kind=8), public, parameter :: CLOCK_REALTIME=1,&
       CLOCK_MONOTONIC=2, CLOCK_PROCESS_CPUTIME_ID=3,&
       CLOCK_THREAD_CPUTIME_ID=4, HRTIME_GETTIMEOFDAY=0

  public :: gettimeofday, hrtime, clock_gettime, tm, nanosleep, zeropad

  type :: tm
     integer(kind=8) :: sec,min,hour,mday,mon_m_1,year_m_1900,&
          wday,yday,isdst,ierr
     logical :: valid=.false.
   contains
     procedure:: gmtime => tm_gmtime
     procedure:: localtime => tm_localtime
     procedure:: strftime => tm_strftime
     procedure:: strptime => tm_strptime
     procedure:: mktime => tm_mktime
     procedure:: add => tm_add
  end type tm

  interface
     subroutine sia_c_nanosleep(rtime) bind(c)
       use iso_c_binding
       real(kind=c_double), value :: rtime
     end subroutine sia_c_nanosleep
  end interface

  interface
     subroutine c_strftime(used,s,max,fmt,&
          sec,min,hour,mday,mon_m_1,year_m_1900,wday,yday,isdst) bind(c)
       use iso_c_binding, only: c_int64_t,c_char,c_null_char
       implicit none
       integer(kind=8),value :: max,sec,min,hour,mday,mon_m_1,&
            year_m_1900,wday,yday,isdst
       integer(kind=8) :: used
       character(kind=c_char) :: s, fmt
     end subroutine c_strftime
  end interface

  interface
     subroutine sia_c_mktime(timet,&
          sec,min,hour,mday,mon_m_1,year_m_1900,wday,yday,isdst) bind(c)
       use iso_c_binding, only: c_int64_t,c_char,c_null_char
       implicit none
       integer(kind=c_int64_t),value :: sec,min,hour,mday,mon_m_1,&
            year_m_1900,wday,yday,isdst
       integer(kind=c_int64_t) :: timet
     end subroutine sia_c_mktime
  end interface

  interface
     subroutine c_strptime(used,s,max,fmt,&
          sec,min,hour,mday,mon_m_1,year_m_1900,wday,yday,isdst) bind(c)
       use iso_c_binding, only: c_int64_t,c_char,c_null_char
       implicit none
       integer(kind=8) :: max,sec,min,hour,mday,mon_m_1,&
            year_m_1900,wday,yday,isdst
       integer(kind=8) :: used
       character(kind=c_char) :: s(*), fmt(*)
     end subroutine c_strptime
  end interface

  interface
     subroutine c_gm_local_time(timep,have_timep,gm,&
          sec,min,hour,mday,mon_m_1, &
          year_m_1900,wday,yday,isdst,ierr) bind(c)
       use iso_c_binding, only: c_int64_t
       integer(kind=c_int64_t), value :: timep,have_timep,gm
       integer(kind=c_int64_t) :: sec,min,hour,mday, &
            mon_m_1,year_m_1900,wday,yday,isdst,ierr
     end subroutine c_gm_local_time
  end interface

  interface
     subroutine c_gettimeofday(when) bind(c)
       use iso_c_binding, only: c_int64_t
       integer(kind=c_int64_t) :: when(2)
     end subroutine c_gettimeofday
  end interface

  interface
     subroutine c_clock_gettime(which_timer,when) bind(c)
       use iso_c_binding, only: c_int64_t
       integer(kind=c_int64_t) :: when(2)
       integer(kind=c_int64_t), value :: which_timer
     end subroutine c_clock_gettime
  end interface

  interface
     subroutine sia_c_zeropad(num,digits,len,s) bind(c)
       use iso_c_binding
       integer(kind=c_int64_t),value :: num,digits,len
       character(kind=c_char) :: s(*)
     end subroutine sia_c_zeropad
  end interface

contains

  subroutine zeropad(num,digits,s) 
    use iso_c_binding
    character(len=*) :: s
    character(kind=c_char) :: s0(len(s)+1)
    integer(kind=c_int64_t) :: num,digits,len0
    integer i
    do i=1,len(s)
       s0(i)=s(i:i)
    enddo
    s0(i)=c_null_char
    len0=len(s)+1
    call sia_c_zeropad(num,digits,len0,s0)
    do i=1,len(s)
       if(s0(i)==c_null_char) exit
       s(i:i)=s0(i)
    enddo
    do while(i<=len(s))
       s(i:i)=' '
       i=i+1
    enddo
  end subroutine zeropad

  function tm_add(t,rtime)
    use iso_c_binding
    integer(kind=c_int64_t) :: rtime,ntime
    class(tm) :: t
    type(tm) :: tm_add

    call t%mktime(ntime)
    call tm_add%localtime(ntime+rtime)
  end function tm_add

  subroutine nanosleep(rtime)
    real(kind=8) :: rtime
    call sia_c_nanosleep(rtime)
  end subroutine nanosleep

  subroutine tm_mktime(t,time)
    use iso_c_binding
    class(tm), intent(inout) :: t
    integer(kind=c_int64_t) :: time
    call sia_c_mktime(time,t%sec,t%min,t%hour,t%mday,t%mon_m_1,t%year_m_1900,&
                      t%wday,t%yday,t%isdst)
  end subroutine tm_mktime
  
  subroutine tm_strftime(t,fmt,s)
    use iso_c_binding, only: c_int64_t,c_char,c_null_char
    class(tm), intent(inout) :: t
    character*500 :: buf
    character*(*) :: fmt,s
    integer(kind=8) :: used,i
    call c_strftime(used,buf,499,fmt//c_null_char,&
         t%sec,t%min,t%hour,t%mday,t%mon_m_1,t%year_m_1900,&
         t%wday,t%yday,t%isdst)
18  format('f: "',A,'" => "',A,'"')
    !print 18,fmt,trim(buf)
    i=1
    s=' '
    do while(i<=len(s) .and. i<=len(buf))
       if(buf(i:i)==c_null_char) exit
       s(i:i)=buf(i:i)
       i=i+1
    enddo
    !print 18,fmt,trim(s)
    used=min(used,len(s))
  end subroutine tm_strftime

  function tm_strptime(t,fmt,s)
    use iso_c_binding
    class(tm), intent(inout) :: t
    character*(*) :: fmt,s
    character(kind=c_char) :: fmt0(len(fmt)+1),s0(len(s)+1)
    integer(kind=c_int64_t) :: tm_strptime
    integer(kind=c_int64_t) :: used,i

    do i=1,len(fmt)
       fmt0(i)=fmt(i:i)
    enddo
    fmt0(i)=c_null_char

    do i=1,len(s)
       s0(i)=s(i:i)
    enddo
    s0(i:i)=c_null_char

    call c_strptime(used,s0,499,fmt0, &
         t%sec,t%min,t%hour,t%mday,t%mon_m_1,t%year_m_1900,&
         t%wday,t%yday,t%isdst)
    tm_strptime=min(used,len(s))
  end function tm_strptime

  subroutine tm_gmtime(t,timep)
    class(tm), intent(inout) :: t
    integer(kind=8), optional :: timep
    if(present(timep)) then
       call c_gm_local_time(timep,1,1,&
            t%sec,t%min,t%hour,t%mday,t%mon_m_1,&
            t%year_m_1900,t%wday,t%yday,t%isdst,t%ierr)
    else
       call c_gm_local_time(0,0,1,&
            t%sec,t%min,t%hour,t%mday,t%mon_m_1,&
            t%year_m_1900,t%wday,t%yday,t%isdst,t%ierr)
    endif
  end subroutine tm_gmtime

  subroutine tm_localtime(t,timep)
    class(tm), intent(inout) :: t
    integer(kind=8), optional :: timep
    if(present(timep)) then
       call c_gm_local_time(timep,1,0,&
            t%sec,t%min,t%hour,t%mday,t%mon_m_1,&
            t%year_m_1900,t%wday,t%yday,t%isdst,t%ierr)
    else
       call c_gm_local_time(0,0,0,&
            t%sec,t%min,t%hour,t%mday,t%mon_m_1,&
            t%year_m_1900,t%wday,t%yday,t%isdst,t%ierr)
    endif
  end subroutine tm_localtime

  subroutine gettimeofday(sec,usec)
    use iso_c_binding, only: c_int64_t
    integer(kind=8) :: when(2),sec,usec
    call c_gettimeofday(when)
    sec=when(1)
    usec=when(2)
  end subroutine gettimeofday

  subroutine clock_gettime(sec,nsec,which_timer)
    use iso_c_binding, only: c_int64_t
    integer(kind=8) :: when(2),sec,nsec
    integer, optional :: which_timer
    integer(kind=c_int64_t) :: wt
    if( present(which_timer)) then
       wt=which_timer
    else
       wt=CLOCK_REALTIME
    endif
    call c_clock_gettime(wt,when)
    sec=when(1)
    nsec=when(2)
  end subroutine clock_gettime

  function hrtime(which_timer)
    use iso_c_binding, only: c_int64_t
    integer, optional :: which_timer
    integer(kind=8) :: sec,part
    real(kind=8) :: hrtime
    integer :: wt
    if( present(which_timer)) then
       wt=which_timer
    else
       wt=CLOCK_REALTIME
    endif
    if(wt==HRTIME_GETTIMEOFDAY) then
       call gettimeofday(sec,part)
       hrtime=real(sec,kind=8) + real(part,kind=8)/1.0e6_8
    else
       call clock_gettime(sec,part,wt)
       hrtime=real(sec,kind=8) + real(part,kind=8)/1.0e9_8
    endif
  end function hrtime

end module sia_time
