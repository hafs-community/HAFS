#include <sys/time.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#define _XOPEN_SOURCE
#include <time.h>

char *strptime(const char *s, const char *format, struct tm *tm);

void sia_c_zeropad(int64_t num,int64_t digits,int64_t len,char *s) {
  char fmt[10];
  if(digits<1) digits=1;
  if(digits>9) digits=9;
  snprintf(fmt,10,"%%0%dd",(int)digits);
  snprintf(s,len,fmt,num);
}

void sia_c_mktime(int64_t *time,
                  int64_t sec,int64_t min,int64_t hour,int64_t mday,
                  int64_t mon_m_1,int64_t year_m_1900,int64_t wday,
                  int64_t yday,int64_t isdst) {
  struct tm t;
  t.tm_sec=sec;
  t.tm_min=min;
  t.tm_hour=hour;
  t.tm_mday=mday;
  t.tm_mon=mon_m_1;
  t.tm_year=year_m_1900;
  t.tm_wday=wday;
  t.tm_yday=yday;
  t.tm_isdst=isdst;
  *time=mktime(&t);
}

void sia_c_nanosleep(double rtime) {
  struct timespec req,rem;
  double seconds=floor(rtime);
  req.tv_sec=seconds;
  req.tv_nsec=(rtime-seconds)*1e9;
  //  fprintf(stderr,"Sleep %g = %lld + %lld/1e9\n",rtime,(long long)req.tv_sec,(long long)req.tv_nsec);
  nanosleep(&req,&rem);
}

void c_gettimeofday(int64_t tod[2]) {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  tod[0]=tv.tv_sec;
  tod[1]=tv.tv_usec;
}

void c_clock_gettime(int64_t which_timer,int64_t when[2]) {
  clockid_t timer;
  struct timespec tp;
  switch(which_timer) {
  case 1: timer=CLOCK_REALTIME ; break;
  case 2: timer=CLOCK_MONOTONIC ; break;
  case 3: timer=CLOCK_PROCESS_CPUTIME_ID ; break;
  case 4: timer=CLOCK_THREAD_CPUTIME_ID ; break;
  default: timer=CLOCK_REALTIME ; break;
  }
  clock_gettime(timer,&tp);
  when[0]=tp.tv_sec;
  when[1]=tp.tv_nsec;
}

void c_strftime(int64_t *used,char *s,int64_t max,char *fmt,
                int64_t sec,int64_t min,int64_t hour,int64_t mday,
                int64_t mon_m_1,int64_t year_m_1900,int64_t wday,
                int64_t yday,int64_t isdst) {
  struct tm t;
  size_t ret,stmax=max;
  t.tm_sec=sec;
  t.tm_min=min;
  t.tm_hour=hour;
  t.tm_mday=mday;
  t.tm_mon=mon_m_1;
  t.tm_year=year_m_1900;
  t.tm_wday=wday;
  t.tm_yday=yday;
  t.tm_isdst=isdst;
  ret=strftime(s,max,fmt,&t);
  //fprintf(stderr,"C: strftime \"%s\" => \"%s\"\n",fmt,s);
  *used=ret;
}

void c_strptime(int64_t *used,char *s,int64_t max,char *fmt,
                int64_t *sec,int64_t *min,int64_t *hour,int64_t *mday,
                int64_t *mon_m_1,int64_t *year_m_1900,int64_t *wday,
                int64_t *yday,int64_t *isdst) {
  struct tm t;
  size_t ret,stmax=max;
  char *p;
  memset(&t,0,sizeof(struct tm));
  p=strptime(s,fmt,&t);
  if(p) {
    *sec=t.tm_sec;
    *min=t.tm_min;
    *hour=t.tm_hour;
    *mday=t.tm_mday;
    *mon_m_1=t.tm_mon;
    *year_m_1900=t.tm_year;
    *wday=t.tm_wday;
    *yday=t.tm_yday;
    *isdst=t.tm_isdst;
    *used=p-s;
  } else
    *used=0;
}

void c_gm_local_time(int64_t timep, int64_t have_timep, int64_t gm,
              int64_t *sec, int64_t *min,int64_t *hour,int64_t *mday, 
              int64_t *mon_m_1,int64_t *year_m_1900,int64_t *wday,
              int64_t *yday,int64_t *isdst,int64_t *ierr) {
  struct tm t,*tp;
  time_t tt=timep;
  if(!have_timep) tt=time(&tt);
  if(gm) {
    tp=gmtime_r(&tt,&t);
  } else {
    tp=localtime_r(&tt,&t);
  }
  *sec=t.tm_sec;
  *min=t.tm_min;
  *hour=t.tm_hour;
  *mday=t.tm_mday;
  *mon_m_1=t.tm_mon;
  *year_m_1900=t.tm_year;
  *wday=t.tm_wday;
  *yday=t.tm_yday;
  *isdst=t.tm_isdst;
  if(tp!=&t) {
    *ierr=errno;
    return;
  }
}

