#include <sys/types.h>
#include <grp.h>
#include <pwd.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

void sia_c_groupmax(int64_t *n) {
  long l;
  l=sysconf(_SC_GETGR_R_SIZE_MAX);
  if(l<=9999) l=9999;
  if(l>999999) l=999999;
  *n=l;
}

void sia_c_passwdmax(int64_t *n) {
  long l;
  l=sysconf(_SC_GETPW_R_SIZE_MAX);
  if(l<=9999) l=9999;
  if(l>999999) l=99999;
  *n=l;
}

void sia_c_getpwuid(int64_t n,char *buf,int64_t uid,int64_t *idex,int64_t *ierr) {
  struct passwd g, *gg;
  int ret;
  ret=getpwuid_r(uid,&g,buf,n,&gg);
  if(!gg) {
    *ierr=1;
    return;
  }
  *ierr=0;
  idex[0]=1+g.pw_name-buf;
  idex[1]=idex[0] + strlen(g.pw_name)-1;

  idex[2]=1+g.pw_passwd-buf;
  idex[3]=idex[2] + strlen(g.pw_passwd)-1;

  idex[4]=1+g.pw_gecos-buf;
  idex[5]=idex[4] + strlen(g.pw_gecos)-1;

  idex[6]=1+g.pw_dir-buf;
  idex[7]=idex[6] + strlen(g.pw_dir)-1;

  idex[8]=1+g.pw_shell-buf;
  idex[9]=idex[8] + strlen(g.pw_shell)-1;
}

void sia_c_getgrgid(int64_t n,char *buf,int64_t gid,int64_t *n1,int64_t *n2,char ***pmembers,int64_t *nmembers,int64_t *ierr) {
  struct group g, *gg;
  int ret;
  ret=getgrgid_r(gid,&g,buf,n,&gg);
  if(!gg) {
    *ierr=1;
    fprintf(stderr,"NO GG for %lld\n",gid);
    return;
  }
  *ierr=0;
  *n1=1+g.gr_name-buf;
  *n2=*n1 + strlen(g.gr_name)-1;
  *pmembers=g.gr_mem;
  *nmembers=0;
  while(1) {
    if(!g.gr_mem) break;
    if(!g.gr_mem[*nmembers]) break;
    if(!g.gr_mem[*nmembers][0]) break;
    ++ *nmembers;
  }
  //  fprintf(stderr,"gid = %lld name = %s has %lld members\n",gid,g.gr_name,*nmembers);
}

void sia_c_grmember(char *buf,char **pmembers,int64_t imember,int64_t *n1,int64_t *n2) {
  *n1=1+pmembers[imember-1]-buf;
  *n2=*n1 + strlen(pmembers[imember-1])-1;
}
