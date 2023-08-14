#define _GNU_SOURCE 1
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <fcntl.h>
#include <utime.h>
#include <sys/time.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BLOCKSIZE 67108864

#define COPY_GID  1
#define COPY_TIME 2
#define COPY_MODE 4

void sia_c_rename(char *fromname,char *toname,int64_t *ierr) {
  fprintf(stderr,"c: rename \"%s\" \"%s\"\n",fromname,toname);
  if(rename(fromname,toname)) {
    *ierr=errno;
    fprintf(stderr,"c: cannot mv \"%s\" \"%s\": %lld %s\n",fromname,toname,(long long)errno,strerror(errno));
  } else
    *ierr=0;
}

void sia_c_chdir(char *todir,int64_t *ierr) {
  fprintf(stderr,"c: cd \"%s\"\n",todir);
  if(chdir(todir)) {
    *ierr=errno;
    fprintf(stderr,"c: cannot cd \"%s\": %lld %s\n",todir,(long long)errno,strerror(errno));
  } else
    *ierr=0;
}

void sia_c_mkdtemp(char *templ,int64_t *ierr) {
  if(!mkdtemp(templ))
    *ierr=errno;
  else
    *ierr=0;
}

void sia_c_symlink(char*from,char*to,int64_t *ierr) {
  if(symlink(from,to)) {
    *ierr=errno;
    if(!*ierr) *ierr=-1;
  } else
    *ierr=0;
}

void sia_c_hardlink(char*from,char*to,int64_t *ierr) {
  if(link(from,to)) {
    *ierr=errno;
    if(!*ierr) *ierr=-1;
  } else
    *ierr=0;
}

void sia_c_copy(char*from,char*to,int64_t flags,int64_t blocksize,int64_t *ierr) {
#define SENDIERR(label) do { fprintf(stderr,"%s:%d: \"%s\"=>\"%s\" error %s\n",__FILE__,__LINE__,from,to,strerror(errno)); *ierr=errno; if(!*ierr) { *ierr=-1; }; goto label; } while(0)
  struct timeval tv[2];
  int ifrom=-1,ito=-1;
  int64_t size=0,count=0,nread=0;

  struct stat fs, ts; // stat: (f)romfile, (t)ofile (s)tat
  int hfs=0, hts=0; // stat (h)ave flags for fs and ts
  void *buffer=NULL;

  // No error unless we change later
  *ierr=0;

  // Open input file without chainging access time:
  ifrom=open(from,O_NOATIME|O_NOCTTY|O_CLOEXEC|O_RDONLY);
  if(ifrom<0)
    // Retry the open without fancy status:
    ifrom=open(from,O_RDONLY|O_CLOEXEC);
  if(ifrom<0)
    SENDIERR(cleanup_final);

  if( !hfs && !(hfs=!stat(from,&fs)) ) // Note: assignment in "if" statement
    SENDIERR(cleanup_closefrom);
  if( (fs.st_mode&S_IFMT) != S_IFREG ) {
    // Refuse to copy non-files
    fprintf(stderr,"%s: will only copy regular files.\n",from);
    *ierr=-1;
    return;
  }

  // Open output file, create if needed, and truncate:
  ito=open(to,O_CREAT|O_TRUNC|O_NOCTTY|O_CLOEXEC|O_WRONLY,0600);
  if(ito<0)
    SENDIERR(cleanup_closefrom);

  // Decide block size
  if(blocksize<=0) {
    // NOTE: degraded performance if max(source,target) is not an
    // integer multiple of source and target.  Usually block sizes are
    // 2**integer though, so that should be rare.
    if( !hfs && !(hts=!stat(to,&ts)) ) // Note: assignment in "if" statement
      SENDIERR(cleanup_closeto);

    size=fs.st_blksize;
    if(ts.st_blksize>size) size=ts.st_blksize;
    if(size<512)
      size=512;
    else if(size>MAX_BLOCKSIZE)
      size=MAX_BLOCKSIZE;
  }

  /*fprintf(stderr,"Copy \"%s\"=%d to \"%s\"=%d bs=%lld flags=%lld\n",
    from,ifrom, to,ito, size, flags);*/

  // Allocate buffer
  if(! (buffer=malloc(size)) ) // Note: assignment in "if" statement
    SENDIERR(cleanup_dealloc);

  // Read/write loop
  do {
    nread=read(ifrom,buffer,size);
    /*fprintf(stderr,"Read %lld bytes of %lld from \"%s\"=%d into %llx\n",
      (long long)nread,(long long)size,from,ifrom,(long long)buffer);*/
    if(nread<0)
      SENDIERR(cleanup_dealloc);
    else if(nread==0)
      break;
    /*fprintf(stderr,"Will write %lld bytes to \"%s\"=%d\n",
      (long long)nread,to,ito);*/
    if(nread!=write(ito,buffer,nread))
      SENDIERR(cleanup_dealloc);
  } while(1);

  // Do we need to copy permissions, times or group?
  if( (flags & (COPY_GID|COPY_MODE|COPY_TIME)) ) {
    // First, close the destination so we do not further change the access time:
    close(ifrom);
    close(ito); 
    ito=-1;
    ifrom=-1;

    // Run a stat if we haven't already.
    if( !hfs && !(hfs=!stat(from,&fs)) ) // Note: assignment in "if" statement
      SENDIERR(cleanup_dealloc);
    if( !hfs && !(hts=!stat(to,&ts)) ) // Note: assignment in "if" statement
      SENDIERR(cleanup_dealloc);
    
    // Copy group ID:
    if( (flags & COPY_GID) && chown(to,-1,fs.st_gid) )
      SENDIERR(cleanup_dealloc);
    
    // Copy mode:
    if( (flags & COPY_MODE) && chmod(to,fs.st_mode & 07777))
      SENDIERR(cleanup_dealloc);
    
    // Copy timestamps:
    if( (flags&COPY_TIME) ) {
      tv[0].tv_sec=fs.st_atim.tv_sec;
      tv[0].tv_usec=fs.st_atim.tv_nsec/1000;
      tv[1].tv_sec=fs.st_mtim.tv_sec;
      tv[1].tv_usec=fs.st_mtim.tv_nsec/1000;
      if(utimes(to,tv))
        SENDIERR(cleanup_dealloc);
    }
  }

 cleanup_dealloc: if(buffer) free(buffer);
 cleanup_closeto: if(ito>=0) close(ito);
 cleanup_closefrom: if(ito>=0) close(ifrom);
 cleanup_final: return;
#undef SENDIERR
}

int64_t sia_c_umask(int64_t mask) {
  return umask(mask);
}

void sia_c_unlink(char*filename,int64_t *ierr) {
  *ierr=unlink(filename);
  fprintf(stderr,"%s: c unlink=%lld errno=%d=%s\n",filename,*ierr,errno,errno?strerror(errno):"(no error)");
}

void sia_c_rmdir(char*filename,int64_t *ierr) {
  *ierr=rmdir(filename);

  fprintf(stderr,"%s: c rmdir=%lld errno=%d=%s\n",filename,*ierr,errno,errno?strerror(errno):"(no error)");
}

void sia_c_mkdir(char*filename,int64_t mode,int64_t *ierr) {
  //fprintf(stderr,"sia c mkdir %5llo \"%s\"\n",mode,filename);
  *ierr=mkdir(filename,mode);
}

static void randsleep(int min,int randpart) {
  float frand=rand() / (float)RAND_MAX;
  int sleep=(int)(frand * randpart + min);
  fprintf(stderr,"randsleep(%d,%d) = usleep(%d)\n",min,randpart,sleep);
  usleep(sleep);
}

void sia_c_makedirs(char*filename,int64_t mode,int64_t *ierr) {
  char*c=filename,*d,store;
  struct stat s;
  int64_t err=0;
  int i;
  for(;*c=='/' && *c;c++);
  while(*c && !err) {
    fprintf(stderr,"Top of loop: c=\"%s\"\n",c);
    for(d=c;*d!='/' && *d;d++);
    if(c==d) break; // Reached last directory component.
    fprintf(stderr,"Also, d=\"%s\"\n",d);
    store=*d;
    *d=0;
    fprintf(stderr," so file=%s and path=%s\n",c,filename);
    if(strcmp(c,".") && strcmp(c,"..")) { // do not make . or ..
      for(i=0;i<5;i++) {    // try multiple times to handle race conditions
        fprintf(stderr,"%s: iteration %d\n",filename,(int)i);
        if(i>1) randsleep(200000,i*100000);
        if(stat(filename,&s)) {
          fprintf(stderr,"%s: mkdir\n",filename);
          err=mkdir(filename,mode);
          fprintf(stderr,"sia c mkdir %5llo \"%s\" = %lld\n",
                  mode,filename,err);
        } else if((s.st_mode&S_IFMT)!=S_IFDIR) { // exists, is not a dir
          fprintf(stderr,"%s: exists and is not a dir\n",filename);
          err=1;
          break;
        } else { // exists, is  a dir
          fprintf(stderr,"%s: exists and is a dir\n",filename);
          break;
        }
      }
    } else
      fprintf(stderr,"%s: do not make special names\n",c);
    *d=store;
    c=d;
    for(;*c=='/';c++);
  }
  *ierr=err;
}

void sia_c_chmod(char*filename,int64_t mode,int64_t *ierr) {
  //fprintf(stderr,"sia c chmod %5llo \"%s\"\n",mode,filename);
  *ierr=chmod(filename,mode);
}
