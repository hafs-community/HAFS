#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

void c_stat_lstat(const char *filename,int64_t want_lstat,void **sb,
                  int64_t *ierr,
        int64_t *dev,int64_t *ino,int64_t *mode,int64_t *nlink,int64_t *uid,
        int64_t *gid,int64_t *rdev,int64_t *size,int64_t *blksize,
        int64_t *blocks,int64_t *atime,int64_t *mtime,int64_t *ctime,
        double *artime,double *mrtime, double *crtime) {
  struct stat s;
  if((want_lstat&1))  {
    *ierr=lstat(filename,&s);
    //fprintf(stderr,"%s: c lstat=%lld\n",filename,*ierr);
  } else {
    *ierr=stat(filename,&s);
    //fprintf(stderr,"%s: c stat=%lld\n",filename,*ierr);
  }
  if(*ierr) return;
  *dev=s.st_dev;
  *ino=s.st_ino;
  *mode=s.st_mode;
  *nlink=s.st_nlink;
  *uid=s.st_uid;
  *gid=s.st_gid;
  *rdev=s.st_rdev;
  *size=s.st_size;
  *blksize=s.st_blksize;
  *blocks=s.st_blocks;
  *atime=s.st_atime;
  *mtime=s.st_mtime;
  *ctime=s.st_ctime;

  *artime = s.st_atim.tv_sec + s.st_atim.tv_nsec/(double)1e9;
  *mrtime = s.st_mtim.tv_sec + s.st_mtim.tv_nsec/(double)1e9;
  *crtime = s.st_ctim.tv_sec + s.st_ctim.tv_nsec/(double)1e9;

  if(sb && (want_lstat&2)) {
    if(!*sb) {
      //fprintf(stderr,"%s: alloc and copy statbuf\n",filename);
      *sb=malloc(sizeof(struct stat));
    }/* else
        fprintf(stderr,"%s: copy statbuf into pre-allocated area\n",filename);*/

    memcpy(*sb,&s,sizeof(struct stat));
  }
}
