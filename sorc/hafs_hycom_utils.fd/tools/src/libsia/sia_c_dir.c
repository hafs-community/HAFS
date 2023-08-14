#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

void c_rewinddir(DIR*d) {
  rewinddir(d);
}

void c_seekdir(DIR *d,int64_t offset) {
  seekdir(d,offset);
}

int64_t c_telldir(DIR *d) {
  long l;
  l=telldir(d);
  return l;
}

DIR *c_opendir(const char *fn,int64_t *dirent_size, int64_t *name_offset) {
  DIR *d=opendir(fn);
  if(d) {
    *dirent_size=offsetof(struct dirent,d_name) + 
      pathconf(fn,_PC_NAME_MAX)+1;
    *name_offset=offsetof(struct dirent,d_name)+1; // +1 for Fortran indexing
  } else {
    *dirent_size=-1;
    *name_offset=-1;
  }
  return d;
}

int64_t c_closedir(DIR *dp) {
  int i=closedir(dp);
  return i?errno:0;
}

int64_t c_readdir(DIR *d,struct dirent *entry) {
  struct dirent *out=NULL;
  int ret;
  /*  fprintf(stderr,"read_dir_r(%llx,%llx,%llx)\n",
      d,entry,&out);*/
  ret=readdir_r(d,entry,&out);
  if(ret>0) {
    return -ret;
  } else if(out!=entry) {
    return 0;
  } else {
    return strlen(entry->d_name);
  }
}
