#include <glob.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

void sia_c_glob_init(int64_t *oGLOB_ERR, int64_t *oGLOB_MARK,
                     int64_t *oGLOB_NOSORT, int64_t *oGLOB_DOOFFS,
                     int64_t *oGLOB_NOCHECK, int64_t *oGLOB_APPEND,
                     int64_t *oGLOB_NOESCAPE, int64_t *oGLOB_PERIOD,
                     int64_t *oGLOB_ALTDIRFUNC, int64_t *oGLOB_BRACE, 
                     int64_t *oGLOB_NOMAGIC, int64_t *oGLOB_TILDE,
                     int64_t *oGLOB_TILDE_CHECK, int64_t *oGLOB_ONLYDIR) {
  *oGLOB_ERR=GLOB_ERR;
  *oGLOB_MARK=GLOB_MARK;
  *oGLOB_NOSORT=GLOB_NOSORT;
  *oGLOB_DOOFFS=GLOB_DOOFFS;
  *oGLOB_NOCHECK=GLOB_NOCHECK;
  *oGLOB_APPEND=GLOB_APPEND;
  *oGLOB_NOESCAPE=GLOB_NOESCAPE;
  *oGLOB_PERIOD=GLOB_PERIOD;
  *oGLOB_ALTDIRFUNC=GLOB_ALTDIRFUNC;
  *oGLOB_BRACE=GLOB_BRACE;
  *oGLOB_NOMAGIC=GLOB_NOMAGIC;
  *oGLOB_TILDE=GLOB_TILDE;
  *oGLOB_TILDE_CHECK=GLOB_TILDE_CHECK;
  *oGLOB_ONLYDIR=GLOB_ONLYDIR;
}

void sia_c_glob_free(glob_t *pglob) {
  globfree(pglob);
  free(pglob);
}

void sia_c_glob_nmatches(glob_t *pglob,int64_t *nmatches) {
  *nmatches = pglob->gl_pathc;
}

void sia_c_glob_match(glob_t *pglob,int64_t imatch,char **cp,int64_t *np) {
  char *c;
  //fprintf(stderr,"Get match %lld of %lld\n",imatch-1,pglob->gl_pathc);
  c=pglob->gl_pathv[imatch-1];
  *cp=c;
  *np=strlen(*cp);
}

void sia_c_glob_glob(glob_t **ppglob,char *pattern,int64_t flags,int64_t *ierr) {
  int ret,allocated=0;
  glob_t *pglob;

  *ierr=0;
  pglob=*ppglob;
  if(!pglob) {
    //fprintf(stderr,"allocate\n");
    allocated=1;
    pglob=(glob_t*)malloc(sizeof(glob_t));
    *ppglob=pglob;
    if(!pglob) {
      *ierr=1;
      //fprintf(stderr,"cannot allocate\n");
      return;
    }
  }

  //fprintf(stderr,"Glob \"%s\" %lld NULL %llx\n",pattern,flags,pglob);
  ret=glob(pattern,flags,NULL,pglob);
  //fprintf(stderr,"glob ret = %d\n",ret);

  if(ret) {
    //  fprintf(stderr,"glob free due to failure\n");
    *ierr=ret;
    if(allocated) {
      free(pglob);
      *ppglob=NULL;
    }
  }
}
