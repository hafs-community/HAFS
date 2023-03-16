#include <unistd.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#define _GNU_SOURCE
#include <sched.h>

#define F_IS_UNINITIALIZED -1
#define F_IS_RUNNING 0
#define F_IS_ERROR 1
#define F_IS_EXITED 2
#define F_IS_TERMINATED 3
#define F_IS_STOPPED 4
#define F_IS_CONTINUED 5

void sia_c_subprocess_init(void) {
  //fprintf(stderr,"Default SIGCHLD\n");
  signal(SIGCHLD,SIG_DFL);
}

void sia_c_waitpid(int64_t ipid,int64_t *oerrno,int64_t *ostate,int64_t *ostatearg) {
  pid_t pid=ipid,opid;
  int status;
  opid=waitpid(pid,&status,WNOHANG);
  if(opid<=0) {
    /* if(opid<0) */
    /*   fprintf(stderr,"c: waitpid(%lld,...) = %lld, errno says %lld %s\n", */
    /*           (long long)ipid,(long long)opid,(long long)errno,strerror(errno)); */
    /* else */
    /*   fprintf(stderr,"c: waitpid(%lld,...) = %lld\n",(long long)ipid,(long long)opid); */
  } else {
    *oerrno=0;
    if(WIFEXITED(status)) {
      *ostate=F_IS_EXITED;
      *ostatearg=WEXITSTATUS(status);
      /* fprintf(stderr,"c: waitpid(%lld,...) = exit %d\n", */
      /*         (long long)ipid,(int)*ostatearg); */
    } else if(WIFSIGNALED(status)) {
      *ostate=F_IS_TERMINATED;
      *ostatearg=WTERMSIG(status);
      /* fprintf(stderr,"c: waitpid(%lld,...) = term %d\n", */
      /*         (long long)ipid,(int)*ostatearg); */
    } else if(WIFSTOPPED(status)) {
      *ostate=F_IS_STOPPED;
      *ostatearg=WSTOPSIG(status);
      /* fprintf(stderr,"c: waitpid(%lld,...) = stop %d\n", */
      /*         (long long)ipid,(int)*ostatearg); */
    } else if(WIFCONTINUED(status)) {
      *ostate=F_IS_CONTINUED;
      *ostatearg=0;
      /* fprintf(stderr,"c: waitpid(%lld,...) = cont %d\n", */
      /*         (long long)ipid,(int)*ostatearg); */
    } else {
      // Unknown state.
      /* fprintf(stderr,"c: waitpid(%lld,...) = unknown %d\n", */
      /*         (long long)ipid,(int)*ostatearg); */
      *ostate=F_IS_ERROR;
      *ostatearg=0;
    }
  }
}

void sia_c_kill(int64_t ipid,int64_t isig,int64_t *oerrno) {
  pid_t pid=ipid;
  int sig=isig;
  if(kill(pid,sig))
    *oerrno=errno;
  else
    *oerrno=0;
}

void sia_c_background(char *cmd,int64_t *opid,int64_t *oerrno) {
  pid_t pid;
  char * arg[4];
  int i;
  fprintf(stderr,"c: %s: start me\n",cmd);

  // Construct the command:
  arg[0]="/bin/sh";
  arg[1]="-c";
  arg[2]=cmd;
  arg[3]=0;

  pid=vfork();
  if(pid<0) {
    *oerrno=errno;
    return;
  } else if(pid>0) {
    *opid=pid;
    *oerrno=0;
    fprintf(stderr,"c: %lld = %s\n",(long long)*opid,cmd);
    return;
  }
  /* for(i=1;i<64;i++) */
  /*   signal(i,SIG_DFL); */
  /* unshare(CLONE_FILES|CLONE_FS|CLONE_NEWNS); */
  // We are in the new child process.  If we reach this line, the
  // function will not return.

  //fprintf(stderr,"c: %s: in child\n",cmd);

  // Close all open file descriptors except those we need:
  /* closefrom(3); */

  //fprintf(stderr,"c: %s: closed fds\n",cmd);

  // Replace the process image with the new command
  //fprintf(stderr,"c: %s: execv(\"%s\",[\"%s\",\"%s\",\"%s\",NULL])\n",
  //        cmd,"/bin/sh","/bin/sh","-c",cmd);
  execv("/bin/sh",arg);

  // Ensure non-zero exit status upon failure to execv:
  exit(2);
}
