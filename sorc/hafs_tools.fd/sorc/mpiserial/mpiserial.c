/**********************************************************************/
/*  PROGRAM MPISERIAL

PURPOSE: This program is a simple MPI wrapper that runs serial
programs on each MPI rank.  It has been tested on GAEA, Jet, Tide,
Gyre, Surge, Luna, and Theia.  Each MPI rank has two variables set:
SCR_COMM_RANK and SCR_COMM_SIZE containing the MPI rank and
communicator size.  There are three run modes, the default being
"mpmd."  In "mpmd" mode, a file listing commands is divvied out
among ranks as they become available.  

CAVEATS: This program is not a substitute for a vendor MPMD interface
that provides support for MPI in the commands, in that it can only
handle purely serial programs.  The script that is executed cannot run
any MPI programs, or most MPI implementations (including those on
Zeus, GAEA, Tide and Gyre) will fail in generally unpredictable ways.
The coupled model case is an example of what must be handled by the
vendor's MPMD method.

AUTHORSHIP: 

 circa 2010-2012 - various implementations made by Sam
   Trahan, George Vandenberghe and Hendrik Tollman.  

 late 2012 - Sam Trahan made a version that included best features of
   all versions

 early 2013 - wrong version (old wave model version) given to NCO,
   lacking error handling and SPMD support

 2013 March - "Merged" version tested on Tide, Gyre, GAEA, Zeus, Jet,
   Cirrus and Stratus and finalized.

 2015 April - Sam Trahan added optional verbosity and a workaround for
   an LSF signal handling issue

 2016 April - Sam Trahan replaced system() with vfork and execve to
   allow immediate exit on error.

 2017 Feb - Sam Trahan additions:
    - "mpmd" mode: mpi size does not have to match job count
    - entire process tree is killed on error, not just immediate child
    - rearranged immediate exit logic to avoid an MPI_Abort in one
      rank while others are in a global communication

---------

CALLING CONVENTION: This program can be called in one of two different
ways:

SINGLE PROGRAM MULTIPLE DATA (SPMD)

  mpiexec mpiserial 'command to run'

Called like that, the program will run the specified command on all
MPI ranks using /bin/sh.  This is intended to be used to execute
simple commands like hostname or sync.  However, more complex programs
can differentiate between tasks using $SCR_COMM_RANK.

Multiple arguments can be specified in SPMD mode, but they will simply
be appended to one another with a space between each.  That behavior
may be changed eventually to correctly handle arguments via the execvp
family of functions.

JOB LIST (MPMD)

  cp /my/command/file cmdfile
  mpiexec mpiserial

OR

  export SCR_CMDFILE=/my/command/file
  mpiexec mpiserial

SERIAL (SPMD)

  mpiexec mpiserial echo hello world

EXACT RANK COUNT (MPMD)

  # number of jobs must equal number of ranks
  export SCR_PGMMODE=MPMD
  export SCR_CMDFILE=/my/command/file
  mpiexec mpiserial

---------

EXIT STATUS: The return status from mpiserial is 0 if the serial
programs all exited with status 0.  If a command could not be
executed, or if an MPI error is encountered, mpiserial will return a
non-zero status.

 */
/**********************************************************************/

#include <time.h>
#include <sys/time.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <getopt.h>

////////////////////////////////////////////////////////////////////////

static double g_event_loop_sleep_time=0.5;

static const char *MPISERIAL_VERSION="3.0";

enum {
  MODE_DEFAULT=0,
  MODE_SPMD=1,
  MODE_OLD_MPMD=2,
  MODE_MPMD=3
};

static int g_run_mode=MODE_DEFAULT;

typedef struct {
  char *file, **commands;
  size_t *cmdlen;
  size_t len, next, alloclen;
} cmdfile;

static int world_size=0,world_rank=-1;

extern char **environ; /* special unix global for environment vars */

enum {
  EXIT_ON_ERROR=1, /* Exit immediately if any command exits non-zero */
  EXIT_AT_END=0 /* Exit after all commands complete or fail */
};

static int g_immediate_exit=EXIT_AT_END;

enum {
  VERB_QUIET=0,   /* -q or SCR_VERBOSITY=quiet */
  VERB_NORMAL=1,  /* default, -n, or SCR_VERBOSITY=normal */
  VERB_VERBOSE=2, /* -v or SCR_VERBOSITY=verbose */
  VERB_DEBUG=3    /* -d or SCR_VERBOSITY=debug */
};
static int g_verbosity=VERB_NORMAL; /* Verbosity level */

static const int max_command_len=1048576; /* Maximum allowed command length */

static int g_mpi_inited=0; /* Did we call MPI_Init yet? */
static const char * const  DEFAULT_CMDFILE="cmdfile"; /* default cmdfile name */

////////////////////////////////////////////////////////////////////////

static void usage(const char *why,...);
static void message(int level,const char *format,...);
static const char *timestamp();
static void die(const char *format,...);
static void mpicall(int loglev,int ret,const char *name);
static int scr_immediate_exit(void);
static int scr_verbosity(void);
static char *get_nonempty_env(const char *c);
static void lsf_signal_workaround();
static char *append_args(const int argc,const char * const * const argv);
static double doubletime();
static void doublesleep(double s);
static void checkpoint_message(int verbosity,int rc,int running,int max_rc,
                        int max_running,pid_t pid);
static void kill_descendants(int sig);
static void *loudalloc(size_t size);
static int scatter_commands(cmdfile *commands,int running,char **command);
static pid_t forkrun(char*cmd[4],const char*command);
static unsigned event_loop(char *command,cmdfile *commands);
static void hydra_workaround(void);
static int read_cmd_file(const char *i_file,cmdfile **in);
static void mpi_exit(int rc);
static int max_return_code(int rc);
static void initialize_environment();
int main(int argc,char **argv);

////////////////////////////////////////////////////////////////////////

static void usage(const char *why,...) {
  va_list ap;
  va_start(ap,why);

  if(world_rank>0) {
    /* Only rank 0 should print the usage messages.  Otherwise, we get
       a huge dump to stderr. */
  } else if(!strcmp(why,"HELP")) {
    /* Full usage message requested */
    fprintf(stderr,"mpiserial version %s\n"
            "  mpiserial [options] [run_mode]\n"
            "Synopsis:\n"
            "  Runs serial commands on MPI ranks.\n\n"
            "SPMD mode:\n"
            "  Runs the same command on all MPI ranks.\n"
            "    mpiserial [options] -s command to run\n"
            "  OR:\n"
            "    env SCR_PGMMODEL=SPMD mpiserial [options] command to run\n\n"
            "MPMD mode:\n"
            "  Runs different commands on each MPI rank.\n"
            "    mpiserial [options] -m [command files]\n"
            "  OR:\n"
            "    env SCR_PGMMODEL=MPMD mpiserial [options] [command files]\n"
            "  A command file may also be given in the $SCR_CMDFILE variable.\n"
            "  At least one command file must be given.\n\n"
            "OPTIONS:\n"
            "Verbosity:\n"
            "  -q or $SCR_VERBOSITY=quiet -- (q)uiet run mode\n"
            "  -n or $SCR_VERBOSITY=normal -- (n)ormal (default) verbosity\n"
            "  -v or $SCR_VERBOSITY=verbose -- be (v)erbose\n"
            "  -d or $SCR_VERBOSITY=debug -- extremely verbose\n"
            "Error Handling:\n"
            "  -a or $SCR_IMMEDIATE_EXIT=yes -- (a)bort on error if any process returns non-zero,\n"
            "              kill all running processes and exit with non-zero status.\n"
            "  -c or $SCR_IMMEDIATE_EXIT=no -- default: (c)ontinue on error (opposite of -a)\n"
            "Event Loop Settings:\n"
            "  -S f -- sleep f seconds (real number) between checking subprocess in\n"
            "          the main event loop\n"
            "Run Mode:\n"
            "  -m: MPMD mode (see above)\n"
            "  -M: MPMD mode, but number of commands must equal number of ranks\n"
            "  -s: SPMD mode (see above)\n",MPISERIAL_VERSION);
    fflush(stderr);
  } else {
    fprintf(stderr,"mpiserial version %s\n"
            "Format: mpiserial [options] [run_mode] [ command | command files ]\n\n"
            "Synopsis:\n"
            "  Runs serial commands on MPI ranks.\n\n"
            "Run \"mpiserial -h\" for full instructions.\n",MPISERIAL_VERSION);
    
    if(why!="USAGE")
      vfprintf(stderr,why,ap);
  }

  /* This MPI_Barrier is needed on some platforms to ensure the usage
     message is sent to the terminal before the other ranks exit */
  message(3,"%s: MPI Serial %d: MPI_Barrier after run ...\n",
          timestamp(),world_rank);

  va_end(ap);
  mpi_exit(1);
}

////////////////////////////////////////////////////////////////////////

static void message(int level,const char *format,...) {
  /* Debug message function.  Will print the message to stderr only if
     level<=verbosity. */
  va_list ap;

  va_start(ap,format);
  if(level<=g_verbosity)
    vfprintf(stderr,format,ap);
  va_end(ap);
}

////////////////////////////////////////////////////////////////////////

static const char *timestamp() {
  static char timebuf[100];
  struct timeval nowmilli;
  struct tm now;
  size_t len=0;

  memset(timebuf,0,100);
  
  gettimeofday(&nowmilli,NULL);
  gmtime_r(&(nowmilli.tv_sec),&now);
  
  strftime(timebuf,100,"%m/%d %H:%M:%S",&now);
  len=strlen(timebuf);
  snprintf(timebuf+len,100-len,".%03lld",(long long)nowmilli.tv_usec/1000);
  
  return timebuf;
}

////////////////////////////////////////////////////////////////////////

static void die(const char *format,...) {
  /* Error handling function.  Exits program with a message, calling
     MPI_Abort if needed.  On some MPI implementations it is critical
     to call MPI_Abort or the program may hang. */
  va_list ap;

  va_start(ap,format);
  vfprintf(stderr,format,ap);
  va_end(ap);

  if(g_mpi_inited)
    MPI_Abort(MPI_COMM_WORLD,2);
  exit(2);
  abort();
}

////////////////////////////////////////////////////////////////////////

static void mpicall(int loglev,int ret,const char *name) {
  /* This is the MPI error handling function.  It is a simple wrapper
     around "die" that generates an intelligible error message */
  char error[MPI_MAX_ERROR_STRING+1]="";
  int len=-1,err2=0;
  if(ret!=MPI_SUCCESS) {
    if((err2=MPI_Error_string(ret,error,&len))==MPI_SUCCESS) {
      error[len]='\0';
      die("%s: MPI error %d when %s: %s\n",timestamp(),ret,name,error);
    } else {
      die("%s: MPI error %d when %s, and MPI_Error_string could not process "
	  "that error number (it said %d) so I don't even know what went wrong!\n",
          timestamp(),ret,name,err2);
    }
  }
  if(world_rank<0)
    message(loglev,"%s: MPI Serial: %s successful.\n",timestamp(),name);
  else
    message(loglev,"%s: MPI Serial %d: %s successful.\n",timestamp(),world_rank,name);
}

////////////////////////////////////////////////////////////////////////

static int scr_immediate_exit(void) {
  /* Are we instructed to immediately exit on child non-zero exit
     status?  Taken from SCR_IMMEDIATE_EXIT environment variable.
     Returns g_immediate_exit if no value is set in the environment.*/

  char *immediate_exits=get_nonempty_env("SCR_IMMEDIATE_EXIT");;
  int immediate_exiti=g_immediate_exit;
  if(!immediate_exits || !*immediate_exits)
    return g_immediate_exit;
  if(!strcasecmp(immediate_exits,"yes") || !strcasecmp(immediate_exits,"true"))
    return 1;
  if(!strcasecmp(immediate_exits,"no") || !strcasecmp(immediate_exits,"false"))
    return 0;
  if(sscanf(immediate_exits,"%d",&immediate_exiti)>=1)
    return immediate_exiti;
  return g_immediate_exit;
}

////////////////////////////////////////////////////////////////////////

static int scr_pgmmodel(void) {
  /* Determines the run_mode from the SCR_PGMMODEL environment
     variable.  Returns MODE_DEFAULT if no value is specified. */
  int run_mode=MODE_DEFAULT;
  char *pgmmodel=get_nonempty_env("SCR_PGMMODEL");
  if(!pgmmodel)
    run_mode=MODE_DEFAULT;
  else if(!strcasecmp(pgmmodel,"OLD_MPMD"))
    run_mode=MODE_OLD_MPMD;
  else if(!strcasecmp(pgmmodel,"SPMD"))
    run_mode=MODE_SPMD;
  else if(!strcasecmp(pgmmodel,"MPMD"))
    run_mode=MODE_MPMD;
  else
    die("%s: Invalid value \"%s\" for $SCR_PGMMMODEL\n",
        timestamp(),pgmmodel);
  return run_mode;
}

////////////////////////////////////////////////////////////////////////

static int scr_verbosity(void) {
  /* What is the verbosity level? 
   (From SCR_VERBOSITY environment level.)*/
  char *cverb=get_nonempty_env("SCR_VERBOSITY");;
  int verbosity=VERB_NORMAL;

  if(!cverb) 
    return verbosity;

  if(!strcasecmp(cverb,"quiet"))
    return VERB_QUIET;
  else if(!strcasecmp(cverb,"normal"))
    return VERB_NORMAL;
  else if(!strcasecmp(cverb,"verbose"))
    return VERB_VERBOSE;
  else if(!strcasecmp(cverb,"debug"))
    return VERB_DEBUG;

  sscanf(cverb,"%d",&verbosity); /* on failure, sscanf will not modify verbosity */

  return verbosity;
}

////////////////////////////////////////////////////////////////////////

static char * get_nonempty_env(const char *c) {
  /* Returns the specified environment variable if it exists and
     consists of more than just blank characters.  Returns NULL
     otherwise. */
  char *val=getenv(c);
  if(!val || !*val)
    return NULL;
  while(*val) {
    if(!isblank(*val))
      return val;
    val++;
  }
  return NULL;
}

////////////////////////////////////////////////////////////////////////

static void signal_workarounds() {
  int isig=0;
  int lsf=0;
  int openmpi=0;
  
  /* Decide if we are using LSF.  The !! turns a pointer into 0 for
     NULL or 1 for non-NULL. */
  lsf+= !!get_nonempty_env("LSB_JOBNAME");
  lsf+= !!get_nonempty_env("LSB_HOSTS");
  lsf+= !!get_nonempty_env("LSB_JOBINDEX");
  lsf+= !!get_nonempty_env("LSB_SUB_USER");
  lsf+= !!get_nonempty_env("LSB_TRAPSIGS");
  lsf+= !!get_nonempty_env("LSF_VERSION");
  lsf+= !!get_nonempty_env("LSF_INVOKE_CMD");
  lsf+= !!get_nonempty_env("LSF_BINDIR");
  lsf=(lsf>=2); /* Require at least two recognized variables */


  /* Decide if we are using OpenMPI */
  openmpi+=!!get_nonempty_env("OMPI_COMM_WORLD_SIZE");
  openmpi+=!!get_nonempty_env("OMPI_COMM_WORLD_RANK");
  openmpi+=!!get_nonempty_env("OMPI_COMM_WORLD_LOCAL_RANK");
  openmpi+=!!get_nonempty_env("OMPI_UNIVERSE_SIZE");
  openmpi+=!!get_nonempty_env("OMPI_COMM_WORLD_LOCAL_SIZE");
  openmpi+=!!get_nonempty_env("OMPI_COMM_WORLD_NODE_RANK");
  openmpi=(openmpi>=2); /* Require at least two recognized variables */

  if(lsf || openmpi) {
    message((world_rank<1) ? 2 : 3,
            "%s: MPI Serial %d: restoring signal handlers 1-63 to work around LSF bug\n",
            timestamp(),world_rank);
    
    for(isig=1;isig<=63;isig++)
      signal(isig,SIG_DFL);
  } else {
    message((world_rank>0) ? 3 : 2,
            "%s: MPI Serial %d: not using LSF nor OpenMPI, retain signal handling setup\n",
            timestamp(),world_rank);
  }
}

////////////////////////////////////////////////////////////////////////

static char *append_args(const int argc,const char * const * const argv) {
  /* Appends contents of argv together with spaces in between,
     allocating memory as needed.  Returns the resulting string. */
  char *buf=NULL;
  int iarg=0;
  size_t len=0,len1=0,buflen=0;

  if(argc==0)
    return strdup("/bin/false"); /* no arguments given */

  len=0;
  for(iarg=0;iarg<argc;iarg++)
    len+=1+strlen(argv[iarg]);
  if(!(buf=(char*)malloc(len)))
    die("%s: Unable to allocate %llu bytes.\n",
	timestamp(),(unsigned long long)len);

  buflen=len;
  len=0;
  for(iarg=0;iarg<argc;iarg++) {
    len1=strlen(argv[iarg]);
    memcpy(buf+len,argv[iarg],len1);
    len+=len1;

    if(iarg==argc-1)
      buf[len]='\0';
    else
      buf[len]=' ';
    len++;
  }
  if(len!=buflen)
    die("%s: ASSERTION FAILURE: %llu!=%llu in %s:%d\n",
	timestamp(),
        (unsigned long long)len,
        (unsigned long long)buflen,
        __FILE__,__LINE__);

  return buf;
}

////////////////////////////////////////////////////////////////////////

static double doubletime() {
  /* Return the time since the UNIX epoch in seconds, with nanosecond
     precision. */
  struct timeval nowmilli = { 0, 0};
  gettimeofday(&nowmilli,NULL);
  return (  (double)nowmilli.tv_sec + (double)nowmilli.tv_usec/1e6 );
}

////////////////////////////////////////////////////////////////////////

static void doublesleep(double s) {
  /* Sleep s seconds. */
  struct timespec rem, req;
  double start=doubletime(),target=start+s,naptime=0;
  do {
    naptime=target-doubletime();
    if(naptime<1e-7)
      return;
    req.tv_sec=floor(naptime);
    req.tv_nsec=fmodf(naptime,(double)1)*1e9;
    if(req.tv_nsec>999999999)
      req.tv_nsec=999999999;
    if(req.tv_nsec<0)
      req.tv_nsec=0;
    if(req.tv_nsec<100000 && req.tv_sec<1)
      req.tv_nsec=100000;

    nanosleep(&req,&rem);

  } while(doubletime() < target);
}

////////////////////////////////////////////////////////////////////////

static void checkpoint_message(int verbosity,int rc,int running,int max_rc,
                               int max_running,pid_t pid) {
  if(running)
    message(verbosity,"%s: MPI Serial %d: process %lld is still running.\n",
	    timestamp(),world_rank,(long long)pid);
  else if(max_running)
    message(verbosity,"%s: MPI Serial %d: process %lld exited (status %d), waiting"
	    " for other processes.\n",timestamp(),world_rank,(long long)pid,rc);
  else
    message(verbosity,"%s: MPI Serial %d: all processes exited.  My process (%lld) "
	    "status was %d, max status was %d\n",
	    timestamp(),world_rank,(long long)pid,rc,max_rc);
}

////////////////////////////////////////////////////////////////////////

static void kill_descendants(int sig) {
  /* Kill all descendant processes with signal `sig` without killing
     self */

  struct sigaction old,other;

  sigaction(sig,NULL,&old); /* store old signal action */
  signal(sig,SIG_IGN); /* Disable the signal for my process */

  /* Note: race condition here, albeit extremely unlikely.  It is
     possible something else will send signal "sig" between the above
     "signal" line and the next "sigaction" line intending to kill
     this process.  If so, that signal will lbe ignored. */

  kill(0,sig); /* Kill myself and descendants with the signal */

  sigaction(sig,&old,&other); /* restore old signal action */
}

////////////////////////////////////////////////////////////////////////

static void *loudalloc(size_t size) {
  /* Allocate size bytes or call die() */
  void *v=malloc(size);
  if(!v)
      die("%s: MPI Serial %d: cannot allocate %d bytes; %s\n",
          timestamp(),world_rank,size,strerror(errno));
  return v;
}

////////////////////////////////////////////////////////////////////////

static int scatter_commands(cmdfile *commands,int running,char **command) {
  /* For each process that is not running, scatter_commands will send
     a command to run.  Once there are no more commands to run,
     remaining ranks will receive the null string. Returns the number
     of commands still in `commands`.

  commands - the cmdfile with commands to run
  running - is my rank already running commands?
  command - a pointer to the command to run */
  static int *runbuf=NULL,*countbuf=NULL,*displbuf=NULL;
  char *cmdbuf=NULL,*tgt=NULL,*newcommand=NULL;
  size_t send=0,bufidx=0,cmdlen=0,next=0,count=0,index=0,displ;
  int irank=0;
  int left=0;

  /* Allocate space if it is not already allocated */
  if(!world_rank && !runbuf)
    runbuf=(int*)loudalloc(sizeof(int)*world_size);

  if(!countbuf) {
    countbuf=(int*)loudalloc(sizeof(int)*world_size);
    displbuf=(int*)loudalloc(sizeof(int)*world_size);
  }
 
  /* Gather run flags to root */
  mpicall(2,MPI_Gather(&running,1,MPI_INT,runbuf,1,MPI_INT,
                       0,MPI_COMM_WORLD),
          "getting run flags");

  if(world_rank==0) {
    /* Determine send buffer length on root */
    next=commands->next;
    send=0;
    for(irank=0;irank<world_size;irank++) {
      if(!runbuf[irank] && next<commands->len) {
        send+=commands->cmdlen[next]+1;
        message(3,"%s: MPI Serial 0: rank %d: command length %d\n",
                timestamp(),irank,commands->cmdlen[next]);
        next++;
      } else {
        message(3,"%s: MPI Serial 0: rank %d: no command\n",
                timestamp(),irank);
        send++;
      }
    }

    message(3,"%s: MPI Serial 0: send buffer size is %llu bytes\n",
            timestamp(),(unsigned long long)sizeof(char)*send);

    cmdbuf=loudalloc(sizeof(char)*send);

    /* Store information for scatters on root */
    next=commands->next;
    index=0;
    for(irank=0;irank<world_size;irank++) {
      if(!runbuf[irank] && next<commands->len) {
        /* Store next command */
        displbuf[irank]=index;
        countbuf[irank]=commands->cmdlen[next];
        message(3,"%s: MPI Serial 0: rank %d: command length %d\n",
                timestamp(),irank,commands->cmdlen[next]);
        if(strlen(commands->commands[next])+1 != commands->cmdlen[next])
          die("%s: MPI Serial 0: command %d cmdlen[%d]=%d not %d\n",
              timestamp(),next,next,commands->cmdlen[next],
              next,commands->commands[next]);
        memcpy(cmdbuf+index,commands->commands[next],commands->cmdlen[next]);
        index+=commands->cmdlen[next];
        next++;
      } else {
        /* Store empty string instead of command for ranks that don't
           need commands or are still running */
        message(3,"%s: MPI Serial 0: rank %d: no command\n",
                timestamp(),irank);
        displbuf[irank]=index;
        countbuf[irank]=1;
        cmdbuf[index]=0;
        index++;
      } 
      message(3,"%s: MPI Serial 0: rank %d: displ=%llu count=%llu\n",
              timestamp(),irank,(unsigned long long)displbuf[irank],
              (unsigned long long)countbuf[irank]);
      if(index>send) {
        die("%s: MPI Serial 0: ASSERTION FAILURE: index>send %llu>=%llu\n",
            timestamp(),(unsigned long long)index,
            (unsigned long long)send);
      }
    }
    commands->next=next;
    left=commands->len-commands->next;
  }

  /* Send number of remaining commands */
  mpicall(2,MPI_Bcast(&left,1,MPI_INT,0,MPI_COMM_WORLD),
          "sending number of remaining commands");

  /* Send displacements and counts to all ranks. */
  message(3,"%s: MPI Serial %d: bcast displacements\n",timestamp(),world_rank);
  mpicall(2,MPI_Bcast(displbuf,world_size,MPI_INT,0,MPI_COMM_WORLD),
          "broadcasting displacements for later scatterv");
  message(3,"%s: MPI Serial %d: bcast counts\n",timestamp(),world_rank);
  mpicall(2,MPI_Bcast(countbuf,world_size,MPI_INT,0,MPI_COMM_WORLD),
          "broadcasting counts for later scatterv");

  message(3,"%s: MPI Serial %d: allocate receive buffer\n",timestamp(),world_rank);

  /* Get the length of my command */
  count=countbuf[world_rank];
  newcommand=(char*)loudalloc(count);

  /* Send commands to all ranks */
  message(3,"%s: MPI Serial %d: scatterv commands\n",timestamp(),world_rank);
  mpicall(2,MPI_Scatterv(cmdbuf,countbuf,displbuf,MPI_CHAR,
                         newcommand,count,MPI_CHAR,0,MPI_COMM_WORLD),
          "scatterv commands to all ranks");

  if(!world_rank) {
    message(3,"%s: MPI Serial %d: free command buffer\n",timestamp(),world_rank);
    free(cmdbuf);
  }

  *command=newcommand;
  return left;
}

////////////////////////////////////////////////////////////////////////

static pid_t forkrun(char*cmd[4],const char*command) {
  /* Forks the process and runs the command in `cmd`.  Return value is
     the pid of the child process, or (pid_t)-1 on failure.  Child
     process does not return.  The `command` is only used for
     logging.

  NOTE: This implementation uses vfork.  That is needed due to design
  limitations of the Linux infiniband drivers.  The drivers fail after
  a fork() but are tolerant of vfork() */

  pid_t pid;
  message(2,"%s: MPI Serial %d: vfork -> sh -c \"%s\"\n",
	  timestamp(),world_rank,command);
  pid=vfork(); /* SPLIT PROCESS HERE */
  if(pid<0)
    message(-1,"%s: MPI Serial %d: %s: ERROR: vfork failed: %s\n",
	    timestamp(),world_rank,command,strerror(errno));
  else if(pid==0)
    /* We are in the child process.  After vfork, only two functions
       can safely be called: execve and _exit.  The below line will
       run the command using /bin/sh, and exit with the exit status of
       execve if /bin/sh cannot be started. */
    _exit(execve("/bin/sh",cmd,environ)); /* never returns */
  return pid;
}

////////////////////////////////////////////////////////////////////////

static unsigned event_loop(char *command,cmdfile *commands) {
  /* Main event loop for mpiserial.

     Runs the specified command, and returns an exit status which will
     be in the range 0-255.  This correctly handles killed or stopped
     jobs, returning 128 plus the signal number. 

  Return value is the maximum return code from any process.

  command = single command to run (SPMD or old MPMD mode)
  commands = mpmd mode: list of commands to run (rank 0) or
    non-NULL placeholder (other ranks
  */
  unsigned rc=99, running=1, checked=0, joblist_empty=0;
  unsigned send[3],recv[3], my_max_return_code=0;
  int ret=-200, immediate_exit=1, maxret=-200, i=-1,said_inactive=0;
  int commands_left=1e9;
  time_t lastprint=0,now=0;
  pid_t pid=-1,pid2=-1;
  char *cmd[4],*newcommand=NULL;

  recv[0]=0; // Below logic needs this in mpmd mode.

  /* Are we instructed to immediately exit on error? */
  immediate_exit=g_immediate_exit;

  cmd[0]=strdup("/bin/sh");
  cmd[1]=strdup("-c");
  cmd[2]=NULL;
  cmd[3]=NULL;

  if(commands) {
    command=NULL;
    running=0;
    rc=0;
  } else if(command) {
    cmd[2]=strdup(command);
    pid=forkrun(cmd,command);
  } else {
    running=0;
    rc=0;
    message(2,"%s: MPI Serial %d: Nothing to do.  Will wait for other ranks to exit.\n",
           timestamp(),world_rank);
  }

  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
  message(2,"%s: MPI Serial %d: %s: before main loop\n",
	  timestamp(),world_rank,command);
  do {
    checked=0;
    message(3,"%s: MPI Serial %d: %s: top of main loop\n",
	    timestamp(),world_rank,command);
    if(immediate_exit && recv[0])
      /* Refuse to run any more commands if we are in immediate_exit
         mode and any commands returned non-zero status. */
      commands_left=0;
    else if(commands) {
      message(3,"%s: MPI Serial %d: does anyone need a new command?\n",
	      timestamp(),world_rank);
      /* We are allowed to request new commands.  All ranks must enter
         here in order for us to use a global communication */
      commands_left=scatter_commands(commands,running,&newcommand);
      if(!running && newcommand && *newcommand) {
        if(command)
          free(command);
        command=newcommand;
        message(1,"%s: MPI Serial %d: starting: %s\n",
	      timestamp(),world_rank,newcommand);
        running=1;
        cmd[2]=command;
        pid=forkrun(cmd,command);
        if(pid<0) {
          rc=255;
          message(-1,"%s: MPI Serial %d: %s: vfork failed.  Set rc=%d.\n",
                  timestamp(),world_rank,command,rc); 
        }
      } else if(!running) {
        message(said_inactive ? 3 : 2,
                "%s: MPI Serial %d: inactive; no new command received\n",
                timestamp(),world_rank);
        said_inactive=1;
      } else
        message(3,"%s: MPI Serial %d: active; already have a command\n",
                timestamp(),world_rank);
    }

    if(running && pid<0) {
      /* Call to vfork failed.  Abort will be handled later in this loop. */
      rc=255;
      message(-1,"%s: MPI Serial %d: %s: vfork failed.  Set rc=%d.\n",
	      timestamp(),world_rank,command,rc);
    } else if(running) {
      /* Child was running in last iteration of loop.  Is child still
	 running? */
      message(3,"%s: MPI Serial %d: %s: running; call waitpid.  Set rc=%d.\n",
	      timestamp(),world_rank,command,rc);
      pid2=waitpid(pid,&ret,WNOHANG);
      checked=1;
      rc=0; /* Abort if this is non-zero in any rank after the big IF
	       block below. */
      if(pid2==pid) {
        /* We have an update on the status of our child process. */
	message(3,"%s: MPI Serial %d: %s: waitpid status=%d.\n",
		timestamp(),world_rank,command,ret);
	if(WIFEXITED(ret)) {
	  rc=(unsigned int)WEXITSTATUS(ret);
	  running=0;
	  message( rc==0 ? 1 : 0,
                   "%s: MPI Serial %d: %s: exit %d\n",
		  timestamp(),world_rank,command,rc);
	} else if(WIFSIGNALED(ret)) {
	  rc=(unsigned int)(128+WTERMSIG(ret));
	  running=0;
	  message(0,"%s: MPI Serial %d: %s: signal %d\n",
		  timestamp(),world_rank,command,WTERMSIG(ret));
	}  else if(WIFSTOPPED(ret)) {
	  rc=(unsigned int)(128+WSTOPSIG(ret));
	  running=0;
	  message(0,"%s: MPI Serial %d: %s: stopped by signal %d\n",
		  timestamp(),world_rank,command,WSTOPSIG(ret));
	} else {
	  message(0,"%s: MPI Serial %d: %s: unrecognized return status %d "
		  "(not -1, not exited, not signalled, not stopped)\n",
		  timestamp(),world_rank,command,ret);
	  running=0;
	  rc=255;
	}
      } else if(pid<0) {
	message(-1,"%s: MPI Serial %d: %s: ABORT: waitpid failed: %s\n",
		timestamp(),world_rank,command,strerror(errno));
	rc=255;
      } else {
	message(3,"%s: MPI Serial %d: %s: no status update from waitpid\n",
		timestamp(),world_rank,command);
      }
    } else if(!commands) {
      /* We are not running and there are no additional commands to run. */
      message(3,"%s: MPI Serial %d: %s: not checking process; no longer running.\n",
	      timestamp(),world_rank,command);
    }

    if(rc>my_max_return_code) {
      my_max_return_code=rc;
      message(3,"%s: MPI Serial %d: max return code now %d\n",
              timestamp(),world_rank,my_max_return_code);
    }

    send[0]=my_max_return_code;
    send[1]=running;
    send[2]=checked;
    message(3,"%s: MPI Serial %d: %s: local: rc=%d running=%d checked=%d\n",
	    timestamp(),world_rank,command,send[0],send[1],send[2]);
    mpicall(3,MPI_Allreduce(send,recv,3,MPI_UNSIGNED,MPI_MAX,MPI_COMM_WORLD),
	    "using an mpi_allreduce to get the maximum return code in monitor loop");

    now=time(NULL);
    if(now-lastprint>300 && g_verbosity<=1) {
      checkpoint_message(1,rc,running,recv[0],recv[1],pid);
      lastprint=now;
    } else if(now-lastprint>30 && g_verbosity<=2) {
      checkpoint_message(2,rc,running,recv[0],recv[1],pid);
      lastprint=now;
    } else {
      message(3,"%s: MPI Serial %d: %s: allreduce: recv[0]=rc=%d recv[1]=running=%d recv[2]=checked=%d\n",
	      timestamp(),world_rank,command,recv[0],recv[1],recv[2]);
      checkpoint_message(3,rc,running,recv[0],recv[1],pid);
    }

    if(!recv[1]) {
      if(!commands_left || !commands) {
        message(1,"%s: MPI Serial %d: no tasks running.  Should exit %d.\n",
                timestamp(),world_rank,recv[0]);
        return recv[0];
      }
    } else if(!recv[2]) {
      die("%s: MPI Serial %d: ASSERTION FAILURE: no tasks checked children, but children are running.\n",
	  timestamp(),world_rank);
    } else {
      message(3,"%s: MPI Serial %d: non-zero recv[1]=%d\n",
	      timestamp(),world_rank,recv[1]);
    }

    if(recv[0] && running && immediate_exit) {
      /* At least one rank failed and we are instructed to exit immediately. */
      message(-1,"%s: MPI Serial %d: %s: non-zero exit from another rank; kill child process.\n",
	      timestamp(),world_rank,command);
      if(running && pid>0) {
        message(0,"%s: MPI Serial %d: %s: send SIGINT to %d.\n",
                timestamp(),world_rank,command,pid);
	kill_descendants(SIGINT);
	/* Wait 5 seconds for child to exit from SIGINT */
	for(i=0;i<100 && running;i++) { /* 100*0.05=5 seconds */
	  pid2=waitpid(pid,&ret,WNOHANG);
	  if(pid2==pid && (WIFEXITED(ret) || WIFSIGNALED(ret))) {
	    /* Child exited. */
            message(0,"%s: MPI Serial %d: %s: child exited from SIGINT.\n",
                    timestamp(),world_rank,command);
	    rc=recv[0];
            running=0;
            break;
          }
	  doublesleep(0.05);
	}
        if(running) {
          message(0,"%s: MPI Serial %d: %s: send SIGTERM to %d.\n",
                timestamp(),world_rank,command,pid);
          kill_descendants(SIGTERM);
          /* Wait 5 seconds for child to exit from SIGTERM */
          for(i=0;i<100 && running;i++) { /* 100*0.05=5 seconds */
            pid2=waitpid(pid,&ret,WNOHANG);
            if(pid2==pid && (WIFEXITED(ret) || WIFSIGNALED(ret))) {
              /* Child exited. */
              message(0,"%s: MPI Serial %d: %s: child exited from SIGTERM.\n",
                      timestamp(),world_rank,command);
              rc=recv[0];
              running=0;
              break;
            }
            doublesleep(0.05);
          }
        }
	if(running) {
	  /* Child ignored SIGINT and SIGTERM.  Send SIGKILL and a
	     scary message. */
	  message(-1,"%s: MPI Serial %d: %s: DANGER: child did not exit from SIGTERM twice.  "
		  "Sending STOP, KILL, CONT.\n",timestamp(),world_rank,command);
          kill(pid,SIGSTOP);
	  kill(pid,SIGKILL);
          kill(pid,SIGCONT);
          message(3,"%s: MPI Serial %d: %s: after KILL\n",
                  timestamp(),world_rank,command);
	  pid2=waitpid(-pid,&ret,WNOHANG);
          rc=recv[0];
          running=0;
	}
      }
    }

    message(3,"%s: MPI Serial %d: %s: sleep 0.5 seconds recv={%d %d}\n",
	    timestamp(),world_rank,command,recv[0],recv[1]);
    doublesleep(g_event_loop_sleep_time);
  } while(recv[1] || (commands && commands_left)); /* while any ranks are running - bottom of main loop*/
  message(1,"%s: MPI Serial %d: %s: exited loop.\n",
	  timestamp(),world_rank,command);
  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
  
  if(rc>255||rc<0) rc=255;

  return rc;
}

////////////////////////////////////////////////////////////////////////

static void hydra_workaround(void) {
  /* Make sure stdin is not a directory.  This is a workaround for a
     bug in the hydra MPI launcher. */
  struct stat s;
  if(!fstat(0,&s) && S_ISDIR(s.st_mode)) {
    fprintf(stderr,"warning: stdin is a directory; reopening /dev/null.  This is a bug in the Hydra MPI.  Submit a ticket to your helpdesk.\n");
    int x=open("/dev/null",O_RDONLY);
    if(x>0)
      dup2(x,0);
    else
      fprintf(stderr,"error: could not open /dev/null: %s\n",strerror(errno));
  }
}

////////////////////////////////////////////////////////////////////////

static int read_cmd_file(const char *i_file,cmdfile **in) {
  /* Reads the given command file, placing the inputs in `*in`.
     If i_file is NULL, then the DEFAULT_CMDFILE is read.  

     Return value is always 0 unless i_file is NULL and the default
     command file cannot be opened for reading.  In that case, 1 is
     returned.

     At the first call to read_cmd_file, `in` should be a pointer to
     NULL.  This call will allocate the cmdfile.  Later calls fill in
     additional data.
   */
  char *file=NULL; /* duplicate (strdup) of i_file or DEFAULT_CMDFILE*/
  FILE *f=NULL; /* FILE opened for `file` */
  char buf[max_command_len]; /* Command storage buffer */
  char *cret=NULL; /* Return value from fgets, used for error detection */
  char *point=NULL; /* Character iterator for blank and comment parsing */
  char *command=NULL; /* Command on current line */
  size_t cmdlen=0; /* strlen(command) as a size_t*/
  int icmdlen=-999; /* strlen(command) as an int */
  int lastline=-999; /* flag: is this the last line in the file? */
  int blankline=-999; /* flag: is this line blank? */
  int fail=0; /* flag: did parsing failure on this line? */
  cmdfile *all=*in; /* alias to *in */

  /* Figure out which file to read, and duplicate the string */
  if(!i_file)
    /* NULL i_file requests a read of the DEFAULT_CMDFILE. */
    file=strdup(DEFAULT_CMDFILE);
  else if(!*i_file) /* ignore null string */
    return 0;

  file=strdup(i_file);

  if(world_rank==0)
    message(1,"%s: MPI_Serial 0: command file \"%s\"\n",timestamp(),file);

  /* Initialize cmdfile if needed */
  if(!all) {
    /* First call to read_cmd_file.  Must initialize cmdfile struct. */
    all=(cmdfile*)loudalloc(sizeof(cmdfile));
    all->file=file;
    all->commands=NULL;
    all->len=0;
    all->next=0;
    all->cmdlen=NULL;
    all->alloclen=0;
    *in=all;
  }

  /* Only rank 0 continues from this lien forward */
  if(world_rank!=0)
    return 0;

  /* Open the file */
  f=fopen(file,"rt");
  if(!f) {
    if(!i_file && !in) {
      /* Special case: default command file was given and this is the first call. */
      message(-1,"%s: Unable to open command file $SCR_CMDFILE=\"%s\": %s\n",
              timestamp(),file,strerror(errno));
      return 1;
    }
    die("%s: Unable to open command file $SCR_CMDFILE=\"%s\": %s\n",
        timestamp(),file,strerror(errno));
  }

  message(3,"%s: MPI Serial begin parser loop\n",timestamp());

  lastline=0;

  while(!lastline) {
    blankline=1;

    /* Read lines until we reach a non-blank, non-comment line */
    while(blankline && !lastline) {
      cret=fgets(buf,max_command_len,f);
      if(cret!=buf) {
        if(feof(f)) {
          lastline=1;
        } else {
          die("%s: I/O error reading command file \"%s\": %s\n",
                  timestamp(),file,strerror(errno));
          fail=1;
          break;
        }
      }

      for(point=buf;*point;point++) {
        if(*point==' ' || *point=='\t')
          continue;
        else if(*point=='\n' || *point=='\r')
          break; /* end of line */
        else if(*point=='#')
          break;
        else {
          blankline=0;
          break;
        }
      }
    } /* End of blank line logic */

    if(fail)
      break;
    if(lastline)
      break;
    if(blankline)
      continue;

    /* Turn all end-of-line characters to null, to ensure the
       end-of-line is not in the command */
    for(point=buf;*point;point++) {
      if(*point=='\r' || *point=='\n') {
        *point='\0';
        break;
      }
    }

    /* Determine the command length and command */
    cmdlen=point-buf+1;
    icmdlen=cmdlen;
    if(cmdlen!=icmdlen) {
      die("%s: Command has length %llu which is too long to represent the length with "
              "a %d-byte integer.\n",timestamp(),cmdlen,sizeof(int));
      fail=1;
      break;
    }

    command=buf;

    /* Allocate space for the command list buffer */

    if(!all->commands) {
      /* No space allocated.  Allocate some now. */
      message(3,"%s: MPI Serial 0: allocate commands\n",timestamp());
      all->alloclen=7; /* must be >1 due to logic below */
      all->commands=(char**)loudalloc(sizeof(char*)*all->alloclen);
      all->cmdlen=(size_t*)loudalloc(sizeof(size_t)*all->alloclen);
      all->len=0;
      message(3,"%s: MPI Serial 0: allocated commands\n",timestamp());
    } else if(all->len==all->alloclen) {
      /* Not enough space allocated.  Reallocate to a larger space */
      all->alloclen=(all->alloclen-1)*2+1;
      message(3,"%s: MPI Serial 0: realloc commands from %llu to %llu\n",
              timestamp(),(unsigned long long)all->len,(unsigned long long)all->alloclen);
      if(all->alloclen<0) {
        die("Integer wrap-around in size_t.  Too many commands.\n");
        fail=1;
        break;
      }
      all->commands=(char**)realloc(all->commands,sizeof(char*)*all->alloclen);
      all->cmdlen=(size_t*)realloc(all->cmdlen,sizeof(size_t)*all->alloclen);
      if(!all->commands || !all->cmdlen) {
        die("Cannot allocate memory for command storage: %s\n",strerror(errno));
        fail=1;
        break;
      }
    }

    /* Store the command */
    message(3,"%s: MPI Serial 0: strdup command\n",timestamp());
    command=strndup(command,cmdlen);
    message(3,"%s: MPI Serial 0: store cmd[%d] len %d = %s\n",timestamp(),
            all->len,cmdlen,command);
    if(!command) {
      die("Cannot allocate memory for command storage: %s\n",strerror(errno));
      fail=1;
      break;
    }
    all->commands[all->len]=command;
    all->cmdlen[all->len]=cmdlen;
    all->len++;
  } // end of command file loop
  message(3,"%s: MPI Serial 0: finished reading commands.\n",timestamp());
  return 0;
}

////////////////////////////////////////////////////////////////////////

static void mpi_exit(int rc) {
  /* Exits with a non-zero exit code.  Tries to use return code rc.
     If rc is non-zero, and a specific return code cannot be used,
     then uses MPI_Abort to ensure a non-zero exit code. */

  rc=max_return_code(rc);
  message(3,"%s: MPI Serial %d: intended exit status %d\n",
          timestamp(),world_rank,rc);

  /* This final MPI_Barrier is necessary on some platforms: */
  message(3,"%s: MPI Serial %d: Final MPI_Barrier before exit...\n",
          timestamp(),world_rank);
  mpicall(2,MPI_Barrier(MPI_COMM_WORLD),"during final MPI_Barrier");

#ifdef MPI_ABORT_FOR_NONZERO_EXIT
  /* To exit with non-zero status, some platforms require MPI_Abort */
  if(world_rank==0 && rc!=0)   {
    message(-1,"%s: %s: maximum exit status was %d.  Calling MPI_Abort instead "
	    "of exiting to work around a problem in your MPI implementation.\n",
	    timestamp(),argv[0],rc);
    mpicall(2,MPI_Abort(MPI_COMM_WORLD,rc),"calling MPI_Abort");
  }
  /* We reach this line if the desired exit status is zero OR if this
   is not the root.  The root MPI_Aborts, which ensures non-zero exit
   status for the entire MPI_COMM_WORLD.*/
#endif /* MPI_ABORT_FOR_NONZERO_EXIT */
  
  mpicall(2,MPI_Finalize(),"finalizing MPI library");
  exit(rc);
}

////////////////////////////////////////////////////////////////////////

static int max_return_code(int rc) {
  /* Given the maximum return code from one rank, return the maximum
     across all ranks. */

  int mycode=rc, maxcode=255;

  /* This final MPI_Barrier is necessary on some platforms: */
  message(3,"%s: MPI Serial %d: MPI_Barrier after run ...\n",
          timestamp(),world_rank);
  mpicall(2,MPI_Barrier(MPI_COMM_WORLD),"MPI_Barrier after run");

  /* Determine the maximum return code */
  mycode=rc;
  maxcode=255;
  message(2,"%s: MPI Serial %d: get max return code (local=%d)\n",
	  timestamp(),world_rank,mycode);
  message(3,"%s: MPI Serial %d: MPI_Allreduce to get max error code...\n",
          timestamp(),world_rank);
  mpicall(2,MPI_Allreduce(&mycode,&maxcode,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD),
	  "using an mpi_allreduce to get the maximum return code at end of program");
  message(world_rank==0 ? 1 : 3,"%s: MPI Serial %d: max return code is %d\n",
	  timestamp(),world_rank,maxcode);

  return maxcode;
}

////////////////////////////////////////////////////////////////////////

static void initialize_environment(int * p_argc,char ***p_argv) {
  /* Start MPI, signal handlers, and global variables. */
  char buffer[300];

  /* Set an initial verbosity that does not require argv parsing */
  g_verbosity=scr_verbosity();

  /* Allow MPI to discard arguments */
  mpicall(2,MPI_Init( p_argc, p_argv ),"calling MPI_Init");
  g_mpi_inited=1; /* indicate to "die" that MPI_Init was called */

  hydra_workaround(); /* make sure stdin is not a directory */
  signal_workarounds(); /* when using LSF or OpenMPI, restore all signals */

  /* Enable MPI error handling so that we can give intelligent error messages */
  mpicall(4,MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN),"initializing mpi error handler");

  /* Set world_size and world_rank to avoid duplicate calls */
  mpicall(2,MPI_Comm_rank(MPI_COMM_WORLD,&world_rank),"determining mpi rank in MPI_COMM_WORLD");
  mpicall(2,MPI_Comm_size(MPI_COMM_WORLD,&world_size),"getting size of MPI_COMM_WORLD");
}

////////////////////////////////////////////////////////////////////////

void determine_settings(int i_argc,char **i_argv,char **p_command,
                        cmdfile **p_commands) {
  /* Reads arguments and environment variables to determine run mode
     and other settings.  Reads the MPMD command files or constructs
     the SPMD command. */

  cmdfile *commands=NULL; /* storage space for MPMD command list */

  /* Command file specified by the environment */
  const char *env_cmdfile=get_nonempty_env("SCR_CMDFILE");

  int no_run_mode=0; /* flag: 1=no run mode specified (used default) */

  /* Get settings that are specified by the environment */
  int env_run_mode=scr_pgmmodel();             /* $SCR_PGMMODEL */
  int env_verbosity=scr_verbosity();           /* $SCR_VERBOSITY */
  int env_immediate_exit=scr_immediate_exit(); /* $SCR_IMMEDIATE_EXIT */

  /* The opt_* are settings after command-line options */
  int opt_run_mode=env_run_mode;
  int opt_immediate_exit=env_immediate_exit;

  /* For argument parsing: */
  int opt=-1;
  int argc=i_argc;
  char **argv=i_argv;

  int iarg,ilen;

  g_verbosity=env_verbosity;

  /* Parse arguments to allow the command line to override the
     settings */
  while( (opt=getopt(argc,argv,":qvndMmjsahS:")) != -1 ) {
    switch(opt) {
      /* Verbosity settings: -q, -v, -n, -d */
    case 'q': g_verbosity=VERB_QUIET;   break;
    case 'v': g_verbosity=VERB_VERBOSE; break;
    case 'n': g_verbosity=VERB_NORMAL;  break;
    case 'd': g_verbosity=VERB_DEBUG;   break;

      /* Abort on error: -a (on) or -c (off) */
    case 'a': opt_immediate_exit=1;       break;
    case 'c': opt_immediate_exit=0;       break;

      /* Run mode: -M, -m, -j (synonym for -m), -s */
    case 'M': opt_run_mode=MODE_OLD_MPMD; break;
    case 'j':
    case 'm': opt_run_mode=MODE_MPMD;  break;
    case 's': opt_run_mode=MODE_SPMD;     break;

      /* Event loop sleep time: */
    case 'S':
      if(!optarg)
        usage("-S requires an argument\n");
      if(sscanf(optarg,"%lf",&g_event_loop_sleep_time)<1)
        usage("-S %s: cannot parse sleep time\n",optarg);
      if(g_event_loop_sleep_time<1e-5)
        g_event_loop_sleep_time=1e-5;
      if(g_event_loop_sleep_time>600)
        g_event_loop_sleep_time=600;
      message(world_rank ? 3 : 1,
              "%s: MPI Serial %d: Event loop sleep time set to %g seconds.\n",
              timestamp(),world_rank,g_event_loop_sleep_time);
      break;

      /* Help */
    case 'h':
      usage("HELP"); /* does not return */
    case '?':
    default:
      usage("Unknown option -%c\n",optopt); /* does not return */
    };
  };

  if(opt_run_mode==MODE_DEFAULT) {
    /* Special case: no run mode specified. */
    no_run_mode=1;
    if(optind<argc)
      /* Arguments were specified, so run mode is SPMD. */
      opt_run_mode=MODE_SPMD;
    else
      /* No arguments, so run_mode is MPMD. */
      opt_run_mode=MODE_MPMD;
  }

  /* Set global variables */
  g_run_mode=opt_run_mode;
  g_immediate_exit=opt_immediate_exit;

  *p_command=NULL;
  *p_commands=NULL;

  /* Construct command if we are in SPMD mode. */
  if(opt_run_mode==MODE_SPMD) {
    if(optind>=argc)
      usage("In SPMD mode, a command must be given.\n");
    *p_command=append_args(argc-optind,argv+optind);
    return;
  }

  /* Read command files if we are NOT in SPMD mode. */

  /* Read $SCR_CMDFILE if set and not the null string */
  if(env_cmdfile && *env_cmdfile)
    read_cmd_file(env_cmdfile,&commands);
  else if(optind>=argc && !no_run_mode)
    /* Special case.  No command file specified, but the run mode was
       explicitly set to an MPMD mode.  Use the default command
       file. */
    if(read_cmd_file(NULL,&commands)) {
      /* Default command file did not exist and no run mode was
         specified.  This happens if the user runs "mpiserial" with no
         or invalid arguments and no or invalid environment.  Send the
         default usage message. */
      usage("USAGE");
    }

  /* Read command files specified in the argument list, if any. */
  for(iarg=optind;iarg<argc;iarg++)
    read_cmd_file(argv[iarg],&commands);

  if(!commands)
    usage("In MPMD mode, at least one command file must be specified.\n");

  /* Root prints the command list info and checks validity */
  if(!world_rank) {
    message(1,"%s: MPI Serial %d: Have %d commands for %d ranks.\n",
            timestamp(),world_rank,commands->len,world_size);

    if(g_run_mode==MODE_OLD_MPMD) {
      /* In OLD_MPMD mode, the number of commands must equal the number of ranks */
      if(commands->len<world_size)
        die("Not enough commands: only found %d of %d\n",commands->len,world_size);
      if(commands->len>world_size)
        die("Too many commands: found %d instead of %d\n",commands->len,world_size);
    }

    if(g_verbosity>=3)
      /* In debug mode, print all commands */
      for(ilen=0;ilen<commands->len;ilen++)
        message(3,"%s: MPI Serial %d: cmd[%d] (len %d) = \"%s\"\n",
                timestamp(),world_rank,ilen,commands->cmdlen[ilen],
                commands->commands[ilen]);
  }

  /* Store the commands for later execution */
  *p_commands=commands;
}

////////////////////////////////////////////////////////////////////////

void forward_environment() {
  /* Sets environment variables to settings parsed from command line
     or elsewhere.  This lets child processes see the
     configuration.  */
  char buf[300];
  const char *model;

  /* g_run_mode => $SCR_PGMMODEL */
  switch(g_run_mode) {
  case MODE_OLD_MPMD: model="OLD_MPMD"; break;
  case MODE_SPMD:     model="SPMD";     break;
  default:            model="MPMD";
  }
  setenv("SCR_PGMMODEL",model,1);

  /* g_verbosity => $SCR_VERBOSITY */
  snprintf(buf,299,"%d",g_verbosity);
  setenv("SCR_VERBOSITY",buf,1);

  /* g_immediate_exit => $SCR_IMMEDIATE_EXIT */
  if(g_immediate_exit)
    setenv("SCR_IMMEDIATE_EXIT","1",1);
  else
    setenv("SCR_IMMEDIATE_EXIT","0",1);

  /* world_rank => $SCR_COMM_RANK */
  sprintf(buf,"%d",world_rank);
  setenv("SCR_COMM_RANK",buf,1);

  /* world_size => $SCR_COMM_SIZE */
  sprintf(buf,"%d",world_size);
  setenv("SCR_COMM_SIZE",buf,1);

  /* MPISERIAL_VERSION => $SCR_MPISERIAL_VERSION */
  setenv("SCR_MPISERIAL_VERSION",MPISERIAL_VERSION,1);

  /* g_event_loop_sleep_time => $SCR_ELOOP_SLEEP_USEC */
  sprintf(buf,"%llu",
          (unsigned long long)(1e6*g_event_loop_sleep_time));
  setenv("SCR_ELOOP_SLEEP_USEC",buf,1);
}

////////////////////////////////////////////////////////////////////////

int main(int argc,char **argv) {
  unsigned int rc=255;
  char *command=NULL;
  cmdfile *commands=NULL;

  initialize_environment(&argc,&argv);

  determine_settings(argc,argv,&command,&commands);

  forward_environment();

  if(g_run_mode==MODE_OLD_MPMD)
    scatter_commands(commands,0,&command);

  if(g_run_mode==MODE_MPMD)
    rc=event_loop(NULL,commands);
  else
    rc=event_loop(command,NULL);

  /* Ensure child processes are wait()ed for.  Set SIGCHLD to the
     special value SIG_IGN.  On Linux, this arranges for the kernel to
     pretend this process calls wait() on all children.  This is
     necessary to avoid zombie processes, which can cause some MPI
     implementations' launchers to hang. */
  signal(SIGCHLD,SIG_IGN);
 
  mpi_exit(rc); /* never returns */
  return 0; /* to make some compilers happy */
}
