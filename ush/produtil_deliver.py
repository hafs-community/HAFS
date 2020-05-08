#! /usr/bin/env python3

##@namespace produtil_deliver
# A test program for the produtil.fileop.deliver_file() function.  
#
# This program provides access to the produtil.fileop.deliver_file()
# function to shell programs.  It was originally used to test that
# function, but it can also be used in scripts for other purposes.
# It is called as follows:
# @code[.sh]
#  produtil_deliver.py [options] [--] fromfile tofile
# @endcode
#
# It will copy the fromfile to the tofile file or directory without
# leaving the destination file in an incomplete state.  This is
# accomplished by copying to a temporary file in the same directory
# and renaming that file to the final filename.  If requested, a
# simple move (like "mv") is done if possible.
#
# Options:
# * -v --- verbose; use more -v for higher verbosity
# * -m --- move the file instead of copying it, if possible.  This
#          may not be possible if the source and destination are in
#          different filesystems or filesets.
# * -c --- verify: check to see if the temporary and original file
#          have the same contents before renaming to the destination
# * -b bytes --- copy this many bytes at a time
# * -t template --- template to prefix to tempoarary filenames
# * -k --- keep the temporary file if the rename fails
# * -- (two hyphens) --- Terminate option processing.  This is
#          needed for "misbehaving" filenames that begin with a
#          hyphen


from produtil.fileop import *
from produtil.sigsafety import *
import sys,logging,getopt

##@var minblock 
# Minimum required block size in bytes when -b is specified.
minblock=4096

def usage(s=None):
    """!Prints a usage message to stderr
    @param s ignored"""
    print('''Format: %s [options] [--] from to

This is the test program for produtil.fileop.deliver_file.  It will
copy or move file "from" to file or directory "to".  This is similar
to the cp and commands, but the "to" file will never be seen in an
incomplete state.  This is accomplished either via a "mv" or by
copying to a temporary file and "mv"ing that file to the destination.
All "mv" operations are within one filesystem, to ensure the
operations are unit operations.  Be sure to use the -m option if you
do not need the original "from" file any more to allow a simple "mv".

Options:
  -v           verbose (multiple -v for higher verbosity)
  -m           use "mv" to deliver if "from" and "to" are in the same filesystem
               Note: if that is done, it will cause "from" to cease to exist.
  -c           verify: check to see that the temporary file has the same contents as
               "from" before moving it to the destination
  -b bytes     copy "bytes" bytes at a time (must be >= %d)
  -t template  prefix to prepend to temporary filenames
               Can only contain: a-z A-Z 0-9 _ . -
  -k           Keep the temporary file if the delivery fails.
  --           Terminate option processing.  Use this if you need to deliver a
               file whose name begins with a dash (-)''' % (sys.argv[0],minblock), file=sys.stderr)
    if s is not None:
        print('SCRIPT IS EXITING DUE TO INCORRECT ARGUMENTS: %s'%(str(s),minblock), file=sys.stderr)

def main():
    """!Main program.  Parses arguments, delivers file."""
    install_handlers() # raise exception on SIGTERM or SIGHUP
    
    dash_v_count=0
    kwargs={}
    try:
        (optarg,rest) = getopt.getopt(sys.argv[1:],'kmcrvb:t:')
    except getopt.GetoptError as e:
        usage(str(e))
    
    if(len(rest)!=2):
        usage('You must provide exactly two non-option arguments: the source and destination (you gave %d args).'%(len(rest),))
    (infile,outfile)=rest
    
    for opt,arg in optarg:
        if(opt=='-m'):
            kwargs['keep']=False
        elif(opt=='-c'):
            kwargs['verify']=True
        elif(opt=='-b'):
            try:
                bs=float(arg)
                if not (bs>=minblock):
                    usage('Minimum allowed block size is %d'%(minblock,))
                kwargs['blocksize']=bs
            except ValueError as e:
                usage(str(e))
        elif(opt=='-v'):
            dash_v_count+=1
        elif(opt=='-t'):
            if(re.search('[^a-zA-Z0-9_-.]',arg)):
                usage('The temp prefix must only contain alphanumerics, underscore (_), dash (-) and period (.).')
            kwargs['tempprefix']=arg
        elif(opt=='-k'):
            kwargs['removefailed']=False
        else:
            usage('Unknown option %s'%(opt,))
    
    level=logging.WARNING
    if dash_v_count>1:
        level=logging.DEBUG
    elif dash_v_count>0:
        level=logging.INFO
    logging.basicConfig(level=level)
    
    try:
        deliver_file(infile,outfile,logger=logging.getLogger(),**kwargs)
        sys.exit(0)
    except DeliveryFailed as d:
        logging.critical(str(d))
        sys.exit(1)
    except (IOError,OSError) as e:
        logging.critical('%s delivery: %s' % (infile,str(e)))
        sys.exit(1)
    except (KeyboardInterrupt,CaughtSignal) as e:
        logging.critical('%s delivery: abort due to signal: %s'%(infile,str(e)))
        sys.exit(1)

if __name__=='__main__': main()
