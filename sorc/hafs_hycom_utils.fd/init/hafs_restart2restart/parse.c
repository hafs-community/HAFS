/*
C**********
 *
 *  @(#) parse.c
 *
 *  This is the C source code for the FORTRAN callable routine 
 *  called PARSE.  It provides list-directed, internal read
 *  capability for those FORTRAN compilers that don't support, 
 *  for example, read(string,*) x,i,str
 *
 *  Calling sequence:
 *    CALL PARSE(string,isize, most, 
 *   +   nvals, rarray, iarray, carray, jsize) 
 * 
 *  Variables (all passed by reference)
 *    string : The FORTRAN character variable to be parsed
 *    isize  : The length of the string variable 
 *    most   : The maximum number of values to parse from the string
 *    nvals  : (output) The number of values parsed.
 *    rarray : (output) An array containing floating point values
 *    iarray : (output) An array containing integer values
 *    carray : (output) An array containing character fields
 *    jsize  : The size of each element in the carray array
 *
 *    B. Maloy, Planning Systems, Inc.
 *
C**********
 */

#include <stdlib.h>
#include <string.h>
/*
 *  On the CRAY, FORTRAN callable routines are expected to be
 *  all caps, with no trailing underscrore.
 *  Under AIX, FORTRAN callable routines are expected to be
 *  lower case, with no trailing underscrore.
 *  Everywhere else, it seems, the name should be lower case and
 *  have a trailing underscore. 
 */
#ifdef CRAY
#define MODULE_NAME PARSE
#else
#ifdef AIX
#define MODULE_NAME parse
#else
#define MODULE_NAME parse_
#endif
#endif

double strtod();

void MODULE_NAME (s, len, size, num, rvals, ivals, cvals, clen)
char *s;       /* The string to be parsed                    */
int  *size;    /* The size of the arrays (input)             */
int  *len;     /* The "FORTRAN length" of the string         */
int  *num;     /* The number of values parsed (output)       */
double *rvals; /* An array of floating point values (output) */
int  *ivals;   /* An array of integer values (output)        */
char *cvals;   /* An array of characters (output)            */
int  *clen;    /* The size of each element in array cvals[]  */
{

char *SCCS="@(#)parse.c -- version 0.02b of 1994.09.15";
char *delim = " ,\t";
#define MAX_LEN 256 
char buffer[MAX_LEN+1];
int  i,j,n;
double *rp;
int  *ip;
char *cp, *p;

/*
#ifdef sun
 *  Just because this is a Sun, don't assume it's using SunOS 4.1.x
 *  Yes, do assume, this is a solaris compilation, SDR 06/96
 * 
#ifdef NO_FIX_LIBC
#else
 fix_libc_();   SunOS 4.1 buggy libc (in mixed language apps)
#endif
#endif
*/

 rp = rvals;
 ip = ivals;
 cp = cvals;

 i = *len;
 if(i>MAX_LEN) i = MAX_LEN; 

 strncpy(buffer,s,i);
 buffer[i] = '\0';

 p = strtok(buffer,delim);
 n = 0;
 while(p && (n<*size) ) {
   *rp++ = strtod(p, (char **)NULL); 
   *ip++ = strtol(p, (char **)NULL, 10);

   i = strlen(p);
   if(i > *clen) i = *clen;

   if(i) {
      strncpy(cp,p,i);
      for(j=i; j<*clen; j++) {
        cp[j] = ' ';
      }
   }
   cp += *clen; 
   n++;
   p = strtok((char *)0,delim);
 }
 *num = n;
}

