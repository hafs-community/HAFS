#include <stdio.h>
#include <string.h>

void dumpline(void);

int main(int argc,char **argv) {
  const size_t n=200; // NOTE: must be one more than scanf format
  char symbol[n],symtype[n],c;
  int iline=0;
  long long defval;
  while(!feof(stdin)) {
    iline+=1;
    c=getc(stdin);
    if(c!='@') {
      //fprintf(stderr,"Line %d: first char is '%c' so dump line\n",iline,c);
      ungetc(c,stdin);
      dumpline();
    } else {
      //fprintf(stderr,"Line %d: first char is '%c' so parse line\n",iline,c);
      symbol[0]=0;
      defval=0;
      if(3!=scanf("%199s %199s %lld\n",symtype,symbol,&defval)) {
        //fprintf(stderr,"ERROR: line %d: Cannot parse line.\n",iline);
        continue;
      }
      if(!strcmp(symtype,"int64_t")) {
        printf(
        "#ifdef %s\n"
        "  printf(\"integer(kind=c_int64_t), parameter :: %-20s = %%lld\\n\",\n"
        "         (long long)%14s);\n"
        "  printf(\"logical                , parameter :: have_%-15s = .true.\\n\");\n"
        "#else\n"
        "  printf(\"integer(kind=c_int64_t), parameter :: %20s = %lld ! missing\\n\");\n"
        "  printf(\"logical                , parameter :: have_%-15s = .false.\\n\");\n"
        "#endif\n",
        symbol,symbol,symbol,symbol,symbol,defval,symbol);
      } else {

        //fprintf(stderr,"ERROR: line %d: Unknown type \"%s\" (must be one of: "
        //        "int64_t)\n",iline,symtype);
        continue;
      }

    }
  }
}

void dumpline(void) {
  // Read one line from stdin, write to stdout
  char s[20],*t;
  size_t n=0,L=0;
  do {
    if(!fgets(s,20,stdin))
      break;
    fputs(s,stdout);
    L=strlen(s);
    n+=L;      
  } while(s[L-1]!='\n' && s[L-1]!='\r');
  //fprintf(stderr,"Dumped %lld bytes.\n",(long long)n);
}
