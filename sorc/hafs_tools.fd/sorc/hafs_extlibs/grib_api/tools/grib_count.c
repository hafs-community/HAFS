/*
 * Copyright 2005-2018 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
 * virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
 */

#include "grib_api_internal.h"

static void usage(const char* prog)
{
    printf("usage: %s [-v] infile1 infile2 ... \n",prog);
    exit(1);
}

static int count_messages(FILE* in,unsigned long *count)
{
    void* mesg=NULL;
    size_t size=0;
    off_t offset=0;
    int err=GRIB_SUCCESS;
    grib_context* c=grib_context_get_default();

    if (!in) return 1;

    while ( (mesg=wmo_read_any_from_file_malloc ( in,0,  &size,&offset,&err))!=NULL && err==GRIB_SUCCESS) {
        grib_context_free(c,mesg);
        (*count)++;
    }

    if (err==GRIB_END_OF_FILE) err=GRIB_SUCCESS;

    return err;
}

int main(int argc,char* argv[])
{
    FILE* infh = NULL;
    char* filename;
    int i, verbose=0, num_files_processed = 0;
    int err=0;
    unsigned long count_total=0, count_curr=0;

    if (argc <2) usage(argv[0]);

    count_total=0;
    for (i=1;i<argc;i++) {
        if (strcmp(argv[i], "-v")==0) {
            verbose = 1;
            if (argc <3) usage(argv[0]);
            continue;
        }
        filename=argv[i];
        infh=fopen(filename,"r");
        if (!infh) {
            perror(filename);
            exit(1);
        }

        count_curr=0;
        err=count_messages(infh,&count_curr);
        if (err) {
            fprintf(stderr,"Invalid grib message(s) found in %s\n", filename);
            exit(err);
#ifdef DONT_EXIT_ON_BAD_APPLE
            /* If we did not want to fail but warn and continue */
            fclose(infh);
            continue;
#endif
        }
        ++num_files_processed;
        if (verbose) printf ("%7lu %s\n", count_curr, filename);
        count_total += count_curr;

        fclose(infh);
    }
    if (verbose) {
        if (num_files_processed > 1) {
            printf("%7lu %s\n", count_total, "total");
        }
    } else {
        printf("%lu\n", count_total);
    }

    return 0;
}
