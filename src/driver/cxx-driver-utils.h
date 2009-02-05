#ifndef CXX_DRIVERUTILS_H
#define CXX_DRIVERUTILS_H

#include <stdio.h>
#include <sys/time.h>
#include <time.h>

// Temporal handling routines
typedef struct 
{
    // FILE* file;
    const char* name;
}* temporal_file_t;

// Gives you a new temporal file that will be removed when
// finishing the program
temporal_file_t new_temporal_file(void);

// Routine that does the cleanup. Can be atexit-registered
// or used discretionally inside the program. Every temporal
// file is closed and erased.
void temporal_files_cleanup(void);

const char* get_extension_filename(const char* filename);

int execute_program(const char* program_name, const char** arguments);
int execute_program_flags(const char* program_name, const char** arguments, 
        const char *stdout_f, const char *stderr_f);

// char** routines
int count_null_ended_array(void** v);

typedef struct
{
  struct timeval start;
  struct timeval end;
  double elapsed_time;
} timing_t;

void timing_start(timing_t* t);
void timing_end(timing_t* t);
int timing_seconds(const timing_t* t);
int timing_microseconds(const timing_t* t);
double timing_elapsed(const timing_t* t);

void run_gdb(void);

#endif // CXX_DRIVERUTILS_H
