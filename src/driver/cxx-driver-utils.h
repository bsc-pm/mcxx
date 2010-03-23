/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#ifndef CXX_DRIVERUTILS_H
#define CXX_DRIVERUTILS_H

#include <stdio.h>
#include <sys/time.h>
#include <time.h>

// Cygwin is so unix...
#if defined(WIN32_BUILD) && !defined(__CYGWIN__)
  #define DIR_SEPARATOR "\\"
#else
  #define DIR_SEPARATOR "/"
#endif

#ifndef PKGLIB_INSTALL
  #define CONFIG_RELATIVE_PATH DIR_SEPARATOR".."DIR_SEPARATOR"share"DIR_SEPARATOR"mcxx"DIR_SEPARATOR"config.mcxx"
  #define DIR_CONFIG_RELATIVE_PATH DIR_SEPARATOR".."DIR_SEPARATOR"share"DIR_SEPARATOR"mcxx"DIR_SEPARATOR"config.d"
#else
  #define CONFIG_RELATIVE_PATH DIR_SEPARATOR".."DIR_SEPARATOR".."DIR_SEPARATOR"share"DIR_SEPARATOR"mcxx"DIR_SEPARATOR"config.mcxx"
  #define DIR_CONFIG_RELATIVE_PATH DIR_SEPARATOR".."DIR_SEPARATOR".."DIR_SEPARATOR"share"DIR_SEPARATOR"mcxx"DIR_SEPARATOR"config.d"
#endif

// Temporal handling routines
typedef struct 
{
    // FILE* file;
    const char* name;
    char is_dir;
    char is_temporary;
}* temporal_file_t;

// Gives you a new temporal file that will be removed when
// finishing the program
temporal_file_t new_temporal_file(void);

// The same but a directory that will be wiped at the end of the program
temporal_file_t new_temporal_dir(void);

temporal_file_t new_temporal_file_extension(const char* extension);

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

// Like rename but works accross filesystems
char move_file(const char* source, const char* dest);

// This function adds a file for cleanup for non temporary ones
void mark_file_for_cleanup(const char* name);
void mark_dir_for_cleanup(const char* name);

// Find the path where the application runs
const char* find_home(void);

#endif // CXX_DRIVERUTILS_H
