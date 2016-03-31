/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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



#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "config.h"
#include "prescanner-driver.h"
#include "prescanner-process.h"
#include "cxx-driver-utils.h"
#include "cxx-utils.h"
#include "filename.h"

#define GETOPT_STRING "o:w:hdmapI:r:ql"

#define HELP_STRING \
"Usage: " PACKAGE " [options] -o file file\n" \
"Options: \n" \
"  -m       \tInstead of appending continuated lines\n" \
"           \tuse Fortran 95 continuation marks\n" \
"  -a       \tDo not pad unended strings prior to \n" \
"           \tcontinuated lines with spaces\n" \
"  -d       \tEnable debugging\n" \
"  -o file  \tPlace the output into <file>\n" \
"  -w width \tColumn width\n" \
"  -p       \tEnable OpenMP 2.5 preprocessing\n" \
"  -I <dir> \tConsiders directory <dir> when looking\n" \
"           \tfor included files\n" \
"  -r <dir> \tRegenerates headers in directory <dir>.\n" \
"           \tIt will be also searched for includes\n" \
"  -q       \tQuiet mode\n" \
"  -l       \tAdd #line marks\n" \
"\n"

static prescanner_t prescanner;

static void tool_initialization(int argc, const char* argv[]);
static void initialize_default_values(void);
static void show_help(void);
static void parse_parameters(int argc, char* argv[]);

int main(int argc, char* argv[])
{
    tool_initialization(argc, (const char**)argv);
    initialize_default_values();

    parse_parameters(argc, argv);
    
    if (!prescanner.quiet)
    {
        fprintf(stderr, PACKAGE " Prescanner - Version " VERSION "\n");
    }

    fortran_prescanner_run(&prescanner);
    return 0;
}

static void show_help(void)
{
    fprintf(stderr, HELP_STRING);
}

static compilation_configuration_t minimal_default_configuration;
static void initialize_default_values(void)
{
    // Initialize here all default values
    compilation_process.config_file = strappend(compilation_process.home_directory, CONFIG_RELATIVE_PATH);
    compilation_process.config_dir = strappend(compilation_process.home_directory, DIR_CONFIG_RELATIVE_PATH);
    compilation_process.num_translation_units = 0;

    // The minimal default configuration
    memset(&minimal_default_configuration, 0, sizeof(minimal_default_configuration));
    SET_CURRENT_CONFIGURATION(&minimal_default_configuration);
}

static char in_cleanup_routine = 0;
static void cleanup_routine(void)
{
    in_cleanup_routine = 1;
    temporal_files_cleanup();
    in_cleanup_routine = 0;
}

debug_options_t debug_options;

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig)
{
    fprintf(stderr, "Signal handler called (signal=%d). Exiting.\n", sig);

    if (CURRENT_CONFIGURATION != NULL
            && !debug_options.do_not_run_gdb
            // Do not call the debugger for Ctrl-C
            && sig != SIGINT)
        run_gdb();

    if (!in_cleanup_routine)
        cleanup_routine();

    raise(sig);
}
#endif

// alternate signal stack
#if !defined(WIN32_BUILD)
static char *_alternate_signal_stack;
#endif

static void tool_initialization(int argc, const char* argv[])
{
    atexit(cleanup_routine);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    // Define alternate stack
    stack_t alternate_stack;

    // Allocate a maximum of 1 Mbyte or more if MINSIGSTKSZ was
    // bigger than that (this is unlikely)
    int allocated_size = 1024 * 1024;
    if (MINSIGSTKSZ > 1024*1024)
    {
        allocated_size = MINSIGSTKSZ;
    }

    _alternate_signal_stack = xmalloc(allocated_size);

    alternate_stack.ss_flags = 0;
    alternate_stack.ss_size = allocated_size;
    alternate_stack.ss_sp = (void*)_alternate_signal_stack;

    if (alternate_stack.ss_sp == 0
            || sigaltstack(&alternate_stack, /* oss */ NULL) != 0)
    {
        fatal_error("Setting alternate signal stack failed (%s)\n",
                strerror(errno));
    }

    // Program signals
    struct sigaction terminating_sigaction;
    memset(&terminating_sigaction, 0, sizeof(terminating_sigaction));

    terminating_sigaction.sa_handler = terminating_signal_handler;
    // Use alternate stack and we want the signal be reset when it happens
    terminating_sigaction.sa_flags = SA_RESETHAND | SA_ONSTACK;
    // Block all blockable signals while handling the termination
    sigfillset(&terminating_sigaction.sa_mask);

    int result = 0;
    result |= sigaction(SIGSEGV, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGQUIT, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGINT,  &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGTERM, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGABRT, &terminating_sigaction, /* old_sigaction */ NULL);
    
    if (result != 0)
    {
        fatal_error("Signal programming failed with '%s'\n", strerror(errno));
    }
#endif

    memset(&compilation_process, 0, sizeof(compilation_process));
    compilation_process.argc = argc;
    compilation_process.argv = (const char**)argv;
    compilation_process.exec_basename = give_basename(argv[0]);

    // Find my own directory
    compilation_process.home_directory = find_home(argv[0]);
}

static void parse_parameters(int argc, char* argv[])
{
    memset(&prescanner, 0, sizeof(prescanner));
    // By default
    prescanner.width = 72;
    prescanner.append = 1;
    prescanner.openmp_processing = 0;
    prescanner.pad_strings = 1;
    prescanner.output_filename = NULL;
    prescanner.num_include_directories = 1;
    prescanner.include_directories = NEW0(const char*);
    prescanner.include_directories[0] = xstrdup(".");
    prescanner.output_include_directory = NULL;

    char c;
    char error = 0;
    while ((c = getopt(argc, argv, GETOPT_STRING)) != (char) -1)
    {
        switch (c)
        {
            case 'o' :
                {
                    prescanner.output_filename = xstrdup(optarg);
                    break;
                }
            case 'w' :
                {
                    prescanner.width = atoi(optarg);
                    if (prescanner.width == 0)
                    {
                        fprintf(stderr, "Column width cannot be 0\n");
                        error = 1;
                    }
                    break;
                }
            case 'd' :
                {
                    debug_options.enable_debug_code = 1;
                    break;
                }
            case 'a' :
                {
                    prescanner.pad_strings = 0;
                    break;
                }
            case 'm' :
                {
                    prescanner.append = 0;
                    break;
                }
            case 'p' :
                {
                    prescanner.openmp_processing = 1;
                    break;
                }
            case 'h' :
                {
                    show_help();
                    exit(EXIT_SUCCESS);
                    break;
                }
            case 'I' :
                {
                    char* directory = xstrdup(optarg);
                    prescanner.num_include_directories++;
                    prescanner.include_directories = NEW_REALLOC(
                            const char*,
                            prescanner.include_directories, 
                            prescanner.num_include_directories);
                    prescanner.include_directories[prescanner.num_include_directories-1] = directory;

                    break;
                }
            case 'r' :
                {
                    if (prescanner.output_include_directory != NULL)
                    {
                        fprintf(stderr, "Do not specify -r more than once\n");
                        show_help();
                        exit(EXIT_FAILURE);
                        break;
                    }

                    char* directory = xstrdup(optarg);
                    prescanner.output_include_directory = directory;

                    prescanner.num_include_directories++;
                    prescanner.include_directories = NEW_REALLOC(
                            const char*,
                            prescanner.include_directories, 
                            prescanner.num_include_directories);
                    prescanner.include_directories[prescanner.num_include_directories-1] = directory;
                    break;
                }
            case 'q':
                {
                    prescanner.quiet = 1;
                    break;
                }
            case 'l':
                {
                    prescanner.line_marks = 1;
                    break;
                }
            case '?' :
                {
                    error = 1;
                    break;
                }
            default :
                error = 1;
                break;
        }
    }

    if (prescanner.output_filename == NULL)
    {
        fprintf(stderr, "You must specify an output file\n");
        error = 1;
    }

    if (argc == optind)
    {
        fprintf(stderr, "You must specify an input file.\n");
        error = 1;
    }

    if ((argc - optind) > 1)
    {
        fprintf(stderr, "Too many parameters.");
        error = 1;
    }

    if (error == 1)
    {
        show_help();
        exit(EXIT_FAILURE);
    }

    prescanner.input_filename = xstrdup(argv[optind]);
}
