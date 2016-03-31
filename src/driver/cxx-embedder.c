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




#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "cxx-driver.h"
#include "cxx-driver-utils.h"
#include "cxx-driver-build-info.h"
#include "cxx-macros.h"
#include "cxx-parameters.h"
#include "cxx-embed.h"
#include "cxx-utils.h"
#include "filename.h"

#define HELP_STRING \
"usage: %s [options] --compiler-profile=name -o file.o --profile=name file.o [--profile=name file.o...]\n" \
"\n" \
"This application expects a list of profiles followed by a set of\n" \
"to be embedded under that profile\n" \
"\n" \
"Options:\n" \
"   --verbose           Shows what the embedder is doing\n" \
"\n"

enum {
    OPTION_EMBED_VERSION = 1024,
    OPTION_EMBED_VERBOSE,
    OPTION_EMBED_PROFILE,
    OPTION_EMBED_COMPILER_PROFILE,
};

// It mimics getopt
#define SHORT_OPTIONS_STRING "ho:"
// This one mimics getopt_long but with one less field (the third one is not given)
struct command_line_long_options command_line_long_options[] =
{
    {"help",        CLP_NO_ARGUMENT, 'h' },
    {"version",     CLP_NO_ARGUMENT, OPTION_EMBED_VERSION },
    {"output",      CLP_REQUIRED_ARGUMENT, 'o' },
    {"profile",     CLP_REQUIRED_ARGUMENT, OPTION_EMBED_PROFILE },
    {"compiler-profile", CLP_REQUIRED_ARGUMENT, OPTION_EMBED_COMPILER_PROFILE },
    {"verbose",     CLP_NO_ARGUMENT, OPTION_EMBED_VERBOSE },
};

#define MAX_EMBED_FILES 64

embedded_file_t embed_files[MAX_EMBED_FILES];

static void print_help(void)
{
    fprintf(stderr, HELP_STRING, compilation_process.exec_basename);
}

static void print_help_and_error(const char* error_msg)
{
    print_help();

    fprintf(stderr, "Error: %s\n", error_msg);
    exit(EXIT_FAILURE);
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

static volatile char in_cleanup_routine = 0;

static void cleanup_routine(void)
{
    in_cleanup_routine = 1;
    temporal_files_cleanup();
    in_cleanup_routine = 0;
}

// alternate signal stack
#if !defined(WIN32_BUILD)
static char *_alternate_signal_stack;
#endif

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

static void tool_initialization(int argc, const char* argv[])
{
    atexit(cleanup_routine);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
#if !defined(__CYGWIN__)
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
#endif

    // Program signals
    struct sigaction terminating_sigaction;
    memset(&terminating_sigaction, 0, sizeof(terminating_sigaction));

    terminating_sigaction.sa_handler = terminating_signal_handler;
    // Use alternate stack and we want the signal be reset when it happens
    terminating_sigaction.sa_flags = SA_RESETHAND;
#if !defined(__CYGWIN__)
    terminating_sigaction.sa_flags |= SA_ONSTACK;
#endif
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

int main(int argc, char *argv[])
{
    tool_initialization(argc, (const char**)argv);
    initialize_default_values();

    // Parse parameters (ignore argv[0])
    int parameter_index = 1;

    struct command_line_parameter_t parameter_info;

    const char* compiler_profile = NULL;
    const char* current_profile = NULL;
    const char* output_filename = NULL;

    int num_embed_files = 0;

    while( command_line_get_next_parameter(&parameter_index,
                &parameter_info,
                SHORT_OPTIONS_STRING,
                command_line_long_options,
                argc, (const char**)argv))
    {
        if (parameter_info.flag == CLP_INVALID)
        {
            print_help_and_error("Invalid parameter");
        }
        else if (parameter_info.flag == CLP_PLAIN_PARAMETER)
        {
            if (num_embed_files == MAX_EMBED_FILES)
            {
                char c[256];
                snprintf(c, 255, "Too many files to embed at once. Maximum is %d\n",
                        MAX_EMBED_FILES);
                c[255] = '\0';
                print_help_and_error(c);
            }

            if (current_profile == NULL)
            {
                print_help_and_error("Specify a profile first using --profile");
            }

            embed_files[num_embed_files].filename = parameter_info.argument;
            embed_files[num_embed_files].profile_name = current_profile;

            num_embed_files++;
        }
        else 
        {
            switch (parameter_info.value)
            {
                case 'h':
                    {
                        print_help();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case 'o':
                    {
                        if (output_filename != NULL)
                        {
                            print_help_and_error("Ouput specified twice");
                        }

                        output_filename = xstrdup(parameter_info.argument);
                        break;
                    }
                case OPTION_EMBED_VERSION:
                    {
                        printf("%s - Embedder tool for " PACKAGE " " VERSION " (" MCXX_BUILD_VERSION ")\n",
                                compilation_process.exec_basename);
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_EMBED_PROFILE:
                    {
                        current_profile = xstrdup(parameter_info.argument);
                        break;
                    }
                case OPTION_EMBED_VERBOSE:
                    {
                        CURRENT_CONFIGURATION->verbose = 1;
                        break;
                    }
                case OPTION_EMBED_COMPILER_PROFILE:
                    {
                        if (compiler_profile != NULL)
                        {
                            print_help_and_error("Compiler profile specified twice");
                        }
                        compiler_profile = xstrdup(parameter_info.argument);
                        break;
                    }
                default:
                    {
                        internal_error("Unhandled unknown option", 0);
                        break;
                    }
            }
        }
    }

    if (output_filename == NULL
            && compiler_profile == NULL
            && num_embed_files == 0)
    {
        print_help_and_error("No arguments passed");
    }

    if (output_filename == NULL)
    {
        print_help_and_error("Output not specified");
    }

    if (num_embed_files == 0)
    {
        print_help_and_error("No files to embed have been specified");
    }

    if (compiler_profile == NULL)
    {
        print_help_and_error("No compiler profile specified");
    }

    // Now embed

    // Check if output filename exists, otherwise create an empty one
    struct stat s;

    if (stat(output_filename, &s) != 0)
    {
        if (errno != ENOENT)
        {
            fatal_error("Cannot stat output file '%s'. %s\n", output_filename, strerror(errno));
        }

        // FIXME - Assuming the compiler profile allows C
        temporal_file_t temp_file = new_temporal_file_extension(".c");

        const char* compiler_args[] =
        {
            "-c", 
            "-o", output_filename, 
            temp_file->name, NULL
        };

        if (execute_program(compiler_profile, compiler_args) != 0)
        {
            fatal_error("Invocation of the compiler to generate output file '%s' failed", 
                    output_filename);
        }
    }

    if (embed_to_file(output_filename, num_embed_files, embed_files) == 0)
    {
        fatal_error("Embedding into '%s' failed\n", output_filename);
    }

    return 0;
}
