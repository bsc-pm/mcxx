/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#include <unistd.h>
#include <string.h>
#include <errno.h>
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
  #include <sys/wait.h>
  #include <libgen.h>
  #include <limits.h>
#else
  #include <windows.h>
#endif
#include <sys/stat.h>

#include "cxx-driver.h"
#include "cxx-driver-utils.h"
#include "cxx-utils.h"
#include "uniquestr.h"
#include "filename.h"

#ifndef HAVE_MKDTEMP
// For those systems lacking mkdtemp
extern char* mkdtemp (char *xtemplate);
#endif

typedef struct temporal_file_list_tag
{
    temporal_file_t info;
    struct temporal_file_list_tag* next;
} temporal_file_list_value_t, *temporal_file_list_t;

static temporal_file_list_t temporal_file_list = NULL;

void temporal_files_cleanup(void)
{
    temporal_file_list_t iter = temporal_file_list;

    while (iter != NULL)
    {
        if (iter->info == NULL
                || (!iter->info->is_temporary
                    && CURRENT_CONFIGURATION->keep_files)
                || (iter->info->is_temporary
                    && CURRENT_CONFIGURATION->keep_temporaries))
        {
            // Skip this file
        }
        else
        {
            if (!iter->info->is_dir)
            {
                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "Removing %s filename '%s'\n", 
                            iter->info->is_temporary ? "temporal" : "intermediate",
                            iter->info->name);
                }
                if (remove(iter->info->name) != 0
                        && errno != ENOENT)
                {
                    fprintf(stderr, "Error while removing filename: '%s'\n", strerror(errno));
                }
            }
            else
            {
                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "Removing %s directory '%s'\n", 
                            iter->info->is_temporary ? "temporal" : "intermediate",
                            iter->info->name);
                }
                // FIXME - We really should improve this...
                char rm_fr[256];
                snprintf(rm_fr, 255, "rm -fr \"%s\"", iter->info->name);
                rm_fr[255] = '\0';
                int ret = system(rm_fr);

                if (ret == -1)
                {
                    fatal_error("Execution of 'rm -fr' failed\n");
                }
            }

        }

        temporal_file_list_t prev = iter;
        iter = iter->next;
        DELETE(prev->info);
        DELETE(prev);
    }

    temporal_file_list = NULL;
}

static char name_is_in_temporal_files(const char* name)
{
    temporal_file_list_t it = temporal_file_list;

    while (it != NULL)
    {
        if (strcmp(it->info->name, name) == 0)
            return 1;
        it = it->next;
    }

    return 0;
}

static temporal_file_t add_to_list_of_temporal_files(const char* name, char is_temporary, char is_dir)
{
    temporal_file_t result = NEW0(temporal_file_value_t);
    result->name = uniquestr(name);
    result->is_temporary = is_temporary;
    result->is_dir = is_dir;

    temporal_file_list_t new_file_element = NEW0(temporal_file_list_value_t);
    new_file_element->info = result;
    new_file_element->next = temporal_file_list;
    temporal_file_list = new_file_element;

    return result;
}

static void add_to_list_of_temporal_files_(const char* name, char is_temporary, char is_dir)
{
    // Do not add it twice
    if (name_is_in_temporal_files(name))
        return;

    add_to_list_of_temporal_files(name, is_temporary, is_dir);
}

void mark_file_for_cleanup(const char* name)
{
    add_to_list_of_temporal_files_(name, /* is_temporary */ 0, /* is_dir */ 0);
}

void mark_dir_for_cleanup(const char* name)
{
    add_to_list_of_temporal_files_(name, /* is_temporary */ 0, /* is_dir */ 1);
}

void mark_file_as_temporary(const char* name)
{
    add_to_list_of_temporal_files_(name, /* is_temporary */ 1, /* is_dir */ 0);
}

void mark_dir_as_temporary(const char* name)
{
    add_to_list_of_temporal_files_(name, /* is_temporary */ 1, /* is_dir */ 1);
}

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static temporal_file_t new_temporal_dir_unix(void)
{
    char template[256];

    // Behave like glibc
    const char * dir = getenv("TMPDIR");
    if (dir == NULL)
    {
        if (P_tmpdir != NULL)
        {
            dir = P_tmpdir;
        }
        else
        {
            // Desperate fallback
            dir = "/tmp";
        }
    }

    snprintf(template, 255, "%s/%s_XXXXXX", 
            dir, compilation_process.exec_basename);
    template[255] = '\0';

    // Create the temporal file
    char* directory_name = mkdtemp(template);

    if (directory_name == NULL)
    {
        fatal_error("Cannot create temporary directory in %s (Reason: %s)\n", dir, strerror(errno));
        return NULL;
    }

    // Save the info of the new file

    return add_to_list_of_temporal_files(directory_name, /* is_temporary */ 1, /* is_dir */ 1);
}

static temporal_file_t new_temporal_file_unix(void)
{
    char template[256];

    // Behave like glibc
    const char * dir = getenv("TMPDIR");
    if (dir == NULL)
    {
        if (P_tmpdir != NULL)
        {
            dir = P_tmpdir;
        }
        else
        {
            // Desperate fallback
            dir = "/tmp";
        }
    }

    snprintf(template, 255, "%s/%s_XXXXXX", 
            dir, compilation_process.exec_basename);
    template[255] = '\0';

    char* filename = template;


    // Create the temporal file
    int file_descriptor = mkstemp(filename);

    if (file_descriptor < 0) 
    {
        fatal_error("Cannot create temporary file in %s (Reason: %s)\n", dir, strerror(errno));
        return NULL;
    }
    close(file_descriptor);

    #if defined(__CYGWIN__)
        // In Windows files without extension look suspicious
        // and trigger gcc silly bugs
        char template2[256];
        snprintf(template2, 255, "%s.tmp", filename);
        template2[255] = '\0';
        filename = template2;
    #endif
    

    return add_to_list_of_temporal_files(filename, /* is_temporary */ 1, /* is_dir */ 0);
}
#else
static temporal_file_t new_temporal_file_win32(void)
{
    char *template = NULL;

    template = _tempnam(NULL, compilation_process.exec_basename);
    if (template == NULL)
        return NULL;

    return add_to_list_of_temporal_files(strappend(uniquestr(template), ".tmp"), /* is_temporary */ 1, /* is_dir */ 0);
}
#endif

temporal_file_t new_temporal_dir(void)
{
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    return new_temporal_dir_unix();
#else
#error Not implemented yet
#endif
}

temporal_file_t new_temporal_file(void)
{
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    return new_temporal_file_unix();
#else
    return new_temporal_file_win32();
#endif
}

temporal_file_t new_temporal_file_extension(const char* extension)
{
    ERROR_CONDITION(extension == NULL, "Extension cannot be NULL", 0);

    while (*extension == '.') 
        extension++;

    ERROR_CONDITION(*extension == '\0', "Extension cannot be empty", 0);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    temporal_file_t result = new_temporal_file_unix();

    char c[1024];
    snprintf(c, 1023, "%s/%s.%s", 
            give_dirname(result->name),
            give_basename(result->name), 
            extension);
    c[1023] = '\0';

    if (link(result->name, c) != 0)
    {
        fatal_error("Cannot create temporal file '%s': %s\n", c, strerror(errno));
    }

    if (unlink(result->name) != 0)
    {
        fatal_error("Unlink of '%s' failed: %s\n", result->name, strerror(errno));
    }
    result->name = uniquestr(c);

    return result;
#else
#error Not yet implemented in windows
#endif
}

const char* get_extension_filename(const char* filename)
{
    return strrchr(filename, '.');
}

int execute_program(const char* program_name, const char** arguments)
{
    return execute_program_flags(program_name, arguments, /* stdout_f */ NULL, /* stderr_f */ NULL);
}

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static int execute_program_flags_unix(const char* program_name, const char** arguments, const char* stdout_f, const char* stderr_f)
{
    if (program_name == NULL)
        program_name = "";

    int num = count_null_ended_array((void**)arguments);

    const char** execvp_arguments = NEW_VEC0(const char*, num + 1 + 1);

    execvp_arguments[0] = program_name;

    int i;
    for (i = 0; i < num; i++)
    {
        execvp_arguments[i+1] = arguments[i];
    }

    execvp_arguments[i+1] = NULL;

    if (CURRENT_CONFIGURATION->verbose)
    {
        int j = 0;
        while (execvp_arguments[j] != NULL)
        {
            fprintf(stderr, "%s ", execvp_arguments[j]);
            j++;
        }

        if (stdout_f != NULL)
        {
            fprintf(stderr, "1> %s ", stdout_f);
        }
        if (stderr_f != NULL)
        {
            fprintf(stderr, "2> %s ", stderr_f);
        }

        fprintf(stderr, "\n");
    }

    // This routine is UNIX-only
    pid_t spawned_process;
    if (stdout_f == NULL
            && stderr_f == NULL)
    {
        // If no work previous to execvp is requested, vfork is fine
        spawned_process = vfork();
    }
    else
    {
        spawned_process = fork();
    }

    if (spawned_process < 0)
    {
        fatal_error("error: could not fork to execute subprocess '%s' (%s)", program_name, strerror(errno));
    }
    else if (spawned_process == 0) // I'm the spawned process
    {
        // Redirect output files as needed
        if (stdout_f != NULL)
        {
            FILE *new_stdout = fopen(stdout_f, "w");
            if (new_stdout == NULL)
            {
                fatal_error("error: could not redirect standard output to '%s' (%s)",
                        stdout_f,
                        strerror(errno));
            }

            int fd = fileno(new_stdout);
            close(1);
            int new_fd = dup(fd);
            if (new_fd < 0)
            {
                fatal_error("error: could not duplicate standard output");
            }
        }
        if (stderr_f != NULL)
        {
            FILE *new_stderr = fopen(stderr_f, "w");
            if (new_stderr == NULL)
            {
                fatal_error("error: could not redirect standard error to '%s' (%s)",
                        stderr_f,
                        strerror(errno));
            }

            int fd = fileno(new_stderr);
            close(2);
            int new_fd = dup(fd);
            if (new_fd < 0)
            {
                fatal_error("error: could not duplicate standard error");
            }
        }

        // The cast is here because execvp prototype does not get
        // 'const char* const*' but 'char *const*'
        execvp(program_name, (char**)execvp_arguments);

        // Execvp should not return
        fatal_error("error: execution of subprocess '%s' failed (%s)", program_name, strerror(errno));
    }
    else // I'm the parent
    {
        // Wait for my son
        int status;
        wait(&status);
        if (WIFEXITED(status))
        {
            return (WEXITSTATUS(status));
        }
        else if (WIFSIGNALED(status))
        {
            fprintf(stderr, "Subprocess '%s' was ended with signal %d\n",
                    program_name, WTERMSIG(status));

            return 1;
        }
        else
        {
            internal_error(
                    "Subprocess '%s' ended but neither by normal exit nor signal", 
                    program_name);
        }
    }
}
#else

static char* quote_string(const char *c)
{
    const char *p;
    int num_quotes = 0;
    for (p = c; *p != '\0'; p++)
    {
        if (*p == '"')
            num_quotes++;
    }

    if (num_quotes == 0)
        return xstrdup(c);

    char* result = NEW_VEC0(char, num_quotes + strlen(c) + 1);

    char *q = result;
    for (p = c; *p != '\0'; p++)
    {
        if (*p == '"')
        {
            *q = '\\'; q++;
            *q = '"'; q++;
        }
        else
        {
            *q = *p; q++;
        }
    }

    *q = '\0';

    return result;
}

static int execute_program_flags_win32(const char* program_name, const char** arguments, const char* stdout_f, const char* stderr_f)
{
    int num = count_null_ended_array((void**)arguments);

    const char* quoted_args_list[num + 1];
    quoted_args_list[0] = quote_string(program_name);
    int i;
    int length = 1 + strlen(quoted_args_list[0]);
    for (i = 0; i < num; i++)
    {
        quoted_args_list[i + 1] = quote_string(arguments[i]);
        length += 1 + 2 + strlen(quoted_args_list[i + 1]);
    }

    char quoted_args_str[length];

    quoted_args_str[0] = '\0';

    strcat(quoted_args_str, "\"");
    strcat(quoted_args_str, quoted_args_list[0]);
    strcat(quoted_args_str, "\"");
    for (i = 0; i < num; i++)
    {
        strcat(quoted_args_str, " "); 
        strcat(quoted_args_str, "\"");
        strcat(quoted_args_str, quoted_args_list[i + 1]);
        strcat(quoted_args_str, "\"");
    }

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "%s", quoted_args_str);

        if (stdout_f != NULL)
        {
            fprintf(stderr, "1> %s ", stdout_f);
        }
        if (stderr_f != NULL)
        {
            fprintf(stderr, "2> %s ", stderr_f);
        }

        fprintf(stderr, "\n");
    }

    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof (si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    
    // Redirect output files as needed
    if (stdout_f != NULL)
    {
        FILE *new_stdout = fopen(stdout_f, "w");
        if (new_stdout == NULL)
        {
            fatal_error("error: could not redirect standard output to '%s' (%s)",
                    stdout_f,
                    strerror(errno));
        }

        int fd = _fileno(new_stdout);
        si.hStdOutput = (HANDLE) _get_osfhandle(fd);
    }
    if (stderr_f != NULL)
    {
        FILE *new_stderr = fopen(stderr_f, "w");
        if (new_stderr == NULL)
        {
            fatal_error("error: could not redirect standard error to '%s' (%s)",
                    stderr_f,
                    strerror(errno));
        }

        int fd = _fileno(new_stderr);
        si.hStdError = (HANDLE) _get_osfhandle(fd);
    }

    ZeroMemory( &pi, sizeof(pi) );

    // Start the child process. 
    if(!CreateProcess( 
        NULL,   // Module name 
        quoted_args_str, // Command line
        NULL,           // Process handle not inheritable
        NULL,           // Thread handle not inheritable
        TRUE,          // Set handle inheritance to FALSE
        0,              // No creation flags
        NULL,           // Use parent's environment block
        NULL,           // Use parent's starting directory 
        &si,            // Pointer to STARTUPINFO structure
        &pi )           // Pointer to PROCESS_INFORMATION structure
    ) 
    {
        fatal_error( "CreateProcess failed (%d).\n", GetLastError() );
    }

    // Wait until child process exits.
    if (WaitForSingleObject( pi.hProcess, INFINITE ))
    {
        fatal_error("WaitForSingleObject failed\n", 0);
    }

    DWORD result = 0;
    GetExitCodeProcess(pi.hProcess, &result);

    return result;
}
#endif

int execute_program_flags(const char* program_name, const char** arguments, const char* stdout_f, const char* stderr_f)
{
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    return execute_program_flags_unix(program_name, arguments, stdout_f, stderr_f);
#else
    return execute_program_flags_win32(program_name, arguments, stdout_f, stderr_f);
#endif
}

int count_null_ended_array(void** v)
{
    int result = 0;
    if (v == NULL)
    {
        return result;
    }

    while (v[result] != NULL)
    {
        result++;
    }

    return result;
}

void remove_string_from_null_ended_string_array(const char** string_arr, const char* to_remove)
{
    int i, idx = -1;
    for (i = 0; string_arr[i] != NULL; i++)
    {
        if (strcmp(string_arr[i], to_remove) == 0)
        {
            idx = i;
            break;
        }
    }

    if (idx >= 0) // found
    {
        for (i = idx; string_arr[i] != NULL; i++)
        {
            string_arr[i] = string_arr[i + 1];
        }
    }
}

void timing_start(timing_t* t)
{
    memset(t, 0, sizeof(*t));
    
    gettimeofday(&(t->start), NULL);
}

void timing_end(timing_t* t)
{
    gettimeofday(&(t->end), NULL);

    double start_value = t->start.tv_sec*1e6 + t->start.tv_usec;
    double end_value = t->end.tv_sec*1e6 + t->end.tv_usec;

    double diff_value = end_value - start_value;

    t->elapsed_time = diff_value / 1e6;
}

double timing_elapsed(const timing_t* t)
{
    return (t->elapsed_time);
}

// Inspired on the GNOME's bug-buddy code
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
void run_gdb(void)
{
    pid_t son = fork();

    if (son < 0)
    {
        const char* reason = strerror(errno);
        fprintf(stderr, "fork failed: %s\n", reason);
    }
    else if (son == 0)
    {
        char dump_name[256];
        snprintf(dump_name, 255, "%s_%lu.backtrace.txt", 
                give_basename(compilation_process.argv[0]),
                (unsigned long)getppid());
        dump_name[255] = '\0';

        FILE* output_dump = fopen(dump_name, "w");

        if (output_dump != NULL)
        {
            fprintf(stderr, "Backtrace will be left at '%s'\n", dump_name);
            close(1);
            int new_stdout = dup(fileno(output_dump));
            if (new_stdout < 0)
            {
                fatal_error("error: could not duplicate standard output");
            }
            close(2);
            int new_stderr = dup(fileno(output_dump));
            if (new_stderr < 0)
            {
                fatal_error("error: could not duplicate standard error");
            }

            char pid[16];
            snprintf(pid, 15, "%lu", (unsigned long)getppid());
            pid[15] = '\0';

            char *program_path = xstrdup(compilation_process.argv[0]);

            char *args[] = { "--batch", 
                "--quiet",
                "-ex", "set confirm off",
                "-ex", "set backtrace limit 500",
                "-ex", "bt",
                "-ex", "thread apply all bt full",
                "-ex", "detach",
                "-ex", "q",
                program_path,
                pid,
                (void*)0,
            };

            execvp("gdb", args);

            const char* reason = strerror(errno);
            fprintf(stderr, "exec of gdb failed: %s\n", reason);
        }
        else
        {
            const char* reason = strerror(errno);
            fprintf(stderr, "Could not open for output dump file '%s' (%s)\n",
                    dump_name, reason);
        }
    }
    else
    {
        int wait_result = 0;
        wait(&wait_result);

        if (WIFEXITED(wait_result)
                && WEXITSTATUS(wait_result) == 0)
        {
            fprintf(stderr, "Please, send this backtrack attached to your bug report. Thank you\n");
        }
    }
    // Disable gdb from now
    debug_options.do_not_run_gdb = 0;
}
#endif

char copy_file(const char* source, const char* dest)
{
    // Plain old copy
    FILE* orig_file = fopen(source, "r");
    if (orig_file == NULL)
        return -1;

    FILE* dest_file = fopen(dest, "w");

    if (dest_file == NULL)
    {
        fclose(orig_file);
        return -1;
    }

    // size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
#define BLOCK_SIZE 1024
    char c[BLOCK_SIZE];
    int actually_read = fread(c, sizeof(char), BLOCK_SIZE, orig_file);

    while (actually_read != 0)
    {
        int actually_written = fwrite(c, sizeof(char), actually_read, dest_file);
        if (actually_written < actually_read)
        {
            fclose(dest_file);
            fclose(orig_file);
            return -1;
        }
        actually_read = fread(c, sizeof(char), BLOCK_SIZE, orig_file);
    }
#undef BLOCK_SIZE
    if (feof(orig_file))
    {
        // Everything is OK
        clearerr(orig_file);
    }
    else if (ferror(orig_file))
    {
        // Something went wrong
        fclose(dest_file);
        fclose(orig_file);
        return -1;
    }

    fclose(orig_file);
    fclose(dest_file);
    return 0;
}

char move_file(const char* source, const char* dest)
{
    struct stat buf;
    if (stat(source, &buf) != 0)
        return -1;

    if (S_ISDIR(buf.st_mode))
        return -1;

    dev_t source_fs = buf.st_dev;

    if (stat(give_dirname(dest), &buf) != 0)
        return -1;

    if (!S_ISDIR(buf.st_mode))
        return -1;

    dev_t dest_fs = buf.st_dev;

    if (source_fs == dest_fs)
    {
        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Moving file through rename '%s' -> '%s'\n", source, dest);
        }
        return rename(source, dest);
    }
    else
    {
        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Moving file through copy '%s' -> '%s'\n", source, dest);
        }

        if (copy_file(source, dest) != 0)
            return -1;

        return remove(source);
    }
    // Everything ok
    return 0;
}

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static const char* find_home_unix(const char* progname)
{
    const char* res = NULL;

    int path_max = 0;
#ifdef PATH_MAX
    path_max = PATH_MAX;
#else
    path_max = pathconf(path, _PC_PATH_MAX);
    if (path_max <= 0)
        path_max = 4096;
#endif
    char* c = NEW_VEC(char, path_max);

    if (strchr(progname, '/') == NULL)
    {
        char found = 0;
        // Use PATH to find ourselves
        char* path_env = xstrdup(getenv("PATH"));

        char *current_dir = strtok(path_env, ":");

        while (current_dir != NULL)
        {
            snprintf(c, path_max - 1, "%s/%s", current_dir, progname);
            c[path_max - 1] = '\0';
            errno = 0;
            struct stat buf;
            if (stat(c, &buf) == 0)
            {
                found = 1;
                break;
            }

            current_dir = strtok(NULL, ":");
        }

        DELETE(path_env);

        if (!found)
        {
            fatal_error("Could not find where '%s' is located\n", progname);
        }
    }
    else
    {
        char* ret = realpath(progname, c);

        if (ret == NULL)
        {
            fatal_error("relpath failed when solving '%s'\n", progname);
        }
    }

    res = uniquestr(dirname(c));

    DELETE(c);

    return res;
}

#else 
// Version for mingw
static const char* find_home_win32(void)
{
    char c[1024];
    GetModuleFileName(0, c, sizeof(c));

    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];

    _splitpath(c, drive, dir, fname, ext);

    const char* result = strappend(drive, dir);
    return result;
}
#endif

const char* find_home(const char* progname)
{
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    return find_home_unix(progname);
#else
    return find_home_win32();
#endif
}

const char* find_file_in_directories(
        int num_dirs, 
        const char** directories, 
        const char* libname)
{
    int path_max = 0;
#ifdef PATH_MAX
    path_max = PATH_MAX;
#else
    path_max = pathconf(path, _PC_PATH_MAX);
    if (path_max <= 0)
        path_max = 4096;
#endif
    char* full_path = NEW_VEC(char, path_max);
    const char* path_found = NULL;

    int i;
    for (i = 0; i < num_dirs; i++)
    {
        struct stat buf;
        memset(&buf, 0, sizeof(buf));

        snprintf(full_path, path_max, "%s/%s", directories[i], libname);
        full_path[path_max - 1] = '\0';

        if (access(full_path, F_OK) == 0)
        {
            path_found = uniquestr(full_path);
            break;
        }
    }

    DELETE(full_path);

    return path_found;
}
