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

#include "cxx-driver-fortran.h"
#include "cxx-driver-decls.h"
#include "cxx-driver-utils.h"
#include "cxx-utils.h"

#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>

#include "filename.h"


#define ID_FILENAME "MERCURIUM_MODULE"

#define LOCK_SUFFIX "_LOCK"

/* flock version */
#if 0
static void do_lock_file_flock(int *lock_fd, const char* lock_filename)
{
    int res = flock(*lock_fd, LOCK_EX);
    if (res < 0)
    {
        fprintf(stderr, "Warning: could not lock file '%s. Reason %s. Paralell compilation may fail\n",
                lock_filename,
                strerror(errno));
        close(*lock_fd);
        *lock_fd = -1;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Exclusive lock on '%s' successfully acquired\n", lock_filename);
        }
    }
}

static void do_unlock_file_flock(int lock_fd, const char* lock_filename)
{
    int res = 0;
    res = flock(lock_fd, LOCK_UN);
    if (res < 0)
    {
        fprintf(stderr, "Warning: could not release lock of '%s'. Reason %s\n",
                lock_filename,
                strerror(errno));
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Lock of file '%s' successfully released\n", lock_filename);
        }
    }
}
#endif

/* fcntl version */
#if 0
           struct flock {
               ...
               short l_type;    /* Type of lock: F_RDLCK,
                                   F_WRLCK, F_UNLCK */
               short l_whence;  /* How to interpret l_start:
                                   SEEK_SET, SEEK_CUR, SEEK_END */
               off_t l_start;   /* Starting offset for lock */
               off_t l_len;     /* Number of bytes to lock */
               pid_t l_pid;     /* PID of process blocking our lock
                                   (F_GETLK only) */
               ...
           };
#endif

static void do_lock_file_fcntl(int *lock_fd, const char* lock_filename)
{
    struct flock fl;
    memset(&fl, 0, sizeof(fl));

    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;

    int res = fcntl(*lock_fd, F_SETLKW, &fl);
    if (res < 0)
    {
        fprintf(stderr, "Warning: could not lock file '%s. Reason %s. Paralell compilation may fail\n",
                lock_filename,
                strerror(errno));
        close(*lock_fd);
        *lock_fd = -1;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Exclusive lock on '%s' successfully acquired\n", lock_filename);
        }
    }
}

static void do_unlock_file_fcntl(int lock_fd, const char* lock_filename)
{
    struct flock fl;
    memset(&fl, 0, sizeof(fl));

    fl.l_type = F_UNLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;

    int res = fcntl(lock_fd, F_SETLK, &fl);
    if (res < 0)
    {
        fprintf(stderr, "Warning: could not release lock of '%s'. Reason %s\n",
                lock_filename,
                strerror(errno));
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Lock of file '%s' successfully released\n", lock_filename);
        }
    }
}

static void lock_module_name_using_ancillary(const char* module_name, int *fd, const char** out_filename)
{
    *fd = -1;
    *out_filename = NULL;

    if (CURRENT_CONFIGURATION->disable_locking)
        return;

    if (CURRENT_CONFIGURATION->lock_dir == NULL)
    {
        const char* home = getenv("HOME");
        if (home == NULL)
        {
            fatal_error("Error: $HOME not defined\n");
        }

        CURRENT_CONFIGURATION->lock_dir = strappend(home, "/.mercurium_locks");

        int res = mkdir(CURRENT_CONFIGURATION->lock_dir, 0700);
        if (res < 0)
        {
            if (errno != EEXIST)
            {
                fatal_error("Error: cannot create lock dir '%s'. Reason: %s\n",
                        CURRENT_CONFIGURATION->lock_dir,
                        strerror(errno));
            }
        }
    }

    const char* lock_filename =
        strappend(CURRENT_CONFIGURATION->lock_dir,
                strappend("/", strappend(give_basename(module_name), LOCK_SUFFIX)));

    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Opening lock file '%s'\n", lock_filename);
    }

    int lock_fd = open(lock_filename, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);

    if (lock_fd < 0)
    {
        fprintf(stderr, "Warning: cannot create ancillary lock file '%s'. "
                "Reason: %s. Parallel compilation may fail\n",
                lock_filename, strerror(errno));
        lock_fd = -1;
    }
    else
    {
        do_lock_file_fcntl(&lock_fd, lock_filename);
    }

    *fd = lock_fd;
    *out_filename = lock_filename;
}

// static const char* get_module_name_of_module_filename(const char* filename)
// {
//     char c[strlen(filename) + 1];
//     strncpy(c, filename, strlen(filename));
//     c[strlen(filename)] = '\0';
// 
//     char* p = strrchr(c, '.');
//     if (p != NULL)
//     {
//         *p = '\0';
//     }
// 
//     return uniquestr(c);
// }

// static void lock_module_filename_using_ancillary(const char* filename, int *fd, const char** out_filename)
// {
//     lock_module_name_using_ancillary(get_module_name_of_module_filename(filename), fd, out_filename);
// }

static void lock_modules(int *fd, const char** out_filename)
{
    lock_module_name_using_ancillary("global_modules", fd, out_filename);
}

static void unlock_module_name_using_ancillary(int lock_fd, const char* lock_filename)
{
    if (lock_fd < 0)
        return;

    ERROR_CONDITION(lock_filename == NULL, "Invalid name", 0);

    do_unlock_file_fcntl(lock_fd, lock_filename);

    int res = close(lock_fd);
    if (res < 0)
    {
        fprintf(stderr, "Warning: failure when closing ancillary lock file '%s'. Reason %s\n",
                lock_filename,
                strerror(errno));
    }
}

// static void unlock_module_filename_using_ancillary(int lock_fd, const char* lock_filename)
// {
//     unlock_module_name_using_ancillary(lock_fd, lock_filename);
// }


static void unlock_modules(int fd, const char* out_filename)
{
    unlock_module_name_using_ancillary(fd, out_filename);
}

static char check_is_mercurium_wrap_module(const char* filename)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Checking if '%s' is a valid Mercurium wrap module\n",
                filename);
    }

    temporal_file_t temp_file = new_temporal_file();

    const char* arguments[] =
    {
        "tf",
        filename,
        NULL
    };

    if (execute_program_flags("tar", arguments, temp_file->name, NULL) != 0)
    {
        return 0;
    }

    FILE* f = fopen(temp_file->name, "r");
    if (f == NULL)
        return 0;

    char result = 0;

    char line[256] = { 0 };
    while (fgets(line, 255, f) != NULL && !result)
    {
        // Remove endline
        line[strlen(line) - 1] = '\0';

        // Since we use -C . the file will be prepended a "./"
        if (strcmp(line, "./" ID_FILENAME) == 0)
        {
            result = 1;
        }
    }

    fclose(f);

    return result;
}

static const char *get_path_of_file_in_module_dirs(const char* filename, const char* module_name)
{
    const char * result = NULL;

    int i;
    for (i = 0; (i < CURRENT_CONFIGURATION->num_module_dirs) && (result == NULL); i++)
    {
        const char* path = strappend(
                strappend(CURRENT_CONFIGURATION->module_dirs[i], "/"),
                filename);

        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Trying file '%s'\n", path);
        }

        if (access(path, F_OK) == 0)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "DRIVER-FORTRAN: Using file '%s' for module name '%s'\n", path, module_name);
            }
            result = path;
        }
    }

    // Try current directory
    if (result == NULL)
    {
        const char* path = strappend("./", filename);
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Trying file '%s' (last resort)\n", path);
        }
        if (access(path, F_OK) == 0)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "DRIVER-FORTRAN: Using file '%s' for module name '%s'\n", path, module_name);
            }
            result = path;
        }
    }

    return result;
}

static const char *get_path_of_mercurium_nonwrapped_module(const char* module_name)
{
    const char* filename = strappend(module_name, ".mf03");

    return get_path_of_file_in_module_dirs(filename, module_name);
}

static const char *get_path_of_mercurium_wrap_module(const char* module_name)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Getting path of mercurium wrap module for module name '%s'\n",
                module_name);
    }
    const char* filename = strappend(module_name, ".mod");

    const char * result = get_path_of_file_in_module_dirs(filename, module_name);

    if (result != NULL)
    {
        if (!check_is_mercurium_wrap_module(result))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "DRIVER-FORTRAN: Invalid wrap module file '%s'\n", result);
            }
            result = NULL;
        }
    }

    return result;
}

static const char *get_path_of_module_file(const char* module_name, const char* extension)
{
    const char* filename = strappend(module_name, extension);
    if (CURRENT_CONFIGURATION->module_out_dir != NULL)
    {
        return strappend(
                strappend(CURRENT_CONFIGURATION->module_out_dir, "/"), 
                filename);
    }
    else
    {
        return strappend("./", filename);
    }
}

static const char *get_path_of_mercurium_own_module(const char* module_name)
{
    return get_path_of_module_file(module_name, ".mf03");
}

static const char *get_path_of_native_module(const char* module_name)
{
    return get_path_of_module_file(module_name, ".mod");
}

static const char* unwrap_module(const char* wrap_module, const char* module_name)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Unwrapping wrap module file '%s' for module name '%s'\n", 
                wrap_module, module_name);
    }

    static temporal_file_t temp_dir = NULL;
    if (temp_dir == NULL)
    {
        temp_dir = new_temporal_dir();
        CURRENT_CONFIGURATION->module_native_dir = temp_dir->name;
    }

    const char* arguments[] =
    {
        "xf",
        wrap_module,
        "-C", temp_dir->name,
        ".",
        NULL
    };

    timing_t timing_unwrap;
    timing_start(&timing_unwrap);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Unwrapping module file '%s'\n", wrap_module);
    }
    timing_end(&timing_unwrap);

    if (execute_program("tar", arguments) != 0)
    {
        fatal_error("Error when unwrapping module. tar failed");
    }

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Unwrapped module file '%s' in %.2f seconds\n",
                wrap_module,
                timing_elapsed(&timing_unwrap));
    }

    const char* mf03_filename = strappend(module_name, ".mf03");
    const char* result = strappend(strappend(temp_dir->name, "/"), mf03_filename);

    if (access(result, F_OK) != 0)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Mercurium module '%s' for module name '%s' is not accessible\n", 
                    result, module_name);
        }
        result = NULL;
    }

    return result;
}


static void register_module_for_later_wrap(const char* module_name, const char* mf03_filename)
{
    int i;
    for (i = 0; i < CURRENT_COMPILED_FILE->num_modules_to_wrap; i++)
    {
        if (strcasecmp(CURRENT_COMPILED_FILE->modules_to_wrap[i]->module_name, module_name) == 0)
            return;
    }
    module_to_wrap_info_t *module_to_wrap = NEW0(module_to_wrap_info_t);

    module_to_wrap->module_name = module_name;
    module_to_wrap->mercurium_file = mf03_filename;
    module_to_wrap->native_file = get_path_of_native_module(module_name);

    P_LIST_ADD(CURRENT_COMPILED_FILE->modules_to_wrap,
            CURRENT_COMPILED_FILE->num_modules_to_wrap,
            module_to_wrap);
}

static void wrap_module_file(module_to_wrap_info_t* module_to_wrap)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Wrapping module '%s' using files '%s' and '%s'\n",
                module_to_wrap->module_name,
                module_to_wrap->native_file,
                module_to_wrap->mercurium_file);
    }

    // The file is now busy
    int lock_fd = -1;
    const char* lock_filename = NULL;
    lock_modules(&lock_fd, &lock_filename);

    // We need to do some juggling here
    temporal_file_t temp_dir = new_temporal_dir();

    // First move the native module, keeping the file name
    const char* temp_native = strappend(strappend(temp_dir->name, "/"), 
            give_basename(module_to_wrap->native_file));
    if (move_file(module_to_wrap->native_file, temp_native) != 0)
    {
        fatal_error("Error when wrapping a module: move_file '%s' -> '%s' failed. %s\n",
                module_to_wrap->native_file, temp_native, strerror(errno));
    }

    // Do likewise for the mercurium file
    const char* temp_mercurium = strappend(strappend(temp_dir->name, "/"),
            give_basename(module_to_wrap->mercurium_file));
    if (move_file(module_to_wrap->mercurium_file, temp_mercurium) != 0)
    {
        fatal_error("Error when wrapping a module: move_file '%s' -> '%s' failed. %s\n",
                module_to_wrap->mercurium_file, temp_mercurium, strerror(errno));
    }

    const char* id_filename = strappend(strappend(temp_dir->name, "/"), ID_FILENAME);
    FILE* f = fopen(id_filename, "w");
    if (f == NULL)
    {
        fatal_error("Error when wrapping a module: creation of ID file failed. %s\n", strerror(errno));
    }
    fclose(f);

    // Now pack the two files using tar
    const char* arguments[] =
    {
        "cf",
        module_to_wrap->native_file,
        "-C", temp_dir->name,
        ".",
        NULL
    };

    if (execute_program("tar", arguments) != 0)
    {
        fatal_error("Error when wrapping a module: tar failed\n");
    }

    unlock_modules(lock_fd, lock_filename);
}

#define SUFFIX "_MF03BAK"

static void hide_mercurium_module(const char* filename)
{
    const char* hidden_filename = strappend(filename, SUFFIX);

    if (move_file(filename, hidden_filename) != 0)
    {
        fatal_error("Could not hide mercurium module '%s' -> '%s'. %s\n", filename, hidden_filename, strerror(errno));
    }
}

static void restore_mercurium_module(const char* filename)
{
    const char* hidden_filename = strappend(filename, SUFFIX);

    if (move_file(hidden_filename, filename) != 0)
    {
        fatal_error("Could not restore mercurium module '%s' -> '%s'. %s\n", hidden_filename, filename, strerror(errno));
    }
}

void driver_fortran_wrap_all_modules(void)
{
    int i, num_modules = CURRENT_COMPILED_FILE->num_modules_to_wrap;

    for (i = 0; i < num_modules; i++)
    {
        module_to_wrap_info_t* module_to_wrap = CURRENT_COMPILED_FILE->modules_to_wrap[i];

        wrap_module_file(module_to_wrap);

        DELETE(module_to_wrap);
    }

    DELETE(CURRENT_COMPILED_FILE->modules_to_wrap);
    CURRENT_COMPILED_FILE->num_modules_to_wrap = 0;
}

void driver_fortran_discard_all_modules(void)
{
    int i, num_modules = CURRENT_COMPILED_FILE->num_modules_to_wrap;

    for (i = 0; i < num_modules; i++)
    {
        module_to_wrap_info_t* module_to_wrap = CURRENT_COMPILED_FILE->modules_to_wrap[i];

        // The native file will not exist
        mark_file_for_cleanup(module_to_wrap->mercurium_file);

        DELETE(module_to_wrap);
    }

    DELETE(CURRENT_COMPILED_FILE->modules_to_wrap);
    CURRENT_COMPILED_FILE->num_modules_to_wrap = 0;
}

void driver_fortran_retrieve_module(const char* module_name, 
        const char **mf03_filename,
        const char **wrap_filename)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Retrieving module '%s'\n", module_name);
    }
    *mf03_filename = NULL;
    *wrap_filename = NULL;

    const char* wrap_module = NULL;

    // Try first with a hypothetical unwrapped module
    *mf03_filename = get_path_of_mercurium_nonwrapped_module(module_name);
    if (*mf03_filename != NULL)
        return;

    // If we are told not to wrap, do not attempt wrapped ones
    if (CURRENT_CONFIGURATION->do_not_wrap_fortran_modules)
        return;

    // The module may be in use, so lock it
    int lock_fd = 0;
    const char* lock_filename = NULL;
    lock_modules(&lock_fd, &lock_filename);
    wrap_module = get_path_of_mercurium_wrap_module(module_name);

    if (wrap_module != NULL)
    {
        *mf03_filename = unwrap_module(wrap_module, module_name);
        *wrap_filename = wrap_module;

        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: Module retrieved, file is '%s'\n", module_name);
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER-FORTRAN: No wrap module found for module '%s'\n", module_name);
        }
    }

    unlock_modules(lock_fd, lock_filename);
}

void driver_fortran_register_module(const char* module_name, 
        const char **mf03_filename,
        char is_intrinsic)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Registering new module '%s'\n", module_name);
    }

    *mf03_filename = get_path_of_mercurium_own_module(module_name);

    if (!is_intrinsic)
    {
        register_module_for_later_wrap(module_name, *mf03_filename);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: New module '%s' will be created in %s\n", module_name, *mf03_filename);
    }
}

typedef struct flock_item_tag
{
    int fd;
    const char* filename;
} flock_item_t;

static flock_item_t flock_item;

void driver_fortran_hide_mercurium_modules(void)
{
    int num_modules = CURRENT_COMPILED_FILE->num_module_files_to_hide;

    // Sort the files because we want to lock them always in the same order
    qsort(CURRENT_COMPILED_FILE->module_files_to_hide,
            num_modules,
            sizeof(*CURRENT_COMPILED_FILE->module_files_to_hide),
            (int(*)(const void*, const void*))strcasecmp);

    // We will unlock it in driver_fortran_restore_mercurium_modules
    if (num_modules > 0)
    {
        lock_modules(&flock_item.fd, &flock_item.filename);

        int i;
        for (i = 0; i < num_modules; i++)
        {
            const char* filename = CURRENT_COMPILED_FILE->module_files_to_hide[i];

            hide_mercurium_module(filename);
        }
    }
}

void driver_fortran_restore_mercurium_modules(void)
{
    int num_modules = CURRENT_COMPILED_FILE->num_module_files_to_hide;

    if (num_modules > 0)
    {
        int i;
        for (i = 0; i < num_modules; i++)
        {
            const char* filename = CURRENT_COMPILED_FILE->module_files_to_hide[i];

            restore_mercurium_module(filename);
        }

        unlock_modules(flock_item.fd, flock_item.filename);
    }
}
