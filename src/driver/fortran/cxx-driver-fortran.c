#include "cxx-driver-fortran.h"
#include "cxx-driver-decls.h"
#include "cxx-driver-utils.h"
#include "cxx-utils.h"

#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#define ID_FILENAME "MERCURIUM_MODULE"

static char check_is_mercurium_wrap_module(const char* filename)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: File '%s' is a valid Mercurium wrap module\n",
                filename);
    }

    const char* arguments[] = 
    {
        "tf",
        filename,
        NULL
    };

    temporal_file_t temp_file = new_temporal_file();

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

    if (execute_program("tar", arguments) != 0)
    {
        running_error("Error when unwrapping module. tar failed", 0);
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
    else
    {
        P_LIST_ADD(CURRENT_COMPILED_FILE->module_files_to_hide,
                CURRENT_COMPILED_FILE->num_module_files_to_hide,
                wrap_module);
    }

    return result;
}


static void register_module_for_later_wrap(const char* module_name, const char* mf03_filename)
{
    module_to_wrap_info_t *module_to_wrap = calloc(1, sizeof(*module_to_wrap));

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

    // We need to do some juggling here
    temporal_file_t temp_dir = new_temporal_dir();

    // First move the native module, keeping the file name
    const char* temp_native = strappend(strappend(temp_dir->name, "/"), module_to_wrap->native_file);
    if (move_file(module_to_wrap->native_file, temp_native) != 0)
    {
        running_error("Error when wrapping a module: move_file '%s' -> '%s' failed. %s\n",
                module_to_wrap->native_file, temp_native, strerror(errno));
    }

    // Do likewise for the mercurium file
    const char* temp_mercurium = strappend(strappend(temp_dir->name, "/"), module_to_wrap->mercurium_file);
    if (move_file(module_to_wrap->mercurium_file, temp_mercurium) != 0)
    {
        running_error("Error when wrapping a module: move_file '%s' -> '%s' failed. %s\n",
                module_to_wrap->mercurium_file, temp_mercurium, strerror(errno));
    }

    const char* id_filename = strappend(strappend(temp_dir->name, "/"), ID_FILENAME);
    FILE* f = fopen(id_filename, "w");
    if (f == NULL)
    {
        running_error("Error when wrapping a module: creation of ID file failed. %s\n", strerror(errno));
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
        running_error("Error when wrapping a module: tar failed\n", 0);
    }
}

#define SUFFIX "_MF03BAK"

static void hide_mercurium_module(const char* filename)
{
    const char* hidden_filename = strappend(filename, SUFFIX);

    if (move_file(filename, hidden_filename) != 0)
    {
        running_error("Could not hide mercurium module '%s' -> '%s'. %s\n", filename, hidden_filename, strerror(errno));
    }
}

static void restore_mercurium_module(const char* filename)
{
    const char* hidden_filename = strappend(filename, SUFFIX);

    if (move_file(hidden_filename, filename) != 0)
    {
        running_error("Could not restore mercurium module '%s' -> '%s'. %s\n", hidden_filename, filename, strerror(errno));
    }
}

void driver_fortran_wrap_all_modules(void)
{
    int i, num_modules = CURRENT_COMPILED_FILE->num_modules_to_wrap;

    for (i = 0; i < num_modules; i++)
    {
        module_to_wrap_info_t* module_to_wrap = CURRENT_COMPILED_FILE->modules_to_wrap[i];

        wrap_module_file(module_to_wrap);

        free(module_to_wrap);
    }

    free(CURRENT_COMPILED_FILE->modules_to_wrap);
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

        free(module_to_wrap);
    }

    free(CURRENT_COMPILED_FILE->modules_to_wrap);
    CURRENT_COMPILED_FILE->num_modules_to_wrap = 0;
}

void driver_fortran_retrieve_module(const char* module_name, const char **mf03_filename)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Retrieving module '%s'\n", module_name);
    }
    *mf03_filename = NULL;

    const char* wrap_module = NULL;

    if (CURRENT_CONFIGURATION->debug_options.disable_module_cache)
    {
        *mf03_filename = get_path_of_mercurium_nonwrapped_module(module_name);
        return;
    }
    else
    {
        wrap_module = get_path_of_mercurium_wrap_module(module_name);
    }

    if (wrap_module != NULL)
    {
        *mf03_filename = unwrap_module(wrap_module, module_name);

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
}

void driver_fortran_register_module(const char* module_name, const char **mf03_filename)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: Registering new module '%s'\n", module_name);
    }

    *mf03_filename = get_path_of_mercurium_own_module(module_name);

    register_module_for_later_wrap(module_name, *mf03_filename);

    DEBUG_CODE()
    {
        fprintf(stderr, "DRIVER-FORTRAN: New module '%s' will be created in %s\n", module_name, *mf03_filename);
    }
}

void driver_fortran_hide_mercurium_modules(void)
{
    int i, num_modules = CURRENT_COMPILED_FILE->num_module_files_to_hide;

    for (i = 0; i < num_modules; i++)
    {
        const char* filename = CURRENT_COMPILED_FILE->module_files_to_hide[i];

        hide_mercurium_module(filename);
    }
}

void driver_fortran_restore_mercurium_modules(void)
{
    int i, num_modules = CURRENT_COMPILED_FILE->num_module_files_to_hide;

    for (i = 0; i < num_modules; i++)
    {
        const char* filename = CURRENT_COMPILED_FILE->module_files_to_hide[i];

        restore_mercurium_module(filename);
    }
}
