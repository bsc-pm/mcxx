#include "fortran03-modules.h"
#include "cxx-utils.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <sqlite3.h>
#include <errno.h>
#include <string.h>

typedef sqlite3* storage_handle_t;

static void create_storage(storage_handle_t*, const char*);
static void init_storage(storage_handle_t, const char*);
static void dispose_storage(storage_handle_t);

void dump_module_info(scope_entry_t* module)
{
    ERROR_CONDITION(module->kind = SK_MODULE, "Invalid symbol!", 0);

    storage_handle_t handle = NULL;
    create_storage(&handle, module->symbol_name);

    init_storage(handle, module->symbol_name);

    dispose_storage(handle);
}

void load_module_info(scope_entry_t** module)
{
    ERROR_CONDITION(module == NULL, "This cannot be NULL", 0);
    *module = NULL;
}

static void create_storage(storage_handle_t* handle, const char* name)
{
    // We will assume that name is already UTF-8 as we do not support any other
    // sort of identifier than ASCII
    // FIXME - Module directory
    const char* filename = strappend(name, ".mmod");
    // Make sure the file has been removed
    if (remove(filename) != 0)
    {
        running_error("Error while removing old module '%s' (%s)\n", filename, strerror(errno));
    }

    int result = sqlite3_open(filename, handle);

    if (result != SQLITE_OK)
    {
        running_error("Error while opening module database '%s' (%s)\n", filename, sqlite3_errmsg(*handle));
    }
}

#if 0
int sqlite3_exec(
  sqlite3*,                                  /* An open database */
  const char *sql,                           /* SQL to be evaluated */
  int (*callback)(void*,int,char**,char**),  /* Callback function */
  void *,                                    /* 1st argument to callback */
  char **errmsg                              /* Error msg written here */
);
#endif

static void dispose_storage(storage_handle_t handle)
{
    if (sqlite3_close(handle) != SQLITE_OK)
    {
        running_error("Error while closing database (%s)\n", sqlite3_errmsg(handle));
    }
}

static void run_query(storage_handle_t handle, const char* query)
{
    char* errmsg = NULL;
    if (sqlite3_exec(handle, query, NULL, NULL, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, query);
    }
    sqlite3_free(errmsg);
}

static void init_storage(storage_handle_t handle, const char* name)
{
    const char * create_info = "CREATE TABLE info (module, date, version, build);";
    run_query(handle, create_info);

    char* insert_info = sqlite3_mprintf("INSERT INTO info VALUES(%Q, DATE(), %Q, %Q);", name, VERSION, MCXX_BUILD_VERSION);
    run_query(handle, insert_info);
    sqlite3_free(insert_info);
}
