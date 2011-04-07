#include "fortran03-modules.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"

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
static void dump_symbol(storage_handle_t, scope_entry_t* module, scope_entry_t* symbol);

void dump_module_info(scope_entry_t* module)
{
    ERROR_CONDITION(module->kind = SK_MODULE, "Invalid symbol!", 0);

    storage_handle_t handle = NULL;
    create_storage(&handle, module->symbol_name);

    init_storage(handle, module->symbol_name);

    int i;
    for (i = 0; i < module->entity_specs.num_related_symbols; i++)
    {
        scope_entry_t* symbol = module->entity_specs.related_symbols[i];
        if (symbol->kind == SK_VARIABLE
                || symbol->kind == SK_FUNCTION
                || symbol->kind == SK_CLASS)
        {
            dump_symbol(handle, module, symbol);
        }
    }

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

const char* internal_attribute_list = "";

static void init_storage(storage_handle_t handle, const char* name)
{
    const char * create_info = "CREATE TABLE info (module, date, version, build);";
    run_query(handle, create_info);

    char* insert_info = sqlite3_mprintf("INSERT INTO info VALUES(%Q, DATE(), %Q, %Q);", name, VERSION, MCXX_BUILD_VERSION);
    run_query(handle, insert_info);
    sqlite3_free(insert_info);

    char * create_symbol = sqlite3_mprintf("CREATE TABLE symbol(name, kind, type, file, line, %s);", internal_attribute_list);
    run_query(handle, create_symbol);
    sqlite3_free(create_symbol);

    const char * create_attributes = "CREATE TABLE attributes(name, symbol, value);";
    run_query(handle, create_attributes);

    const char * create_types = "CREATE type(kind, ast0, ast1, ref_type, types, symbols);";
    run_query(handle, create_types);
}

static int insert_type_simple(storage_handle_t handler, const char* name)
{
    // FIXME - Do not insert twice the same type
    char * insert_type = sqlite3_mprintf("INSERT INTO type(kind) VALUES (%Q);", name);
    run_query(handler, insert_type);
    int result = sqlite3_last_insert_rowid(handler);
    sqlite3_free(insert_type);
    return result;
}

static int insert_type_ref_to(storage_handle_t handler, const char* name, int ref_type)
{
    char * insert_type = sqlite3_mprintf("INSERT INTO type(kind, ref_type) VALUES(%Q, '%d');", name, ref_type);
    int result = sqlite3_last_insert_rowid(handler);
    sqlite3_free(insert_type);
    return result;
}

static int insert_type_ref_to_list_types(storage_handle_t handler, 
        const char *name, 
        int ref_type, 
        int num_parameters, int *parameter_types)
{
    char *list = sqlite3_mprintf("");
    int i;
    for (i = 0; i < num_parameters; i++)
    {
        if (i != 0)
        {
            char *old_list = list;
            list = sqlite3_mprintf("%s,%d", old_list, parameter_types[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%d", parameter_types[i]);
        }
    }

    char * insert_type = sqlite3_mprintf("INSERT INTO type(kind, ref_type, types) VALUES(%Q, '%d', %Q);", 
            name, ref_type, list);
    int result = sqlite3_last_insert_rowid(handler);
    sqlite3_free(insert_type);
    sqlite3_free(list);
    return result;
}

static int insert_type_ref_to_ast(storage_handle_t handle UNUSED_PARAMETER, 
        const char* name UNUSED_PARAMETER, 
        int result UNUSED_PARAMETER, 
        int ast0 UNUSED_PARAMETER, 
        int ast1 UNUSED_PARAMETER)
{
    return 0;
}

static int insert_ast(storage_handle_t handle UNUSED_PARAMETER, AST a UNUSED_PARAMETER)
{
    return 0;
}

static int insert_type(storage_handle_t handle, type_t* t)
{
    int result = -1;

    if (is_any_int_type(t))
    {
        char * name = sqlite3_mprintf("INTEGER(%d)", type_get_size(t));
        result = insert_type_simple(handle, name);
        sqlite3_free(name);
    }
    else if (is_bool_type(t))
    {
        char * name = sqlite3_mprintf("LOGICAL(%d)", type_get_size(t));
        result = insert_type_simple(handle, name);
        sqlite3_free(name);
    }
    else if (is_floating_type(t))
    {
        char * name = sqlite3_mprintf("REAL(%d)", type_get_size(t));
        result = insert_type_simple(handle, name);
        sqlite3_free(name);
    }
    else if (is_complex_type(t))
    {
        char * name = sqlite3_mprintf("COMPLEX(%d)", type_get_size(complex_type_get_base_type(t)));
        result = insert_type_simple(handle, name);
        sqlite3_free(name);
    }
    else if (is_pointer_type(t))
    {
        result = insert_type(handle, pointer_type_get_pointee_type(t));
        const char *name = "POINTER";
        result = insert_type_ref_to(handle, name, result);
    }
    else if (is_function_type(t))
    {
        const char *name = "FUNCTION";
        // +1 if the num parameters is zero
        int num_parameters = function_type_get_num_parameters(t);
        int parameter_types[num_parameters + 1];

        int i;
        for (i = 0; i < num_parameters; i++)
        {
            parameter_types[i] = insert_type(handle, function_type_get_parameter_type_num(t, i));
        }

        result = 0;
        if (function_type_get_return_type(t) != NULL)
        {
            result = insert_type(handle, function_type_get_return_type(t));
        }

        result = insert_type_ref_to_list_types(handle, name, result, num_parameters, parameter_types);
    }
    else if (is_array_type(t))
    {
        const char *name = "ARRAY";

        int lower_tree = 0;
        int upper_tree = 0;
        if (array_type_is_unknown_size(t))
        {
        }
        else
        {
            lower_tree = insert_ast(handle, array_type_get_array_lower_bound(t));
            upper_tree = insert_ast(handle, array_type_get_array_upper_bound(t));
        }

        result = insert_type(handle, array_type_get_element_type(t));

        result = insert_type_ref_to_ast(handle, name, result, lower_tree, upper_tree);
    }
    else if (is_class_type(t))
    {
    }
    else
    {
        internal_error("Invalid type '%s'\n", print_declarator(t));
    }

    return result;
}

static void dump_symbol(storage_handle_t handle, scope_entry_t* module, scope_entry_t* symbol)
{
    char *insert_symbol = sqlite3_mprintf("INSERT INTO symbol(%Q, %d, )");
}
