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

static sqlite3_int64 insert_symbol(storage_handle_t handle, scope_entry_t* symbol);

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
            insert_symbol(handle, symbol);
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

    sqlite3_int64 result = sqlite3_open(filename, handle);

    if (result != SQLITE_OK)
    {
        running_error("Error while opening module database '%s' (%s)\n", filename, sqlite3_errmsg(*handle));
    }
}

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
    {
        const char * create_info = "CREATE TABLE info (module, date, version, build);";
        run_query(handle, create_info);
    }

    {
        char* insert_info = sqlite3_mprintf("INSERT INTO info VALUES(%Q, DATE(), %Q, %Q);", name, VERSION, MCXX_BUILD_VERSION);
        run_query(handle, insert_info);
        sqlite3_free(insert_info);
    }

    {
        char * create_symbol = sqlite3_mprintf("CREATE TABLE symbol(name, kind, type, file, line, %s);", internal_attribute_list);
        run_query(handle, create_symbol);
        sqlite3_free(create_symbol);
    }

    {
        const char * create_attributes = "CREATE TABLE attributes(name, symbol, value);";
        run_query(handle, create_attributes);
    }

    {
        const char * create_types = "CREATE TABLE type(kind, ast0, ast1, ref_type, types, symbols);";
        run_query(handle, create_types);
    }

    {
        const char * create_ast = "CREATE TABLE ast(kind, file, line, text, ast0, ast1, ast2, ast3);";
        run_query(handle, create_ast);
    }
}

static int count_rows(void* datum, 
        int nrows, 
        char** values UNUSED_PARAMETER,
        char** colnames UNUSED_PARAMETER)
{
    int *num = (int*)datum;
    *num = nrows;
    return 0;
}

static sqlite3_int64 oid_already_inserted(storage_handle_t handle, const char *table, void *ptr)
{
    char * select_oid = sqlite3_mprintf("SELECT oid FROM %s WHERE oid = %p;", table, ptr);
    char* errmsg = NULL;
    int num_rows = 0;
    if (sqlite3_exec(handle, select_oid, count_rows, &num_rows, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, select_oid);
    }
    sqlite3_free(errmsg);

    return (num_rows != 0);
}

static sqlite3_int64 insert_type_simple(storage_handle_t handle, type_t* t, const char* name)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

    char * insert_type = sqlite3_mprintf("INSERT INTO type(oid, kind) VALUES (%p, %Q);", t, name);
    run_query(handle, insert_type);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type);
    return result;
}

static sqlite3_int64 insert_type_ref_to(storage_handle_t handle, type_t* t, const char* name, sqlite3_int64 ref_type)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

    char * insert_type = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type) VALUES(%p, %Q, '%d');", t, name, ref_type);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type);
    return result;
}

static sqlite3_int64 insert_type_ref_to_list_types(storage_handle_t handle, 
        type_t* t,
        const char *name, 
        sqlite3_int64 ref_type, 
        sqlite3_int64 num_parameters, 
        sqlite3_int64 *parameter_types)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

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

    char * insert_type = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, types) VALUES(%p, %Q, '%d', %Q);", 
            t, name, ref_type, list);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type);
    sqlite3_free(list);
    return result;
}

static sqlite3_int64 insert_type_ref_to_ast(storage_handle_t handle, 
        type_t* t,
        const char* name, 
        sqlite3_int64 ref_type, 
        sqlite3_int64 ast0, 
        sqlite3_int64 ast1)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

    char *insert_type = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, ast0, ast1) VALUES (%p, %Q, %d, %d, %d);",
            t, name, ref_type, ast0, ast1);
    int result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type);
    return result;
}

static sqlite3_int64 insert_ast(storage_handle_t handle, AST a)
{
    if (a == NULL)
        return 0;

    if (oid_already_inserted(handle, "type", a))
        return (sqlite3_int64)a;

    sqlite3_int64 children[MAX_AST_CHILDREN];
    memset(children, 0, sizeof(children));

    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        AST child = ast_get_child(a, i);
        if (child != NULL)
        {
            children[i] = insert_ast(handle, child);
        }
    }

    char *text = NULL;
    if (ast_get_text(a) == NULL)
    {
        text = sqlite3_mprintf("NULL");
    }
    else
    {
        text = sqlite3_mprintf("%Q", ast_get_text(a));
    }

    char *insert_node = sqlite3_mprintf("INSERT INTO ast (oid, kind, file, line, text, ast0, ast1, ast2, ast3) "
            "VALUES (%p, %d, %q, %d, %s, %d, %d, %d, %d);",
            a,
            ast_get_type(a),
            ast_get_filename(a),
            ast_get_line(a),
            text,
            children[0],
            children[1],
            children[2],
            children[3]);

    run_query(handle, insert_node);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_node);
    sqlite3_free(text);
    return result;
}

static sqlite3_int64 insert_type(storage_handle_t handle, type_t* t)
{
    sqlite3_int64 result = 0;
    if (t == NULL)
        return result;

    if (is_any_int_type(t))
    {
        char * name = sqlite3_mprintf("INTEGER(%d)", type_get_size(t));
        result = insert_type_simple(handle, t, name);
        sqlite3_free(name);
    }
    else if (is_bool_type(t))
    {
        char * name = sqlite3_mprintf("LOGICAL(%d)", type_get_size(t));
        result = insert_type_simple(handle, t, name);
        sqlite3_free(name);
    }
    else if (is_floating_type(t))
    {
        char * name = sqlite3_mprintf("REAL(%d)", type_get_size(t));
        result = insert_type_simple(handle, t, name);
        sqlite3_free(name);
    }
    else if (is_complex_type(t))
    {
        char * name = sqlite3_mprintf("COMPLEX(%d)", type_get_size(complex_type_get_base_type(t)));
        result = insert_type_simple(handle, t, name);
        sqlite3_free(name);
    }
    else if (is_pointer_type(t))
    {
        result = insert_type(handle, pointer_type_get_pointee_type(t));
        const char *name = "POINTER";
        result = insert_type_ref_to(handle, t, name, result);
    }
    else if (is_function_type(t))
    {
        const char *name = "FUNCTION";
        // +1 if the num parameters is zero
        int num_parameters = function_type_get_num_parameters(t);
        sqlite3_int64 parameter_types[num_parameters + 1];

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

        result = insert_type_ref_to_list_types(handle, t, name, result, num_parameters, parameter_types);
    }
    else if (is_array_type(t))
    {
        const char *name = "ARRAY";

        sqlite3_int64 lower_tree = 0;
        sqlite3_int64 upper_tree = 0;
        if (!array_type_is_unknown_size(t))
        {
            lower_tree = insert_ast(handle, array_type_get_array_lower_bound(t));
            upper_tree = insert_ast(handle, array_type_get_array_upper_bound(t));
        }

        result = insert_type(handle, array_type_get_element_type(t));

        result = insert_type_ref_to_ast(handle, t, name, result, lower_tree, upper_tree);
    }
    else if (is_class_type(t))
    {
        const char* name = "CLASS";

        int i = 0;
        int num_fields = class_type_get_num_nonstatic_data_members(t);

        sqlite3_int64 field_list[num_fields+1];
        memset(field_list, 0, sizeof(field_list));

        for (i = 0; i < num_fields; i++)
        {
            scope_entry_t* field = class_type_get_nonstatic_data_member_num(t, i);

            field_list[i] = insert_symbol(handle, field);
        }

        result = insert_type_ref_to_list_types(handle, t, name, 0, num_fields, field_list);
    }
    else
    {
        internal_error("Invalid type '%s'\n", print_declarator(t));
    }

    return result;
}

static void insert_extra_attr_int(storage_handle_t handle, scope_entry_t* symbol, const char* name, sqlite3_int64 value)
{
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%p, %Q, %d);",
           symbol, name, value); 
    run_query(handle, insert_extra_attr);
    sqlite3_free(insert_extra_attr);
}

static void insert_extra_attr_data(storage_handle_t handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_int64 (*fun)(storage_handle_t handle, void* data))
{
    sqlite3_int64 m = fun(handle, data);
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%p, %Q, %d);",
           symbol, name, m); 
    run_query(handle, insert_extra_attr);
    sqlite3_free(insert_extra_attr);
}

static void insert_extra_attr_symbol(storage_handle_t handle, scope_entry_t* symbol, const char* name,
        scope_entry_t* ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(storage_handle_t, void*))(insert_symbol));
}

static void insert_extra_attr_type(storage_handle_t handle, scope_entry_t* symbol, const char* name,
        type_t* ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(storage_handle_t, void*))(insert_type));
}

UNUSED_PARAMETER static void insert_extra_attr_ast(storage_handle_t handle, scope_entry_t* symbol, const char* name,
        AST ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(storage_handle_t, void*))(insert_ast));
}

#include "fortran03-modules-bits.h"

static sqlite3_int64 insert_symbol(storage_handle_t handle, scope_entry_t* symbol)
{
    if (oid_already_inserted(handle, "symbol", symbol))
        return (sqlite3_int64)symbol;

    char * attribute_values = symbol_get_attribute_values(handle, symbol);

    sqlite3_int64 type_id = insert_type(handle, symbol->type_information);

    char * insert_symbol_query = sqlite3_mprintf("INSERT INTO symbol(oid, name, kind, type, file, line, %s) "
            "VALUES (%p, %Q, %Q, %d, %Q, %d, %s);",
            attr_field_names,
            symbol,
            symbol->symbol_name,
            symbol->kind,
            type_id,
            attribute_values);

    insert_extended_attributes(handle, symbol);

    sqlite3_free(insert_symbol_query);
    sqlite3_free(attribute_values);

    return 0;
}
