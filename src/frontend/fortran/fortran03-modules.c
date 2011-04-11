#include "fortran03-modules.h"
#include "fortran03-buildscope.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <sqlite3.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

static void create_storage(sqlite3**, const char*);
static void init_storage(sqlite3*);
static void dispose_storage(sqlite3*);

static sqlite3_int64 insert_symbol(sqlite3* handle, scope_entry_t* symbol);
static sqlite3_int64 insert_type(sqlite3* handle, type_t* t);

static type_t* load_type(sqlite3* handle, sqlite3_int64 oid);
static scope_entry_t* load_symbol(sqlite3* handle, sqlite3_int64 oid);

static const char *get_path_of_module(const char* module_name, char is_creation);

static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_int64 module_symbol);

void dump_module_info(scope_entry_t* module)
{
    ERROR_CONDITION(module->kind = SK_MODULE, "Invalid symbol!", 0);

    sqlite3* handle = NULL;
    create_storage(&handle, module->symbol_name);

    init_storage(handle);

    sqlite3_int64 module_oid = insert_symbol(handle, module);

    finish_module_file(handle, module->symbol_name, module_oid);

    dispose_storage(handle);
}

static void load_storage(sqlite3** handle, const char* filename)
{
    sqlite3_int64 result = sqlite3_open(filename, handle);

    if (result != SQLITE_OK)
    {
        running_error("Error while opening module database '%s' (%s)\n", filename, sqlite3_errmsg(*handle));
    }
}

void load_module_info(const char* module_name, scope_entry_t** module)
{
    ERROR_CONDITION(module == NULL, "Invalid parameter", 0);
    *module = NULL;
    const char* filename = get_path_of_module(module_name, /* is_creation */ 0);

    if (filename == NULL)
        return;

    sqlite3* handle = NULL;

    load_storage(&handle, filename);
}

static void create_storage(sqlite3** handle, const char* module_name)
{
    const char* filename = get_path_of_module(module_name, /* is_creation */ 1);
    // Make sure the file has been removed
    if (access(filename, F_OK))
    {
        if (remove(filename) != 0)
        {
            running_error("Error while removing old module '%s' (%s)\n", filename, strerror(errno));
        }
    }

    load_storage(handle, filename);
}

static void dispose_storage(sqlite3* handle)
{
    if (sqlite3_close(handle) != SQLITE_OK)
    {
        running_error("Error while closing database (%s)\n", sqlite3_errmsg(handle));
    }
}

static void run_query(sqlite3* handle, const char* query)
{
    char* errmsg = NULL;
    if (sqlite3_exec(handle, query, NULL, NULL, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, query);
    }
    sqlite3_free(errmsg);
}

const char* internal_attribute_list = "";

static void init_storage(sqlite3* handle)
{
    {
        const char * create_info = "CREATE TABLE info (module, date, version, build, root_symbol);";
        run_query(handle, create_info);
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
        const char * create_types = "CREATE TABLE type(kind, kind_size, ast0, ast1, ref_type, types, symbols);";
        run_query(handle, create_types);
    }

    {
        const char * create_ast = "CREATE TABLE ast(kind, file, line, text, ast0, ast1, ast2, ast3, "
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent);";
        run_query(handle, create_ast);
    }

    {
        const char * create_temp_mapping = "CREATE TEMP TABLE oid_ptr_map(oid, ptr, PRIMARY_KEY(oid, ptr));";
        run_query(handle, create_temp_mapping);
    }
}

static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_int64 module_symbol)
{
    char* insert_info = sqlite3_mprintf("INSERT INTO info(module, date, version, buil, root_symbol) "
            "VALUES(%Q, DATE(), %Q, %Q, %lld);", module_name, VERSION, MCXX_BUILD_VERSION, 
            module_symbol);
    run_query(handle, insert_info);
    sqlite3_free(insert_info);
}


static int get_ptr_of_oid_(void* datum, 
        int ncols UNUSED_PARAMETER, 
        char** values,
        char** colnames UNUSED_PARAMETER)
{
    void **p = (void**)datum;

    // oid - values[0]
    // ptr - values[1]

    // Ugly
    *p = (void*)atoll(values[1]);

    return 0;
}

static void* get_ptr_of_oid(sqlite3* handle, sqlite3_int64 oid)
{
    char * select_oid = sqlite3_mprintf("SELECT oid, ptr FROMs WHERE oid_ptr_map WHERE oid = %lld;", oid);
    char* errmsg = NULL;
    void* result = NULL;

    if (sqlite3_exec(handle, select_oid, get_ptr_of_oid_, &result, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, select_oid);
    }
    sqlite3_free(select_oid);

    return result;
}

static void insert_map_ptr(sqlite3* handle, sqlite3_int64 oid, void *ptr)
{
    char* insert_oid_map = sqlite3_mprintf("INSERT INTO oid_ptr_map(oid, ptr) VALUES(%lld, %p);",
            oid, ptr);
    run_query(handle, insert_oid_map);
    sqlite3_free(insert_oid_map);
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

static sqlite3_int64 oid_already_inserted(sqlite3* handle, const char *table, void *ptr)
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

static sqlite3_int64 insert_type_simple(sqlite3* handle, type_t* t, const char* name, sqlite3_int64 kind_size)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, kind_size) VALUES (%p, %Q, %lld);", t, name, kind_size);
    run_query(handle, insert_type_query);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    return result;
}

static sqlite3_int64 insert_type_ref_to(sqlite3* handle, type_t* t, const char* name, sqlite3_int64 ref_type)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type) VALUES(%p, %Q, '%d');", t, name, ref_type);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    return result;
}

static sqlite3_int64 insert_type_ref_to_list_types(sqlite3* handle, 
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

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, types) VALUES(%p, %Q, '%d', %Q);", 
            t, name, ref_type, list);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    sqlite3_free(list);
    return result;
}

static sqlite3_int64 insert_type_ref_to_ast(sqlite3* handle, 
        type_t* t,
        const char* name, 
        sqlite3_int64 ref_type, 
        sqlite3_int64 ast0, 
        sqlite3_int64 ast1)
{
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)t;

    char *insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, ast0, ast1) VALUES (%p, %Q, %d, %d, %d);",
            t, name, ref_type, ast0, ast1);
    int result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    return result;
}

static sqlite3_int64 insert_ast(sqlite3* handle, AST a)
{
    if (a == NULL)
        return 0;

    if (oid_already_inserted(handle, "ast", a))
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

    type_t* type = expression_get_type(a);
    if (type != NULL)
    {
        insert_type(handle, type);
    }
    scope_entry_t* sym = expression_get_symbol(a);
    if (sym != NULL)
    {
        insert_symbol(handle, sym);
    }

    char is_lvalue = expression_is_lvalue(a);
    char is_const_val = expression_is_constant(a);
    sqlite3_int64 const_val = 0;
    if (is_const_val)
    {
        const_val = const_value_cast_to_8(expression_get_constant(a));
    }

    char is_value_dependent = expression_is_value_dependent(a);
    char *insert_node = sqlite3_mprintf("INSERT INTO ast (oid, kind, file, line, text, ast0, ast1, ast2, ast3, "
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent) "
            "VALUES ("
            // 1
            "%p, %d, %q, %d, %s, %d, %d, %d, %d, "
            // 2
            "%p, %p, %d, %d, %lld, %d"
            ");",
            // 1
            a,
            ast_get_type(a),
            ast_get_filename(a),
            ast_get_line(a),
            text,
            children[0],
            children[1],
            children[2],
            children[3],
            // 2
            type,
            sym,
            is_lvalue,
            is_const_val,
            const_val,
            is_value_dependent
            );

    run_query(handle, insert_node);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_node);
    sqlite3_free(text);
    return result;
}

static sqlite3_int64 insert_type(sqlite3* handle, type_t* t)
{
    sqlite3_int64 result = 0;
    if (t == NULL)
        return result;

    if (is_any_int_type(t))
    {
        result = insert_type_simple(handle, t, "INTEGER", type_get_size(t));
    }
    else if (is_bool_type(t))
    {
        result = insert_type_simple(handle, t, "LOGICAL", type_get_size(t));
    }
    else if (is_floating_type(t))
    {
        result = insert_type_simple(handle, t, "REAL", type_get_size(t));
    }
    else if (is_complex_type(t))
    {
        result = insert_type_simple(handle, t, "COMPLEX", type_get_size(complex_type_get_base_type(t)));
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

static void insert_extra_attr_int(sqlite3* handle, scope_entry_t* symbol, const char* name, sqlite3_int64 value)
{
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%p, %Q, %d);",
           symbol, name, value); 
    run_query(handle, insert_extra_attr);
    sqlite3_free(insert_extra_attr);
}

static void insert_extra_attr_data(sqlite3* handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_int64 (*fun)(sqlite3* handle, void* data))
{
    sqlite3_int64 m = fun(handle, data);
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%p, %Q, %d);",
           symbol, name, m); 
    run_query(handle, insert_extra_attr);
    sqlite3_free(insert_extra_attr);
}

static void insert_extra_attr_symbol(sqlite3* handle, scope_entry_t* symbol, const char* name,
        scope_entry_t* ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(sqlite3*, void*))(insert_symbol));
}

static void insert_extra_attr_type(sqlite3* handle, scope_entry_t* symbol, const char* name,
        type_t* ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(sqlite3*, void*))(insert_type));
}

UNUSED_PARAMETER static void insert_extra_attr_ast(sqlite3* handle, scope_entry_t* symbol, const char* name,
        AST ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(sqlite3*, void*))(insert_ast));
}

static sqlite3_int64 insert_gathered_gcc_attribute(sqlite3* handle, void *p)
{
    // We cannot currently store the decl_context_t
    gather_gcc_attribute_t* g = (gather_gcc_attribute_t*)p;
    return insert_ast(handle, g->expression_list);
}

static sqlite3_int64 insert_default_argument_info_ptr(sqlite3* handle, void* p)
{
    // We cannot currently store the decl_context_t
    default_argument_info_t* d = (default_argument_info_t*)p;
    return insert_ast(handle, d->argument);
}

#include "fortran03-modules-bits.h"

static sqlite3_int64 insert_symbol(sqlite3* handle, scope_entry_t* symbol)
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

typedef struct 
{
    sqlite3* handle;
    scope_entry_t* symbol;
} symbol_handle_t;

static int get_symbol(void *datum, int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    symbol_handle_t* symbol_handle = (symbol_handle_t*)datum;

    sqlite3* handle = symbol_handle->handle;
    scope_entry_t** result = &(symbol_handle->symbol);

    sqlite3_int64 oid = atoll(values[0]);
    const char* name = values[1];
    int symbol_kind = atoi(values[2]);
    sqlite3_int64 type_oid = atoll(values[3]);
    const char* filename = uniquestr(values[4]);
    int line = atoi(values[5]);

    (*result) = calloc(1, sizeof(**result));
    insert_map_ptr(handle, oid, *result);

    (*result)->symbol_name = uniquestr(name);
    (*result)->kind = symbol_kind;
    (*result)->file = filename;
    (*result)->line = line;
    (*result)->type_information = load_type(handle, type_oid);

    return 0;
}

static scope_entry_t* load_symbol(sqlite3* handle, sqlite3_int64 oid)
{
    if (oid == 0)
        return NULL;

    {
        scope_entry_t* ptr = (scope_entry_t*)get_ptr_of_oid(handle, oid);
        if (ptr != NULL)
        {
            return ptr;
        }
    }

    symbol_handle_t result;
    memset(&result, 0, sizeof(result));
    result.handle = handle;

    char *errmsg = NULL;
    char * select_symbol_query = sqlite3_mprintf("SELECT oid, * FROM symbol WHERE oid = %lld;", oid);
    if (sqlite3_exec(handle, select_symbol_query, get_symbol, &result, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    return result.symbol;
}

static const char *get_path_of_module(const char* module_name, char is_creation)
{
    // We will assume that name is already UTF-8 as we do not support any other
    // sort of identifier than ASCII
    const char* filename = strappend(module_name, ".mmod");
    if (is_creation)
    {
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

    int i;
    for (i = 0; i < CURRENT_CONFIGURATION->num_module_dirs; i++)
    {
        const char* path = strappend(
                strappend(CURRENT_CONFIGURATION->module_dirs[i], "/"),
                filename);

        if (access(path, F_OK))
        {
            return path;
        }
    }

    return NULL;
}

typedef
struct
{
    sqlite3* handle;
    AST a;
} AST_query_handle_t;

static AST load_ast(sqlite3* handle, sqlite3_int64 oid);

static int get_ast(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    AST_query_handle_t *p = (AST_query_handle_t*)datum;
    sqlite3* handle = p->handle;

    sqlite3_int64 oid = atoll(values[0]);
    node_t node_kind = atoll(values[1]);
    const char *filename = values[2];
    int line = atoll(values[3]);
    const char* text = values[4];
    // Children: 5  + 0 -> 5 + MAX_AST_CHILDREN 
    sqlite3_int64 type_oid = atoll(values[5 + MAX_AST_CHILDREN + 1]);
    sqlite3_int64 sym_oid = atoll(values[5 + MAX_AST_CHILDREN + 2]);
    char is_lvalue = atoll(values[5 + MAX_AST_CHILDREN + 3]);
    char is_const_val = atoll(values[5 + MAX_AST_CHILDREN + 4]);
    sqlite3_int64 const_val = atoll(values[5 + MAX_AST_CHILDREN + 5]);
    char is_value_dependent = atoll(values[5 + MAX_AST_CHILDREN + 6]);

    p->a = ASTLeaf(node_kind, filename, line, text);
    AST a = p->a;

    insert_map_ptr(handle, oid, a);

    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        sqlite3_int64 child_oid = atoll(values[5 + MAX_AST_CHILDREN + i]);
        AST child_tree = load_ast(handle, child_oid);

        ast_set_child(a, i, child_tree);
    }

    if (type_oid != 0)
    {
        expression_set_type(a, load_type(handle, type_oid));
    }

    if (sym_oid != 0)
    {
        expression_set_symbol(a, load_symbol(handle, sym_oid));
    }

    expression_set_is_lvalue(a, is_lvalue != 0);
    if (is_const_val)
    {
        // Fortran is always signed
        expression_set_constant(a, const_value_get(const_val, 8, 1));
    }

    expression_set_is_value_dependent(a, is_value_dependent != 0);

    return 0;
}

static AST load_ast(sqlite3* handle, sqlite3_int64 oid)
{
    if (oid == 0)
        return NULL;

    {
        AST ptr = (AST)get_ptr_of_oid(handle, oid);
        if (ptr != NULL)
        {
            return ptr;
        }
    }

    AST_query_handle_t query_handle;
    memset(&query_handle, 0, sizeof(query_handle));
    query_handle.handle = handle;

    char *errmsg = NULL;
    char * select_ast_query = sqlite3_mprintf("SELECT oid, kind, file, line, text, ast0, ast1, ast2, ast3, "
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent FROM ast WHERE oid = %lld;", oid);
    if (sqlite3_exec(handle, select_ast_query, get_ast, &query_handle, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }
    sqlite3_free(select_ast_query);

    return 0;
}

typedef
struct {
    sqlite3* handle;
    type_t* type;
} type_handle_t;

static int get_type(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    sqlite3* handle = ((type_handle_t*)datum)->handle;
    type_t** pt = &((type_handle_t*)datum)->type;

    sqlite3_int64 current_oid = atoll(values[0]);
    const char* kind = values[1];
    int kind_size = atoi(values[2]);
    int ast0 = atoi(values[3]);
    int ast1 = atoi(values[4]);
    sqlite3_int64 ref = atoll(values[5]);
    const char* types = values[6];
    const char* symbols = values[7];

    if (strcmp(kind, "INTEGER") == 0)
    {
        *pt = choose_int_type_from_kind(NULL, kind_size);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "REAL") == 0)
    {
        *pt = choose_float_type_from_kind(NULL, kind_size);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "LOGICAL") == 0)
    {
        *pt = choose_logical_type_from_kind(NULL, kind_size);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "COMPLEX") == 0)
    {
        *pt = get_complex_type(choose_float_type_from_kind(NULL, kind_size));
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "POINTER") == 0)
    {
        *pt = get_pointer_type(load_type(handle, ref));
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "ARRAY") == 0)
    {
        AST lower_bound = load_ast(handle, ast0);
        AST upper_bound = load_ast(handle, ast1);

        type_t* element_type = load_type(handle, ref);

        // At the moment we do not store the decl_context
        // Hopefully this will be enough
        decl_context_t decl_context = CURRENT_COMPILED_FILE->global_decl_context;
        *pt = get_array_type_bounds(element_type, lower_bound, upper_bound, decl_context);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "CLASS") == 0)
    {
        char *copy = strdup(symbols);

        *pt = get_new_class_type(CURRENT_COMPILED_FILE->global_decl_context, CK_STRUCT);
        insert_map_ptr(handle, current_oid, *pt);

        char *field = strtok(copy, ",");
        while (field != NULL)
        {
            scope_entry_t* member = load_symbol(handle, atoll(field));

            class_type_add_nonstatic_data_member(*pt, member);

            field = strtok(NULL, ",");
        }
        free(copy);
    }
    else if (strcmp(kind, "FUNCTION") == 0)
    {
        char *copy = strdup(types);

#define MAX_PARAMETERS 256
        parameter_info_t parameter_info[MAX_PARAMETERS];
        memset(parameter_info, 0, sizeof(parameter_info));

        int num_parameters = 0;
        char *field = strtok(copy, ",");
        while (field != NULL)
        {
            ERROR_CONDITION(num_parameters == MAX_PARAMETERS, "Too many parameters %d", num_parameters);

            parameter_info[num_parameters].type_info = load_type(handle, atoll(field));

            num_parameters++;
            field = strtok(NULL, ",");
        }
        free(copy);

        type_t* result = load_type(handle, ref);

        *pt = get_new_function_type(result, parameter_info, num_parameters);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else
    {
        internal_error("Invalid type '%s'\n", kind);
    }

    return 0;
}

static type_t* load_type(sqlite3* handle, sqlite3_int64 oid)
{
    if (oid == 0)
        return NULL;

    {
        type_t* ptr = (type_t*)get_ptr_of_oid(handle, oid);
        if (ptr != NULL)
        {
            return ptr;
        }
    }

    type_handle_t type_handle;
    memset(&type_handle, 0, sizeof(type_handle));
    type_handle.handle = handle;

    char* errmsg = NULL;
    char * select_type_query = sqlite3_mprintf("SELECT oid, kind, kind_size, ast0, ast1, ref_type, types, symbols FROM type WHERE oid = %lld;", oid);
    if (sqlite3_exec(handle, select_type_query, get_type, &type_handle, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    return type_handle.type;
}
