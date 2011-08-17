#include "fortran03-modules.h"
#include "fortran03-buildscope.h"
#include "cxx-limits.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "fortran03-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-driver-fortran.h"
#include "cxx-entrylist.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <sqlite3.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#ifdef Q
 #error Q cannot be defined here
#endif

// Uncomment the next line to let you GCC help in wrong types in formats of sqlite3_mprintf
// #define DEBUG_SQLITE3_MPRINTF 1

#if defined(DEBUG_SQLITE3_MPRINTF)
  extern char* sqlite3_mprintf(const char *, ...) 
      __attribute__((format(gnu_printf, 1, 2)));
  // We do this to avoid bogus warnings due to an unknown %Q, %s will do the right thing
  #define Q "%s"
#else
  #define Q "%Q"
#endif

static void create_storage(sqlite3**, const char*);
static void init_storage(sqlite3*);
static void dispose_storage(sqlite3*);

static void start_transaction(sqlite3*);
static void end_transaction(sqlite3*);

static sqlite3_int64 insert_symbol(sqlite3* handle, scope_entry_t* symbol);
static sqlite3_int64 insert_type(sqlite3* handle, type_t* t);

static type_t* load_type(sqlite3* handle, sqlite3_int64 oid);
static scope_entry_t* load_symbol(sqlite3* handle, sqlite3_int64 oid);
static AST load_ast(sqlite3* handle, sqlite3_int64 oid);
static nodecl_t load_nodecl(sqlite3* handle, sqlite3_int64 oid);

typedef
struct module_info_tag module_info_t;

static void get_module_info(sqlite3* handle, module_info_t* minfo);
static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_int64 module_symbol);

static sqlite3_int64 insert_ast(sqlite3* handle, AST a);
static sqlite3_int64 insert_const_value(sqlite3* handle, const_value_t* value);
static sqlite3_int64 insert_nodecl(sqlite3* handle, nodecl_t n);
static void insert_extra_attr_int(sqlite3* handle, scope_entry_t* symbol, const char* name, sqlite3_int64 value);
static void insert_extra_attr_ast(sqlite3* handle, scope_entry_t* symbol, const char* name, AST ast);
static void insert_extra_attr_nodecl(sqlite3* handle, scope_entry_t* symbol, const char* name, nodecl_t ast);
static void insert_extra_attr_symbol(sqlite3* handle, scope_entry_t* symbol, const char* name, scope_entry_t* ref);
static void insert_extra_attr_type(sqlite3* handle, scope_entry_t* symbol, const char* name, type_t* ref);
static void insert_extra_gcc_attr(sqlite3* handle, scope_entry_t* symbol, const char *name, gather_gcc_attribute_t* gcc_attr);
static void insert_extra_attr_data(sqlite3* handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_int64 (*fun)(sqlite3* handle, void* data));
static sqlite3_int64 insert_default_argument_info_ptr(sqlite3* handle, void* p);
static char query_contains_field(int ncols, char** names, const char* field_name, int *result);
static void run_query(sqlite3* handle, const char* query);
static decl_context_t load_decl_context(sqlite3* handle, sqlite3_int64 oid);

static const_value_t* load_const_value(sqlite3* handle, sqlite3_int64 oid);

#define P2LL(x) (long long)(intptr_t)(x)

#define DECL_CONTEXT_FIELDS \
    "flags, " \
    "namespace_scope, " \
    "global_scope, " \
    "block_scope, " \
    "class_scope, " \
    "function_scope, " \
    "prototype_scope, " \
    "current_scope"

struct module_info_tag
{
    const char* module_name;
    const char* date;
    const char* version;
    const char* build;
    sqlite3_int64 module_oid;
};

typedef
struct extra_syms_tag
{
    sqlite3* handle;
    int num_syms;
    scope_entry_t** syms;
} extra_syms_t;

typedef
struct extra_types_tag
{
    sqlite3* handle;
    int num_types;
    type_t** types;
} extra_types_t;

typedef
struct extra_trees_tag
{
    sqlite3* handle;
    int num_trees;
    AST* trees;
} extra_trees_t;

typedef
struct extra_nodecls_tag
{
    sqlite3* handle;
    int num_nodecls;
    nodecl_t* nodecls;
} extra_nodecls_t;

typedef
struct 
{
    sqlite3* handle;
    scope_entry_t* symbol;
} extra_gcc_attrs_t;

static void get_extended_attribute(sqlite3* handle, sqlite3_int64 oid, const char* attr_name,
        void *extra_info,
        int (*get_extra_info_fun)(void *datum, int ncols, char **values, char **names));

static int get_extra_syms(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER);

static int get_extra_types(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER);

static int get_extra_trees(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER);

static int get_extra_nodecls(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER);

typedef 
struct
{
    sqlite3* handle;
    scope_entry_t* symbol;
} extra_default_argument_info_t;

static int get_extra_default_argument_info(void *datum,
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER);

static int get_extra_gcc_attrs(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER);

static sqlite3_int64 safe_atoll(const char *c)
{
    if (c != NULL)
    {
        return atoll(c);
    }
    else
    {
        return 0;
    }
}

static int safe_atoi(const char *c)
{
    if (c != NULL)
    {
        return atoi(c);
    }
    else
    {
        return 0;
    }
}

// Do not use these herein anymore
#pragma GCC poison atoi
#pragma GCC poison atoll

#include "fortran03-modules-bits.h"

void dump_module_info(scope_entry_t* module)
{
    ERROR_CONDITION(module->kind != SK_MODULE, "Invalid symbol!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: Dumping module '%s'\n", module->symbol_name);
    }

    sqlite3* handle = NULL;
    create_storage(&handle, module->symbol_name);

    start_transaction(handle);

    init_storage(handle);

    sqlite3_int64 module_oid = insert_symbol(handle, module);

    finish_module_file(handle, module->symbol_name, module_oid);

    end_transaction(handle);

    dispose_storage(handle);

    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: Finished with dumping of module '%s'\n", module->symbol_name);
    }
}

static void start_transaction(sqlite3* handle)
{
    run_query(handle, "BEGIN TRANSACTION;");
}

static void end_transaction(sqlite3* handle)
{
    run_query(handle, "END TRANSACTION;");
}

static void load_storage(sqlite3** handle, const char* filename)
{
    sqlite3_int64 result = sqlite3_open(filename, handle);

    if (result != SQLITE_OK)
    {
        running_error("Error while opening module database '%s' (%s)\n", filename, sqlite3_errmsg(*handle));
    }

    {
        const char * create_temp_mapping = "CREATE TEMP TABLE oid_ptr_map(oid, ptr, PRIMARY KEY(oid, ptr));";
        run_query(*handle, create_temp_mapping);
    }
}

void load_module_info(const char* module_name, scope_entry_t** module)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: Loading module '%s'\n", module_name);
    }

    ERROR_CONDITION(module == NULL, "Invalid parameter", 0);
    *module = NULL;

    const char* filename = NULL; 
    driver_fortran_retrieve_module(module_name, &filename);

    if (filename == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "FORTRAN-MODULES: No appropriate file was found for module '%s'\n", 
                    module_name);
        }
        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: Using filename '%s' for module '%s'\n", 
                filename,
                module_name);
    }

    sqlite3* handle = NULL;

    load_storage(&handle, filename);

    module_info_t minfo;
    memset(&minfo, 0, sizeof(minfo));

    get_module_info(handle, &minfo);

    *module = load_symbol(handle, minfo.module_oid);

    dispose_storage(handle);
}

static void create_storage(sqlite3** handle, const char* module_name)
{
    const char* filename = NULL;
    driver_fortran_register_module(module_name, &filename);

    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: File used will be '%s'\n", filename);
    }

    // Make sure the file has been removed
    if (access(filename, F_OK) == 0)
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

static int run_select_query(sqlite3* handle, const char* query, 
        int (*fun)(void*, int, char**, char**), 
        void * data,
        char ** errmsg)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: %s\n", query);
    }
    int result = sqlite3_exec(handle, query, fun, data, errmsg);
    if (result != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", *errmsg, query);
    }
    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: Last rowid: %lld\n", 
                sqlite3_last_insert_rowid(handle));
    }
    return result;
}

static void run_query(sqlite3* handle, const char* query)
{
    char* errmsg = NULL;
    run_select_query(handle, query, NULL, NULL, &errmsg);
    sqlite3_free(errmsg);
}

static void init_storage(sqlite3* handle)
{
    {
        const char * create_info = "CREATE TABLE info(module, date, version, build, root_symbol);";
        run_query(handle, create_info);
    }

    {
        char * create_symbol = sqlite3_mprintf("CREATE TABLE symbol(name, kind, type, file, line, language_dependent_value, value, %s);", attr_field_names);
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
        const char * create_context = "CREATE TABLE decl_context(" DECL_CONTEXT_FIELDS ");";
        run_query(handle, create_context);
    }

    {
        const char * create_context = "CREATE TABLE scope(kind, contained_in, related_entry);";
        run_query(handle, create_context);
    }

    {
        const char* create_const_value = "CREATE TABLE const_value(kind, sign, bytes, literal_value, compound_values);";
        run_query(handle, create_const_value);
    }
}

static int get_module_info_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    module_info_t* p = (module_info_t*)datum;
    p->module_name = values[0];
    p->date = values[1];
    p->version = values[2];
    p->build = values[3];
    p->module_oid = safe_atoll(values[4]);

    return 0;
}

static void get_module_info(sqlite3* handle, module_info_t* minfo)
{
    const char * module_info_query = "SELECT module, date, version, build, root_symbol FROM info LIMIT 1;";

    char* errmsg = NULL;
    if (run_select_query(handle, module_info_query, get_module_info_, minfo, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, module_info_query);
    }
}

static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_int64 module_symbol)
{
    char* insert_info = sqlite3_mprintf("INSERT INTO info(module, date, version, build, root_symbol) "
            "VALUES(" Q ", DATE(), " Q ", " Q ", %lld);", module_name, VERSION, MCXX_BUILD_VERSION, 
            (long long int)module_symbol);
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
    *p = (void*)(intptr_t)safe_atoll(values[1]);

    return 0;
}

static void* get_ptr_of_oid(sqlite3* handle, sqlite3_int64 oid)
{
    ERROR_CONDITION(oid == 0, "Invalid zero OID", 0);

    char * select_oid = sqlite3_mprintf("SELECT oid, ptr FROM oid_ptr_map WHERE oid = %lld;", oid);
    char* errmsg = NULL;
    void* result = NULL;

    if (run_select_query(handle, select_oid, get_ptr_of_oid_, &result, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, select_oid);
    }
    sqlite3_free(select_oid);

    return result;
}

static void insert_map_ptr(sqlite3* handle, sqlite3_int64 oid, void *ptr)
{
    char* insert_oid_map = sqlite3_mprintf("INSERT INTO oid_ptr_map(oid, ptr) VALUES(%lld, %lld);",
            P2LL(oid), P2LL(ptr));
    run_query(handle, insert_oid_map);
    sqlite3_free(insert_oid_map);
}

static int count_rows(void* datum, 
        int ncols UNUSED_PARAMETER, 
        char** values UNUSED_PARAMETER,
        char** colnames UNUSED_PARAMETER)
{
    int *num = (int*)datum;
    (*num)++;
    return 0;
}

static sqlite3_int64 oid_already_inserted(sqlite3* handle, const char *table, void *ptr)
{
    if (ptr == NULL)
        return (sqlite3_int64)0;

    char * select_oid = sqlite3_mprintf("SELECT oid FROM %s WHERE oid = %lld;", table, P2LL(ptr));
    char* errmsg = NULL;
    int num_rows = 0;
    if (run_select_query(handle, select_oid, count_rows, &num_rows, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, select_oid);
    }
    sqlite3_free(errmsg);

    return (num_rows != 0);
}

static sqlite3_int64 insert_type_simple(sqlite3* handle, type_t* t, const char* name, sqlite3_int64 kind_size)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)(intptr_t)t;

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, kind_size) VALUES (%lld, " Q ", %lld);", P2LL(t), name, kind_size);
    run_query(handle, insert_type_query);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    return result;
}

static sqlite3_int64 insert_type_ref_to(sqlite3* handle, type_t* t, const char* name, sqlite3_int64 ref_type)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)(intptr_t)t;

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type) VALUES(%lld, " Q ", %lld);", P2LL(t), name, ref_type);
    run_query(handle, insert_type_query);
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
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)(intptr_t)t;

    char *list = sqlite3_mprintf("%s", "");
    int i;
    for (i = 0; i < num_parameters; i++)
    {
        if (i != 0)
        {
            char *old_list = list;
            list = sqlite3_mprintf("%s,%lld", old_list, parameter_types[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%lld", parameter_types[i]);
        }
    }

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, types) VALUES(%lld, " Q ", %lld, " Q ");", 
            P2LL(t), name, ref_type, list);
    run_query(handle, insert_type_query);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    sqlite3_free(list);
    return result;
}

static sqlite3_int64 insert_type_ref_to_list_symbols(sqlite3* handle, 
        type_t* t,
        const char *name, 
        sqlite3_int64 ref_type, 
        sqlite3_int64 num_parameters, 
        sqlite3_int64 *parameter_types)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)(intptr_t)t;

    char *list = sqlite3_mprintf("%s", "");
    int i;
    for (i = 0; i < num_parameters; i++)
    {
        if (i != 0)
        {
            char *old_list = list;
            list = sqlite3_mprintf("%s,%lld", old_list, parameter_types[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%lld", parameter_types[i]);
        }
    }

    char * insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, symbols) VALUES(%lld, " Q ", %lld, " Q ");", 
            P2LL(t), name, ref_type, list);
    run_query(handle, insert_type_query);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    sqlite3_free(list);
    return result;
}

static sqlite3_int64 insert_type_ref_to_symbol(sqlite3* handle, 
        type_t* t,
        const char* name,
        sqlite3_int64 ref_type,
        sqlite3_int64 symbol)
{
    return insert_type_ref_to_list_symbols(handle, t, name, ref_type, 1, &symbol);
}

static sqlite3_int64 insert_type_ref_to_ast(sqlite3* handle, 
        type_t* t,
        const char* name, 
        sqlite3_int64 ref_type, 
        sqlite3_int64 ast0, 
        sqlite3_int64 ast1)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted(handle, "type", t))
        return (sqlite3_int64)(intptr_t)t;

    char *insert_type_query = sqlite3_mprintf("INSERT INTO type(oid, kind, ref_type, ast0, ast1) VALUES (%lld, " Q ", %lld, %lld, %lld);",
            P2LL(t), name, ref_type, ast0, ast1);
    run_query(handle, insert_type_query);
    int result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_type_query);
    return result;
}

static sqlite3_int64 insert_ast(sqlite3* handle, AST a)
{
    if (a == NULL)
        return 0;

    if (oid_already_inserted(handle, "ast", a))
        return (sqlite3_int64)(intptr_t)a;

    sqlite3_int64 children[MCXX_MAX_AST_CHILDREN];
    memset(children, 0, sizeof(children));

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
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
        text = sqlite3_mprintf("" Q "", ast_get_text(a));
    }

    // FIXME!
    type_t* type = nodecl_get_type(_nodecl_wrap(a));
    if (type != NULL)
    {
        insert_type(handle, type);
    }
    scope_entry_t* sym = nodecl_get_symbol(_nodecl_wrap(a));
    if (sym != NULL)
    {
        insert_symbol(handle, sym);
    }

    char is_lvalue = nodecl_expr_is_lvalue(_nodecl_wrap(a));
    char is_const_val = nodecl_is_constant(_nodecl_wrap(a));
    sqlite3_int64 const_val = 0;
    if (is_const_val)
    {
        const_val = insert_const_value(handle, nodecl_get_constant(_nodecl_wrap(a)));
    }

    char is_value_dependent = nodecl_expr_is_value_dependent(_nodecl_wrap(a));
    char *insert_node = sqlite3_mprintf("INSERT INTO ast (oid, kind, file, line, text, ast0, ast1, ast2, ast3, "
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent) "
            "VALUES ("
            // 1
            "%lld, %d, " Q ", %d, %s, %lld, %lld, %lld, %lld, "
            // 2
            "%lld, %lld, %d, %d, %lld, %d"
            ");",
            // 1
            P2LL(a),
            ast_get_type(a),
            ast_get_filename(a),
            ast_get_line(a),
            text,
            children[0],
            children[1],
            children[2],
            children[3],
            // 2
            P2LL(type),
            P2LL(sym),
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

static sqlite3_int64 insert_nodecl(sqlite3* handle, nodecl_t n)
{
    return insert_ast(handle, nodecl_get_ast(n));
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
    else if (is_lvalue_reference_type(t))
    {
        result = insert_type(handle, reference_type_get_referenced_type(t));
        const char *name = "REFERENCE";
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
    else if (is_fortran_character_type(t))
    {
        const char* name = "CHARACTER";

        sqlite3_int64 lower_tree = insert_nodecl(handle, array_type_get_array_lower_bound(t));
        sqlite3_int64 upper_tree = insert_nodecl(handle, array_type_get_array_upper_bound(t));

        // When loading, a 'signed char' will be used instead
        sqlite3_int64 element_type = 0;

        result = insert_type_ref_to_ast(handle, t, name, element_type, lower_tree, upper_tree);
    }
    else if (is_fortran_array_type(t))
    {
        const char *name = "ARRAY";

        sqlite3_int64 lower_tree = insert_nodecl(handle, array_type_get_array_lower_bound(t));
        sqlite3_int64 upper_tree = insert_nodecl(handle, array_type_get_array_upper_bound(t));

        sqlite3_int64 element_type = insert_type(handle, array_type_get_element_type(t));

        result = insert_type_ref_to_ast(handle, t, name, element_type, lower_tree, upper_tree);
    }
    else if (is_unnamed_class_type(t))
    {
        const char* name = "CLASS";

        scope_entry_list_t* members = class_type_get_nonstatic_data_members(t);

        int num_fields = entry_list_size(members);

        sqlite3_int64 field_list[num_fields+1];
        memset(field_list, 0, sizeof(field_list));

        int i = 0;
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* field = entry_list_iterator_current(it);

            field_list[i] = insert_symbol(handle, field);
            i++;
        }
        entry_list_iterator_free(it);
        entry_list_free(members);

        result = insert_type_ref_to_list_symbols(handle, t, name, 0, num_fields, field_list);
    }
    else if (is_void_type(t))
    {
        result = insert_type_simple(handle, t, "VOID", 0);
    }
    else if (is_named_type(t))
    {
        sqlite3_int64 sym_oid = insert_symbol(handle, named_type_get_symbol(t));

        result = insert_type_ref_to_symbol(handle, t, "NAMED", 0, sym_oid);
    }
    else
    {
        internal_error("Invalid type '%s'\n", print_declarator(t));
    }

    return result;
}

UNUSED_PARAMETER static void insert_extra_attr_int(sqlite3* handle, scope_entry_t* symbol, const char* name, sqlite3_int64 value)
{
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%lld, " Q ", %lld);",
           P2LL(symbol), name, value);
    run_query(handle, insert_extra_attr);
    sqlite3_free(insert_extra_attr);
}

static void insert_extra_attr_data(sqlite3* handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_int64 (*fun)(sqlite3* handle, void* data))
{
    sqlite3_int64 m = fun(handle, data);
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%lld, " Q ", %lld);",
           P2LL(symbol), name, m); 
    run_query(handle, insert_extra_attr);
    sqlite3_free(insert_extra_attr);
}

static void insert_extra_gcc_attr(sqlite3* handle, scope_entry_t* symbol, const char *name, gather_gcc_attribute_t* gcc_attr)
{
    insert_ast(handle, nodecl_get_ast(gcc_attr->expression_list));
    char *name_and_tree = sqlite3_mprintf("%s|%lld", 
            gcc_attr->attribute_name,
            P2LL(nodecl_get_ast(gcc_attr->expression_list)));
    char *insert_extra_attr = sqlite3_mprintf("INSERT INTO attributes(symbol, name, value) VALUES(%lld, " Q ", " Q ");",
           P2LL(symbol), name, name_and_tree); 
    run_query(handle, insert_extra_attr);
    sqlite3_free(name_and_tree);
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

static void insert_extra_attr_ast(sqlite3* handle, scope_entry_t* symbol, const char* name,
        AST ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_int64(*)(sqlite3*, void*))(insert_ast));
}

static void insert_extra_attr_nodecl(sqlite3* handle, scope_entry_t* symbol, const char* name,
        nodecl_t ref)
{
    insert_extra_attr_data(handle, symbol, name, nodecl_get_ast(ref), 
            (sqlite3_int64(*)(sqlite3*, void*))(insert_ast));
}

static sqlite3_int64 insert_default_argument_info_ptr(sqlite3* handle, void* p)
{
    // We cannot currently store the decl_context_t
    default_argument_info_t* d = (default_argument_info_t*)p;
    return insert_ast(handle, nodecl_get_ast(d->argument));
}

static char query_contains_field(int ncols, char** names, const char* field_name, int *result)
{
    int i; 
    for (i = 0; i < ncols; i++)
    {
        if ((strlen(names[i]) == strlen(field_name))
                && (strncasecmp(names[i], field_name, strlen(names[i])) == 0))
        {
            *result = i;
            return 1;
        }
    }

    return 0;
}


static int get_extra_syms(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_syms_t* p = (extra_syms_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->syms, p->num_syms, load_symbol(p->handle, safe_atoll(attr_value)));

    return 0;
}

static int get_extra_types(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_types_t* p = (extra_types_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->types, p->num_types, load_type(p->handle, safe_atoll(attr_value)));

    return 0;
}

static int get_extra_trees(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_trees_t* p = (extra_trees_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->trees, p->num_trees, load_ast(p->handle, safe_atoll(attr_value)));

    return 0;
}

static int get_extra_nodecls(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_nodecls_t* p = (extra_nodecls_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->nodecls, p->num_nodecls, load_nodecl(p->handle, safe_atoll(attr_value)));

    return 0;
}

static int get_extra_gcc_attrs(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_gcc_attrs_t* p = (extra_gcc_attrs_t*)datum;

    char *attr_value = strdup(values[0]);

    char *q = strchr(attr_value, '|');
    ERROR_CONDITION(p == NULL, "Wrong field!", 0);
    *q = '\0';

    const char* attr_name = attr_value;
    const char* tree = q+1;

    p->symbol->entity_specs.num_gcc_attributes++;
    ERROR_CONDITION(p->symbol->entity_specs.num_gcc_attributes == MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL, 
            "Too many gcc attributes", 0);
    p->symbol->entity_specs.gcc_attributes[p->symbol->entity_specs.num_gcc_attributes-1].attribute_name = uniquestr(attr_name);
    p->symbol->entity_specs.gcc_attributes[p->symbol->entity_specs.num_gcc_attributes-1].expression_list = 
        _nodecl_wrap(load_ast(p->handle, safe_atoll(tree)));

    free(attr_value);

    return 0;
}

static int get_extra_default_argument_info(void *datum,
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_default_argument_info_t* p = (extra_default_argument_info_t*)datum;

    default_argument_info_t* d = calloc(1, sizeof(*d));
    // We are not storing the context yet
    d->context = CURRENT_COMPILED_FILE->global_decl_context;
    d->argument = _nodecl_wrap(load_ast(p->handle, safe_atoll(values[0])));

    P_LIST_ADD(p->symbol->entity_specs.default_argument_info, p->symbol->entity_specs.num_parameters, d);

    return 0;
}

static void get_extended_attribute(sqlite3* handle, sqlite3_int64 oid, const char* attr_name,
        void *extra_info,
        int (*get_extra_info_fun)(void *datum, int ncols, char **values, char **names))
{
    char * query = sqlite3_mprintf("SELECT value FROM attributes WHERE symbol = %lld AND name = " Q ";",
         oid, attr_name);

    char * errmsg = NULL;
    if (run_select_query(handle, query, get_extra_info_fun, extra_info, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    sqlite3_free(query);
}

static sqlite3_int64 insert_scope(sqlite3* handle, scope_t* scope)
{
    if (scope == NULL)
        return 0;

    if (oid_already_inserted(handle, "scope", scope))
    {
        return (sqlite3_int64)(intptr_t)scope;
    }

    char * insert_scope_query = sqlite3_mprintf("INSERT INTO scope(oid, kind, contained_in, related_entry) VALUES (%lld, %d, %lld, %lld);",
            P2LL(scope),
            scope->kind,
            P2LL(scope->contained_in),
            P2LL(scope->related_entry));
    run_query(handle, insert_scope_query);
    sqlite3_free(insert_scope_query);

    sqlite3_int64 oid = sqlite3_last_insert_rowid(handle);

    insert_symbol(handle, scope->related_entry);

    return oid;
}

static void insert_decl_context(sqlite3* handle, scope_entry_t* symbol, decl_context_t decl_context)
{
    char *insert_decl_context_query = sqlite3_mprintf("INSERT INTO decl_context (oid, " DECL_CONTEXT_FIELDS ") "
            "VALUES (%lld, %d, %lld, %lld, %lld, %lld, %lld, %lld, %lld);",
            P2LL(symbol),
            decl_context.decl_flags,
            insert_scope(handle, decl_context.namespace_scope),
            insert_scope(handle, decl_context.global_scope),
            insert_scope(handle, decl_context.block_scope),
            insert_scope(handle, decl_context.class_scope),
            insert_scope(handle, decl_context.function_scope),
            insert_scope(handle, decl_context.prototype_scope),
            insert_scope(handle, decl_context.current_scope));
    run_query(handle, insert_decl_context_query);
    sqlite3_free(insert_decl_context_query);
}

static sqlite3_int64 insert_symbol(sqlite3* handle, scope_entry_t* symbol)
{
    if (symbol == NULL)
        return 0;

    if (oid_already_inserted(handle, "symbol", symbol))
        return (sqlite3_int64)(intptr_t)symbol;

    char * insert_symbol_query = sqlite3_mprintf("INSERT INTO symbol(oid) "
            "VALUES (%lld);",
            P2LL(symbol));

    run_query(handle, insert_symbol_query);
    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);

    char * attribute_values = symbol_get_attribute_values(handle, symbol);
    sqlite3_int64 type_id = insert_type(handle, symbol->type_information);

    // sqlite3_int64 language_dependent_value_oid = insert_ast(handle, symbol->language_dependent_value);
    sqlite3_int64 language_dependent_value_oid = 0;
    sqlite3_int64 value_oid = insert_nodecl(handle, symbol->value);

    // We should be using UPDATE, but its syntax is so inconvenient here
    char * update_symbol_query = sqlite3_mprintf("INSERT OR REPLACE INTO symbol(oid, name, kind, type, file, line, language_dependent_value, value, %s) "
            "VALUES (%lld, " Q ", %d, %lld, " Q ", %d, %lld, %lld, %s);",
            attr_field_names,
            P2LL(symbol), // oid
            symbol->symbol_name, // name
            symbol->kind, // kind
            type_id, // type
            symbol->file, // file
            symbol->line, // line
            language_dependent_value_oid,
            value_oid,
            attribute_values);

    run_query(handle, update_symbol_query);

    insert_decl_context(handle, symbol, symbol->decl_context);

    insert_extended_attributes(handle, symbol);

    sqlite3_free(insert_symbol_query);
    sqlite3_free(attribute_values);

    return result;
}

typedef struct 
{
    sqlite3* handle;
    scope_entry_t* symbol;
} symbol_handle_t;

static nodecl_t load_nodecl(sqlite3* handle, sqlite3_int64 oid);

static int get_symbol(void *datum, 
        int ncols,
        char **values, 
        char **names)
{
    symbol_handle_t* symbol_handle = (symbol_handle_t*)datum;

    sqlite3* handle = symbol_handle->handle;
    scope_entry_t** result = &(symbol_handle->symbol);

    sqlite3_int64 oid = safe_atoll(values[0]);
    const char* name = values[1];
    int symbol_kind = safe_atoi(values[2]);
    sqlite3_int64 type_oid = safe_atoll(values[3]);
    const char* filename = uniquestr(values[4]);
    int line = safe_atoi(values[5]);
    sqlite3_int64 language_dependent_value_oid = safe_atoll(values[6]);
    sqlite3_int64 value_oid = safe_atoll(values[7]);

    (*result) = calloc(1, sizeof(**result));
    insert_map_ptr(handle, oid, *result);

    (*result)->symbol_name = uniquestr(name);
    (*result)->kind = symbol_kind;
    (*result)->file = filename;
    (*result)->line = line;
    (*result)->type_information = load_type(handle, type_oid);

    (*result)->decl_context = load_decl_context(handle, oid);

    (*result)->value = load_nodecl(handle, value_oid);

    // Add it to its scope
    if ((*result)->symbol_name != NULL)
    {
        insert_entry((*result)->decl_context.current_scope, (*result));
    }

    get_extra_attributes(handle, ncols, values, names, oid, *result);

    // Classes require a bit more of work
    if ((*result)->kind == SK_CLASS)
    {
        decl_context_t class_context = new_class_context((*result)->decl_context, *result);
        type_t* class_type = get_actual_class_type((*result)->type_information);
        class_type_set_inner_context(class_type, class_context);

        scope_entry_list_t* members = class_type_get_nonstatic_data_members(class_type);
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* field = entry_list_iterator_current(it);
            
            // Insert the component in the class context otherwise further lookups will fail
            insert_entry(class_context.current_scope, field);
            
            // Update field context
            field->decl_context = class_context;
        }
        entry_list_iterator_free(it);
        entry_list_free(members);
    }

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
    if (run_select_query(handle, select_symbol_query, get_symbol, &result, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    return result.symbol;
}


typedef
struct scope_info_tag
{
    sqlite3* handle;
    scope_t* scope;
} scope_info_t;

static scope_t* load_scope(sqlite3* handle, sqlite3_int64 oid);

static int get_scope_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    scope_info_t* info = (scope_info_t*)datum;

    info->scope = _new_scope();

    insert_map_ptr(info->handle, safe_atoll(values[0]), info->scope);

    info->scope->kind = safe_atoll(values[1]);
    info->scope->contained_in = load_scope(info->handle, safe_atoll(values[2]));
    info->scope->related_entry = load_symbol(info->handle, safe_atoll(values[3]));

    return 0;
}

static scope_t* load_scope(sqlite3* handle, sqlite3_int64 oid)
{
    if (oid == 0)
        return NULL;

    void *p = get_ptr_of_oid(handle, oid);
    if (p != NULL)
    {
        return (scope_t*)p;
    }

    scope_info_t info;
    memset(&info, 0, sizeof(info));
    info.handle = handle;

    char *errmsg = NULL;
    char *query = sqlite3_mprintf("SELECT oid, kind, contained_in, related_entry FROM scope WHERE oid = %lld;\n", oid);
    if (run_select_query(handle, query, get_scope_, &info, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    return info.scope;
}

typedef
struct decl_context_info_tag
{
    sqlite3* handle;
    decl_context_t* p_decl_context;
} decl_context_t_info_t;


static int get_decl_context_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    decl_context_t* p = ((decl_context_t_info_t*)datum)->p_decl_context;
    sqlite3* handle = ((decl_context_t_info_t*)datum)->handle;

    p->decl_flags = safe_atoll(values[0]);
    p->namespace_scope = load_scope(handle, safe_atoll(values[1]));
    p->global_scope = load_scope(handle, safe_atoll(values[2]));
    p->block_scope = load_scope(handle, safe_atoll(values[3]));
    p->class_scope = load_scope(handle, safe_atoll(values[4]));
    p->function_scope = load_scope(handle, safe_atoll(values[5]));
    p->prototype_scope = load_scope(handle, safe_atoll(values[6]));
    p->current_scope = load_scope(handle, safe_atoll(values[7]));

    return 0;
}

static decl_context_t load_decl_context(sqlite3* handle, sqlite3_int64 sym_oid)
{
    decl_context_t decl_context;
    memset(&decl_context, 0, sizeof(decl_context));

    if (sym_oid == 0)
        return decl_context;

    decl_context_t_info_t decl_context_info;
    decl_context_info.p_decl_context = &decl_context;
    decl_context_info.handle = handle;

    char *errmsg = NULL;
    char * query = sqlite3_mprintf("SELECT " DECL_CONTEXT_FIELDS " FROM decl_context WHERE oid = %lld;", sym_oid);
    if (run_select_query(handle, query, get_decl_context_, &decl_context_info, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }
    sqlite3_free(query);

    return decl_context;
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

    sqlite3_int64 oid = safe_atoll(values[0]);
    node_t node_kind = safe_atoll(values[1]);
    const char *filename = values[2];
    int line = safe_atoll(values[3]);
    const char* text = values[4];
    // Children: 5  + 0 -> 5 + MCXX_MAX_AST_CHILDREN - 1
    sqlite3_int64 type_oid = safe_atoll(values[5 + MCXX_MAX_AST_CHILDREN + 0]);
    sqlite3_int64 sym_oid = safe_atoll(values[5 + MCXX_MAX_AST_CHILDREN + 1]);
    char is_lvalue = safe_atoll(values[5 + MCXX_MAX_AST_CHILDREN + 2]);
    char is_const_val = safe_atoll(values[5 + MCXX_MAX_AST_CHILDREN + 3]);
    sqlite3_int64 const_val = safe_atoll(values[5 + MCXX_MAX_AST_CHILDREN + 4]);
    // char is_value_dependent = safe_atoll(values[5 + MCXX_MAX_AST_CHILDREN + 5]);

    p->a = ASTLeaf(node_kind, filename, line, text);
    AST a = p->a;

    insert_map_ptr(handle, oid, a);

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        sqlite3_int64 child_oid = safe_atoll(values[5 + i]);
        AST child_tree = load_ast(handle, child_oid);

        ast_set_child(a, i, child_tree);
    }

    // FIXME _nodecl_wrap
    if (type_oid != 0)
    {
        nodecl_set_type(_nodecl_wrap(a), load_type(handle, type_oid));
    }

    if (sym_oid != 0)
    {
        nodecl_set_symbol(_nodecl_wrap(a), load_symbol(handle, sym_oid));
    }

    nodecl_expr_set_is_lvalue(_nodecl_wrap(a), is_lvalue != 0);
    if (is_const_val)
    {
        // Fortran is always signed
        const_value_t* v = load_const_value(handle, const_val);
        nodecl_set_constant(_nodecl_wrap(a), v);
    }

    // expression_set_is_value_dependent(a, is_value_dependent != 0);

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
    if (run_select_query(handle, select_ast_query, get_ast, &query_handle, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }
    sqlite3_free(select_ast_query);

    return query_handle.a;
}

static nodecl_t load_nodecl(sqlite3* handle, sqlite3_int64 oid)
{
    AST a = load_ast(handle, oid);
    return _nodecl_wrap(a);
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

    sqlite3_int64 current_oid = safe_atoll(values[0]);
    const char* kind = values[1];
    int kind_size = safe_atoi(values[2]);
    sqlite3_int64 ast0 = safe_atoll(values[3]);
    sqlite3_int64 ast1 = safe_atoll(values[4]);
    sqlite3_int64 ref = safe_atoll(values[5]);
    const char* types = values[6];
    const char* symbols = values[7];

    nodecl_t nodecl_fake = nodecl_make_text("", NULL, 0);

    if (strcmp(kind, "INTEGER") == 0)
    {
        *pt = choose_int_type_from_kind(nodecl_fake, kind_size);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "REAL") == 0)
    {
        *pt = choose_float_type_from_kind(nodecl_fake, kind_size);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "LOGICAL") == 0)
    {
        *pt = choose_logical_type_from_kind(nodecl_fake, kind_size);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "COMPLEX") == 0)
    {
        *pt = get_complex_type(choose_float_type_from_kind(nodecl_fake, kind_size));
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "POINTER") == 0)
    {
        *pt = get_pointer_type(load_type(handle, ref));
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "REFERENCE") == 0)
    {
        *pt = get_lvalue_reference_type(load_type(handle, ref));
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "CHARACTER") == 0)
    {
        nodecl_t lower_bound = load_nodecl(handle, ast0);
        nodecl_t upper_bound = load_nodecl(handle, ast1);

        type_t* element_type = get_signed_char_type();

        // At the moment we do not store the decl_context
        // Hopefully this will be enough
        decl_context_t decl_context = CURRENT_COMPILED_FILE->global_decl_context;
        *pt = get_array_type_bounds(element_type, lower_bound, upper_bound, decl_context);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "ARRAY") == 0)
    {
        nodecl_t lower_bound = load_nodecl(handle, ast0);
        nodecl_t upper_bound = load_nodecl(handle, ast1);

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

        char *context = NULL;
        char *field = strtok_r(copy, ",", &context);
        while (field != NULL)
        {
            scope_entry_t* member = load_symbol(handle, safe_atoll(field));

            ERROR_CONDITION(member == NULL, "Invalid member!\n", 0);
            class_type_add_member(*pt, member);

            field = strtok_r(NULL, ",", &context);
        }
        free(copy);
    }
    else if (strcmp(kind, "FUNCTION") == 0)
    {
        char *copy = strdup(types);

        parameter_info_t parameter_info[MCXX_MAX_FUNCTION_PARAMETERS];
        memset(parameter_info, 0, sizeof(parameter_info));

        int num_parameters = 0;
        char *context = NULL;
        char *field = strtok_r(copy, ",", &context);
        while (field != NULL)
        {
            ERROR_CONDITION(num_parameters == MCXX_MAX_FUNCTION_PARAMETERS, "Too many parameters %d", num_parameters);

            parameter_info[num_parameters].type_info = load_type(handle, safe_atoll(field));

            num_parameters++;
            field = strtok_r(NULL, ",", &context);
        }
        free(copy);

        type_t* result = load_type(handle, ref);

        *pt = get_new_function_type(result, parameter_info, num_parameters);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "VOID") == 0)
    {
        *pt = get_void_type();
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "NAMED") == 0)
    {
        sqlite3_int64 symbol_oid = safe_atoll(symbols);

        scope_entry_t* symbol = load_symbol(handle, symbol_oid);

        *pt = get_user_defined_type(symbol);
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
    if (run_select_query(handle, select_type_query, get_type, &type_handle, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    return type_handle.type;
}

static sqlite3_int64 insert_single_const_value(sqlite3* handle, const_value_t* v, const char* kind, int sign, int bytes, const char* literal_value)
{
    char * insert_value = sqlite3_mprintf("INSERT INTO const_value(oid, kind, sign, bytes, literal_value, compound_values) "
            "VALUES (%lld, " Q ", %d, %d, " Q ", NULL);",
            P2LL(v),
            kind,
            sign,
            bytes,
            literal_value);
    run_query(handle, insert_value);

    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_value);
    return result;
}

static sqlite3_int64 insert_multiple_const_value(sqlite3* handle, const_value_t* v, const char* kind)
{
    int num_elems = const_value_get_num_elements(v);
    int i;

    sqlite3_int64 num_elements_oid[num_elems + 1];
    for (i = 0; i < num_elems; i++)
    {
        num_elements_oid[i] = insert_const_value(handle, const_value_get_element_num(v, i));
    }

    char *list = sqlite3_mprintf("%s", "");
    for (i = 0; i < num_elems; i++)
    {
        if (i != 0)
        {
            char *old_list = list;
            list = sqlite3_mprintf("%s,%lld", old_list, num_elements_oid[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%lld", num_elements_oid[i]);
        }
    }

    char* insert_values = sqlite3_mprintf("INSERT INTO const_value(oid, kind, sign, bytes, literal_value, compound_values) "
            "VALUES(%lld, " Q ", 0, 0, NULL, " Q ");",
            P2LL(v),
            kind,
            list);

    sqlite3_int64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_values);
    sqlite3_free(list);

    return result;
}

#define FLOAT_FORMAT_STR "%.24f"
#define DOUBLE_FORMAT_STR "%.53f"
#define LONG_DOUBLE_FORMAT_STR "%.113Le"

static sqlite3_int64 insert_const_value(sqlite3* handle, const_value_t* value)
{
    if (oid_already_inserted(handle, "const_value", value))
        return (sqlite3_int64)(intptr_t)value;

    if (const_value_is_integer(value))
    {
        char * literal_value = sqlite3_mprintf("%llu", const_value_cast_to_8(value));

        sqlite3_int64 result = insert_single_const_value(handle, value, 
                "INTEGER", 
                const_value_is_signed(value),
                const_value_get_bytes(value),
                literal_value);

        sqlite3_free(literal_value);
        return result;

    }
    else if (const_value_is_float(value))
    {
        char * literal_value = sqlite3_mprintf(FLOAT_FORMAT_STR, const_value_cast_to_float(value));

        sqlite3_int64 result = insert_single_const_value(handle, value, 
                "FLOAT", 
                0, 0,
                literal_value);

        sqlite3_free(literal_value);
        return result;
    }
    else if (const_value_is_double(value))
    {
        char * literal_value = sqlite3_mprintf(DOUBLE_FORMAT_STR, const_value_cast_to_double(value));

        sqlite3_int64 result = insert_single_const_value(handle, value, 
                "DOUBLE", 
                0, 0,
                literal_value);

        sqlite3_free(literal_value);
        return result;
    }
    else if (const_value_is_long_double(value))
    {
        char * literal_value = sqlite3_mprintf(LONG_DOUBLE_FORMAT_STR, const_value_cast_to_long_double(value));

        sqlite3_int64 result = insert_single_const_value(handle, value, 
                "LONG DOUBLE", 
                0, 0,
                literal_value);

        sqlite3_free(literal_value);
        return result;
    }
    else if (const_value_is_complex(value))
    {
        return insert_multiple_const_value(handle, value, "COMPLEX");
    }
    else if (const_value_is_structured(value))
    {
        return insert_multiple_const_value(handle, value, "STRUCT");
    }
    else if (const_value_is_array(value))
    {
        return insert_multiple_const_value(handle, value, "ARRAY");
    }
    else if (const_value_is_vector(value))
    {
        return insert_multiple_const_value(handle, value, "VECTOR");
    }
    else if (const_value_is_string(value))
    {
        return insert_multiple_const_value(handle, value, "STRING");
    }
    else
    {
        internal_error("Invalid const value kind", 0);
    }
    return 0;
}

typedef
struct const_value_helper_tag
{
    sqlite3* handle;
    const_value_t* v;
} const_value_helper_t;

static int get_const_value(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    const_value_helper_t* p = (const_value_helper_t*)datum;

    sqlite3_int64 oid = safe_atoll(values[0]);
    const char* kind = values[1];
    const char* sign_str = values[2];
    const char* bytes_str = values[3];
    const char* literal_value_str = values[4];
    const char* compound_values_str = values[5];

    if (strcmp(kind, "INTEGER") == 0)
    {
        uint64_t t;
        sscanf(literal_value_str, "%llu", (long long unsigned*)&t);

        int bytes = safe_atoi(bytes_str);
        int sign = !!safe_atoi(sign_str);

        p->v = const_value_get_integer(t, bytes, sign);
    }
    else if (strcmp(kind, "FLOAT") == 0)
    {
        float f;
        sscanf(literal_value_str, "%f", &f);
        p->v = const_value_get_float(f);
    }
    else if (strcmp(kind, "DOUBLE") == 0)
    {
        double d;
        sscanf(literal_value_str, "%lf", &d);
        p->v = const_value_get_double(d);
    }
    else if (strcmp(kind, "LONG DOUBLE") == 0)
    {
        long double ld;
        sscanf(literal_value_str, "%Lf", &ld);
        p->v = const_value_get_long_double(ld);
    }
    else if ((strcmp(kind, "ARRAY") == 0)
            || (strcmp(kind, "VECTOR") == 0)
            || (strcmp(kind, "STRING") == 0)
            || (strcmp(kind, "STRUCT") == 0)
            || (strcmp(kind, "COMPLEX") == 0))
    {
        int num_elems = 0;
        char * copy = strdup(compound_values_str);
        const_value_t** list = NULL;
        if (strlen(copy) != 0)
        {
            char *context = NULL;
            char *field = strtok_r(copy, ",", &context);
            while (field != NULL)
            {
                num_elems++;
                field = strtok_r(NULL, ",", &context);
            }
            // strtok_r may have fried 'copy'
            free(copy);
            copy = strdup(compound_values_str);

            list = calloc(num_elems, sizeof(*list));

            int i;
            field = strtok_r(copy, ",", &context);
            while (field != NULL)
            {
                const_value_t* const_value = load_const_value(p->handle, safe_atoll(field));
                list[i] = const_value;

                field = strtok_r(NULL, ",", &context); 
                i++;
            }
        }

        if (strcmp(kind, "ARRAY") == 0)
        {
            p->v = const_value_make_array(num_elems, list);
        }
        else if (strcmp(kind, "VECTOR") == 0)
        {
            p->v = const_value_make_vector(num_elems, list);
        }
        else if (strcmp(kind, "STRUCT") == 0)
        {
            p->v = const_value_make_struct(num_elems, list);
        }
        else if (strcmp(kind, "COMPLEX") == 0)
        {
            ERROR_CONDITION(num_elems != 2, "Invalid complex constant!", 0);

            p->v = const_value_make_complex(list[0], list[1]);
        }
        else if (strcmp(kind, "STRING") == 0)
        {
            p->v = const_value_make_string_from_values(num_elems, list);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
        free(list);
        free(copy);
    }
    else
    {
        internal_error("Invalid literal kind '%s'\n", kind);
    }

    insert_map_ptr(p->handle, oid, p->v);

    return 0;
}

static const_value_t* load_const_value(sqlite3* handle, sqlite3_int64 oid)
{
    char * select_const_value = sqlite3_mprintf("SELECT oid, kind, sign, bytes, literal_value, compound_values FROM const_value WHERE oid = %lld\n;",
            oid);
    char* errmsg = NULL;
    const_value_helper_t result = { handle, NULL };

    if (run_select_query(handle, select_const_value, get_const_value, &result, &errmsg) != SQLITE_OK)
    {
        running_error("Error during query: %s\nQuery was: %s\n", errmsg, select_const_value);
    }
    sqlite3_free(select_const_value);

    return result.v;
}

#ifdef DEBUG_SQLITE3_MPRINTF
 #error Disable DEBUG_SQLITE3_MPRINTF macro once no warnings for sqlite3_mprintf calls are signaled by gcc
#endif
