#include "fortran03-modules.h"
#include "fortran03-buildscope.h"
#include "cxx-limits.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "fortran03-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-driver-utils.h"
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
static void prepare_statements(sqlite3*);

static void start_transaction(sqlite3*);
static void end_transaction(sqlite3*);

static sqlite3_uint64 insert_symbol(sqlite3* handle, scope_entry_t* symbol);
static sqlite3_uint64 insert_type(sqlite3* handle, type_t* t);

static type_t* load_type(sqlite3* handle, sqlite3_uint64 oid);
static scope_entry_t* load_symbol(sqlite3* handle, sqlite3_uint64 oid);
static AST load_ast(sqlite3* handle, sqlite3_uint64 oid);
static nodecl_t load_nodecl(sqlite3* handle, sqlite3_uint64 oid);

typedef
struct module_info_tag module_info_t;

static void get_module_info(sqlite3* handle, module_info_t* minfo);
static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_uint64 module_symbol);

static sqlite3_uint64 insert_ast(sqlite3* handle, AST a);
static sqlite3_uint64 insert_const_value(sqlite3* handle, const_value_t* value);
static sqlite3_uint64 insert_nodecl(sqlite3* handle, nodecl_t n);
static void insert_extra_attr_int(sqlite3* handle, scope_entry_t* symbol, const char* name, sqlite3_uint64 value);
static void insert_extra_attr_ast(sqlite3* handle, scope_entry_t* symbol, const char* name, AST ast);
static void insert_extra_attr_nodecl(sqlite3* handle, scope_entry_t* symbol, const char* name, nodecl_t ast);
static void insert_extra_attr_symbol(sqlite3* handle, scope_entry_t* symbol, const char* name, scope_entry_t* ref);
static void insert_extra_attr_type(sqlite3* handle, scope_entry_t* symbol, const char* name, type_t* ref);
static void insert_extra_gcc_attr(sqlite3* handle, scope_entry_t* symbol, const char *name, gather_gcc_attribute_t* gcc_attr);
static void insert_extra_attr_data(sqlite3* handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_uint64 (*fun)(sqlite3* handle, void* data));
static sqlite3_uint64 insert_default_argument_info_ptr(sqlite3* handle, void* p);
static char query_contains_field(int ncols, char** names, const char* field_name, int *result);
static void run_query(sqlite3* handle, const char* query);
static decl_context_t load_decl_context(sqlite3* handle, sqlite3_uint64 oid);

static const_value_t* load_const_value(sqlite3* handle, sqlite3_uint64 oid);

#define P2ULL(x) (unsigned long long)(uintptr_t)(x)

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
    sqlite3_uint64 module_oid;
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

static void get_extended_attribute(sqlite3* handle, sqlite3_uint64 oid, const char* attr_name,
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

static sqlite3_uint64 safe_atoull(const char *c)
{
    if (c != NULL)
    {
        return strtoull(c, NULL, 10);
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

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Writing module '%s'\n", module->symbol_name);
    }

    timing_t timing_dump_module;
    timing_start(&timing_dump_module);

    sqlite3* handle = NULL;
    create_storage(&handle, module->symbol_name);

    start_transaction(handle);

    init_storage(handle);

    sqlite3_uint64 module_oid = insert_symbol(handle, module);

    finish_module_file(handle, module->symbol_name, module_oid);

    end_transaction(handle);

    dispose_storage(handle);

    timing_end(&timing_dump_module);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Module '%s' written in %.2f seconds\n", module->symbol_name,
                timing_elapsed(&timing_dump_module));
    }

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
    sqlite3_uint64 result = sqlite3_open(filename, handle);

    if (result != SQLITE_OK)
    {
        running_error("Error while opening module database '%s' (%s)\n", filename, sqlite3_errmsg(*handle));
    }

    {
        const char * create_temp_mapping = "CREATE TEMP TABLE oid_ptr_map(oid, ptr, PRIMARY KEY(oid));";
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

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Loading module '%s'\n", module_name);
    }

    timing_t timing_load_module;
    timing_start(&timing_load_module);

    sqlite3* handle = NULL;

    load_storage(&handle, filename);

    prepare_statements(handle);

    start_transaction(handle);

    module_info_t minfo;
    memset(&minfo, 0, sizeof(minfo));

    get_module_info(handle, &minfo);

    *module = load_symbol(handle, minfo.module_oid);

    end_transaction(handle);

    dispose_storage(handle);

    timing_end(&timing_load_module);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Module '%s' loaded in %.2f seconds\n", 
                module_name,
                timing_elapsed(&timing_load_module));
    }
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
        fprintf(stderr, "FORTRAN-MODULES: Last rowid: %llu\n", 
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

static void define_schema(sqlite3* handle)
{
    {
        const char * create_info = "CREATE TABLE info(module, date, version, build, root_symbol);";
        run_query(handle, create_info);
    }

    {
        char * create_symbol = sqlite3_mprintf("CREATE TABLE symbol(name, kind, type, file, line, value, %s);", attr_field_names);
        run_query(handle, create_symbol);
        sqlite3_free(create_symbol);
    }

    {
        const char * create_attributes = "CREATE TABLE attributes(name, symbol, value);";
        run_query(handle, create_attributes);

        // This index is crucial for fast loading of attributes of symbols
        const char * create_attr_index = "CREATE INDEX attributes_index ON attributes (symbol, name);";
        run_query(handle, create_attr_index);
    }

    {
        const char * create_types = "CREATE TABLE type(kind, cv_qualifier, kind_size, ast0, ast1, ref_type, types, symbols);";
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

// List here all the prepared statements
#define PREPARED_STATEMENT_LIST \
    PREPARED_STATEMENT(_check_repeat_oid_ptr) \
    PREPARED_STATEMENT(_insert_oid_ptr) \
    PREPARED_STATEMENT(_load_symbol_stmt) \
    PREPARED_STATEMENT(_get_ptr_of_oid_stmt) \
    PREPARED_STATEMENT(_oid_already_inserted_type) \
    PREPARED_STATEMENT(_oid_already_inserted_ast) \
    PREPARED_STATEMENT(_oid_already_inserted_scope) \
    PREPARED_STATEMENT(_oid_already_inserted_symbol) \
    PREPARED_STATEMENT(_oid_already_inserted_const_value) \
    PREPARED_STATEMENT(_insert_type_simple_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_list_types_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_list_symbols_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_ast_stmt) \
    PREPARED_STATEMENT(_insert_ast_stmt) \
    PREPARED_STATEMENT(_insert_extra_attr_stmt) \
    PREPARED_STATEMENT(_get_extended_attr_stmt) \

// End of list of prepared statements

#define PREPARED_STATEMENT(_name) \
  static sqlite3_stmt* _name = NULL;
PREPARED_STATEMENT_LIST
#undef PREPARED_STATEMENT

static sqlite3_stmt** _prepared_statements_registry[] =
{
#define PREPARED_STATEMENT(_name) \
    &_name,
PREPARED_STATEMENT_LIST
#undef PREPARED_STATEMENT
    NULL
};

static void prepare_statements(sqlite3* handle)
{
#define DO_PREPARE_STATEMENT(_name, _query) \
    do \
    { \
        const char* query = _query; \
        const char* unused_str = NULL; \
        int result_prepare = sqlite3_prepare_v2( \
                handle, \
                query, \
                strlen(query), \
                &_name, \
                &unused_str); \
        if (result_prepare != SQLITE_OK) \
        { \
            internal_error("An error happened while preparing statement '%s' %s\n", \
                    query, \
                    sqlite3_errmsg(handle)); \
        } \
    } while (0)

    DO_PREPARE_STATEMENT(_check_repeat_oid_ptr, "SELECT oid, ptr FROM oid_ptr_map WHERE oid = $OID AND ptr = $PTR;");
    DO_PREPARE_STATEMENT(_insert_oid_ptr, "INSERT INTO oid_ptr_map(oid, ptr) VALUES ($OID, $PTR);");
    DO_PREPARE_STATEMENT(_load_symbol_stmt, "SELECT oid, * FROM symbol WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_get_ptr_of_oid_stmt, "SELECT ptr FROM oid_ptr_map WHERE oid = $OID;");

    // Already inserted statements
    DO_PREPARE_STATEMENT(_oid_already_inserted_type,        "SELECT oid FROM type WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_ast,         "SELECT oid FROM ast WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_scope,       "SELECT oid FROM scope WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_symbol,      "SELECT oid FROM symbol WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_const_value, "SELECT oid FROM const_value WHERE oid = $OID;");

    // Insert type
    DO_PREPARE_STATEMENT(_insert_type_simple_stmt, 
            "INSERT INTO type(oid, kind, cv_qualifier, kind_size) VALUES($OID, $TYPEKIND, $CVNAME, $KINDSIZE);");
    DO_PREPARE_STATEMENT(_insert_type_ref_to_stmt, 
            "INSERT INTO type(oid, kind, cv_qualifier, ref_type) VALUES($OID, $NAME, $CVNAME, $REFTYPE);");
    DO_PREPARE_STATEMENT(_insert_type_ref_to_list_types_stmt,
            "INSERT INTO type(oid, kind, cv_qualifier, ref_type, types) VALUES($OID, $KIND, $CVNAME, $REFTYPE, $TYPES);");
    DO_PREPARE_STATEMENT(_insert_type_ref_to_list_symbols_stmt,
            "INSERT INTO type(oid, kind, cv_qualifier, ref_type, symbols) VALUES($OID, $KIND, $CVNAME, $REFTYPE, $SYMBOLS);");
    DO_PREPARE_STATEMENT(_insert_type_ref_to_ast_stmt, 
            "INSERT INTO type(oid, kind, cv_qualifier, ref_type, ast0, ast1) VALUES ($OID, $KIND, $CVNAME, $REFTYPE, $ASTZERO, $ASTONE);");

    // Insert AST
    DO_PREPARE_STATEMENT(_insert_ast_stmt, 
            "INSERT INTO ast (oid, kind, file, line, text, ast0, ast1, ast2, ast3, "
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent) "
            "VALUES ("
            // 1
            "$OID, $KIND, $FILE, $LINE, $TEXT, $ASTZERO, $ASTONE, $ASTTWO, $ASTTHREE,"
            // 2
            "$TYPE, $SYMBOL, $ISLVALUE, $ISCONSTVAL, $CONSTVAL, $ISVALDEP"
            ");");

    // Insert extra attrs
    DO_PREPARE_STATEMENT(_insert_extra_attr_stmt, 
            "INSERT INTO attributes(symbol, name, value) VALUES($SYMBOL, $NAME, $VALUE);");

    // Get extended attr
    DO_PREPARE_STATEMENT(_get_extended_attr_stmt,
            "SELECT value FROM attributes WHERE symbol = $SYMBOL AND name = $NAME;");


    // Check all the statements registered have been prepared
#define PREPARED_STATEMENT(_name) \
    if (_name == NULL) \
    { \
        internal_error("Registered statement '" #_name "' has not been prepared\n", 0); \
    }
PREPARED_STATEMENT_LIST
#undef PREPARED_STATEMENT
}

static void init_storage(sqlite3* handle)
{
    define_schema(handle);
    prepare_statements(handle);
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
    p->module_oid = safe_atoull(values[4]);

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

static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_uint64 module_symbol)
{
    char* insert_info = sqlite3_mprintf("INSERT INTO info(module, date, version, build, root_symbol) "
            "VALUES(" Q ", DATE(), " Q ", " Q ", %llu);", module_name, VERSION, MCXX_BUILD_VERSION, 
            (long long int)module_symbol);
    run_query(handle, insert_info);
    sqlite3_free(insert_info);
}

static void* get_ptr_of_oid(sqlite3* handle, sqlite3_uint64 oid)
{
    ERROR_CONDITION(oid == 0, "Invalid zero OID", 0);

    sqlite3_bind_int64(_get_ptr_of_oid_stmt, 1, oid);

    void* result = 0;

    int result_query = sqlite3_step(_get_ptr_of_oid_stmt);
    switch (result_query)
    {
        case SQLITE_ROW:
            {
                result = (void*)(intptr_t)sqlite3_column_int64(_get_ptr_of_oid_stmt, 0);
                break;
            }
        case SQLITE_DONE:
            {
                break;
            }
        default:
            {
                internal_error("Unexpected error %d when running query '%s'", 
                        result_query,
                        sqlite3_errmsg(handle));
            }
    }

    sqlite3_reset(_get_ptr_of_oid_stmt);

    return result;
}

static void insert_map_ptr(sqlite3* handle, sqlite3_uint64 oid, void *ptr)
{
    // Check if the oid, ptr are already in the map, if they are do nothing
    // This is an anticipation of a primary key violation
    sqlite3_bind_int64(_check_repeat_oid_ptr, 1, oid);
    sqlite3_bind_int64(_check_repeat_oid_ptr, 2, P2ULL(ptr));

    int result_query = sqlite3_step(_check_repeat_oid_ptr);
    char found = 0;
    switch (result_query)
    {
        case SQLITE_ROW:
            {
                found = 1;
                break;
            }
        case SQLITE_DONE:
            {
                // Not found
                break;
            }
        default:
            {
                internal_error("Unexpected error %d when running query '%s'", 
                        result_query,
                        sqlite3_errmsg(handle));
            }
    }

    sqlite3_reset(_check_repeat_oid_ptr);

    // If an exact row with the same oif and ptr was found, we don't need insert anything
    // because the tuple (oid, ptr) already exists.
    if (found)
        return;

    // If a row with the same oid but different ptr existed, next INSERT will fail
    // (primary key violation)
    sqlite3_bind_int64(_insert_oid_ptr, 1, oid);
    sqlite3_bind_int64(_insert_oid_ptr, 2, P2ULL(ptr));

    result_query = sqlite3_step(_insert_oid_ptr);
    switch (result_query)
    {
        case SQLITE_DONE:
            {
                // OK
                break;
            }
        default:
            {
                internal_error("Unexpected error %d when running query '%s'", 
                        result_query,
                        sqlite3_errmsg(handle));
            }
    }

    sqlite3_reset(_insert_oid_ptr);
}

#define DEF_OID_ALREADY_INSERTED(_table) \
static char oid_already_inserted_##_table (sqlite3* handle, void *ptr) \
{ \
    sqlite3_bind_int64(_oid_already_inserted_##_table, 1, P2ULL(ptr)); \
    char result = 0; \
    int result_query = sqlite3_step(_oid_already_inserted_##_table); \
    switch (result_query) \
    { \
        case SQLITE_ROW: \
            { \
                result = 1; \
                break; \
            } \
        case SQLITE_DONE: \
            { \
                break; \
            } \
        default: \
            { \
                internal_error("Unexpected error %d when running query '%s'",  \
                        result_query, \
                        sqlite3_errmsg(handle)); \
            } \
    } \
    sqlite3_reset(_oid_already_inserted_##_table); \
    return result; \
} \

static char oid_already_inserted_type(sqlite3* handle, void *ptr);
DEF_OID_ALREADY_INSERTED(type);

static char oid_already_inserted_ast(sqlite3* handle, void *ptr);
DEF_OID_ALREADY_INSERTED(ast);

static char oid_already_inserted_scope(sqlite3* handle, void *ptr);
DEF_OID_ALREADY_INSERTED(scope);

static char oid_already_inserted_symbol(sqlite3* handle, void *ptr);
DEF_OID_ALREADY_INSERTED(symbol);

static char oid_already_inserted_const_value(sqlite3* handle, void *ptr);
DEF_OID_ALREADY_INSERTED(const_value);

static const char* _cv_qualifier_map[8] = 
{
    // cvr
    [0] = "",
    [1] = "const",
    [2] = "volatile", 
    [3] = "const volatile",
    [4] = "restrict",
    [5] = "restrict const",
    [6] = "restrict volatile",
    [7] = "restrict const volatile",
};

static const char* get_qualifier_name_of_type(type_t* t)
{
    int k = (is_const_qualified_type(t) | (is_volatile_qualified_type(t) << 1) | (is_restrict_qualified_type(t) << 2)) % 8;

    return _cv_qualifier_map[k];
}

static type_t* get_qualified_type_with_cv_qualifier_name(type_t* t, const char* name)
{
    if (name == NULL
            || strcmp(name, "") == 0)
        return t;

    int i;
    for (i = 0; i < 8; i++)
    {
        if (strcmp(name, _cv_qualifier_map[i]) == 0)
        {
            char is_restrict = !!(i & (1<<2));
            char is_volatile = !!(i & (1<<1));
            char is_const = !!(i & (1<<0));

            if (is_const)
            {
                t = get_const_qualified_type(t);
            }
            if (is_volatile)
            {
                t = get_volatile_qualified_type(t);
            }
            if (is_restrict)
            {
                t = get_restrict_qualified_type(t);
            }
        }
    }

    return t;
}

static sqlite3_uint64 insert_type_simple(sqlite3* handle, type_t* t, 
        const char* type_kind_name, 
        sqlite3_uint64 kind_size)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    const char* qualifier_name = get_qualifier_name_of_type(t);

    sqlite3_bind_int64(_insert_type_simple_stmt, 1, P2ULL(t));
    sqlite3_bind_text (_insert_type_simple_stmt, 2, type_kind_name, -1, SQLITE_STATIC);
    sqlite3_bind_text (_insert_type_simple_stmt, 3, qualifier_name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_type_simple_stmt, 4, kind_size);

    int result_query = sqlite3_step(_insert_type_simple_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_type_simple_stmt);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    return result;
}

static sqlite3_uint64 insert_type_ref_to(sqlite3* handle, type_t* t, const char* name, sqlite3_int64 ref_type)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    const char* qualifier_name = get_qualifier_name_of_type(t);
    
    sqlite3_bind_int64(_insert_type_ref_to_stmt, 1, P2ULL(t));
    sqlite3_bind_text (_insert_type_ref_to_stmt, 2, name, -1, SQLITE_STATIC);
    sqlite3_bind_text (_insert_type_ref_to_stmt, 3, qualifier_name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_type_ref_to_stmt, 4, ref_type);

    int result_query = sqlite3_step(_insert_type_ref_to_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_type_ref_to_stmt);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    return result;
}

static sqlite3_uint64 insert_type_ref_to_list_types(sqlite3* handle, 
        type_t* t,
        const char *name, 
        sqlite3_uint64 ref_type, 
        sqlite3_uint64 num_parameters, 
        sqlite3_uint64 *parameter_types)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    const char* qualifier_name = get_qualifier_name_of_type(t);
    
    char *list = sqlite3_mprintf("%s", "");
    unsigned int i;
    for (i = 0; i < num_parameters; i++)
    {
        if (i != 0)
        {
            char *old_list = list;
            list = sqlite3_mprintf("%s,%llu", old_list, parameter_types[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%llu", parameter_types[i]);
        }
    }

    sqlite3_bind_int64(_insert_type_ref_to_list_types_stmt, 1, P2ULL(t));
    sqlite3_bind_text (_insert_type_ref_to_list_types_stmt, 2, name, -1, SQLITE_STATIC);
    sqlite3_bind_text (_insert_type_ref_to_list_types_stmt, 3, qualifier_name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_type_ref_to_list_types_stmt, 4, ref_type);
    sqlite3_bind_text (_insert_type_ref_to_list_types_stmt, 5, list, -1, SQLITE_STATIC);

    int result_query = sqlite3_step(_insert_type_ref_to_list_types_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }
    sqlite3_reset(_insert_type_ref_to_list_types_stmt);


    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(list);
    return result;
}

static sqlite3_uint64 insert_type_ref_to_list_symbols(sqlite3* handle, 
        type_t* t,
        const char *name, 
        sqlite3_uint64 ref_type, 
        sqlite3_uint64 num_parameters, 
        sqlite3_uint64 *parameter_types)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    char *list = sqlite3_mprintf("%s", "");
    unsigned int i;
    for (i = 0; i < num_parameters; i++)
    {
        if (i != 0)
        {
            char *old_list = list;
            list = sqlite3_mprintf("%s,%llu", old_list, parameter_types[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%llu", parameter_types[i]);
        }
    }

    const char* qualifier_name = get_qualifier_name_of_type(t);
    
    sqlite3_bind_int64(_insert_type_ref_to_list_symbols_stmt, 1, P2ULL(t));
    sqlite3_bind_text (_insert_type_ref_to_list_symbols_stmt, 2, name, -1, SQLITE_STATIC);
    sqlite3_bind_text (_insert_type_ref_to_list_symbols_stmt, 3, qualifier_name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_type_ref_to_list_symbols_stmt, 4, ref_type);
    sqlite3_bind_text (_insert_type_ref_to_list_symbols_stmt, 5, list, -1, SQLITE_STATIC);

    int result_query = sqlite3_step(_insert_type_ref_to_list_symbols_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }
    sqlite3_reset(_insert_type_ref_to_list_symbols_stmt);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(list);
    return result;
}

static sqlite3_uint64 insert_type_ref_to_symbol(sqlite3* handle, 
        type_t* t,
        const char* name,
        sqlite3_uint64 ref_type,
        sqlite3_uint64 symbol)
{
    return insert_type_ref_to_list_symbols(handle, t, name, ref_type, 1, &symbol);
}

static sqlite3_uint64 insert_type_ref_to_ast(sqlite3* handle, 
        type_t* t,
        const char* name, 
        sqlite3_uint64 ref_type, 
        sqlite3_uint64 ast0, 
        sqlite3_uint64 ast1)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    const char* qualifier_name = get_qualifier_name_of_type(t);
    
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 1, P2ULL(t));
    sqlite3_bind_text (_insert_type_ref_to_ast_stmt, 2, name, -1, SQLITE_STATIC);
    sqlite3_bind_text (_insert_type_ref_to_ast_stmt, 3, qualifier_name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 4, ref_type);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 5, ast0);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 6, ast1);

    int result_query = sqlite3_step(_insert_type_ref_to_ast_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }
    sqlite3_reset(_insert_type_ref_to_ast_stmt);

    int result = sqlite3_last_insert_rowid(handle);
    return result;
}

static sqlite3_uint64 insert_ast(sqlite3* handle, AST a)
{
    if (a == NULL)
        return 0;

    if (oid_already_inserted_ast(handle, a))
        return (sqlite3_uint64)(uintptr_t)a;

    sqlite3_uint64 children[MCXX_MAX_AST_CHILDREN];
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
    sqlite3_uint64 const_val = 0;
    if (is_const_val)
    {
        const_val = insert_const_value(handle, nodecl_get_constant(_nodecl_wrap(a)));
    }

    char is_value_dependent = nodecl_expr_is_value_dependent(_nodecl_wrap(a));

    // 1
    sqlite3_bind_int64(_insert_ast_stmt, 1, P2ULL(a));
    sqlite3_bind_int  (_insert_ast_stmt, 2, ast_get_type(a));
    if (ast_get_filename(a) != NULL)
    {
        sqlite3_bind_text (_insert_ast_stmt, 3, ast_get_filename(a), -1, SQLITE_STATIC);
    }
    else
    {
        sqlite3_bind_null(_insert_ast_stmt, 3);
    }
    sqlite3_bind_int  (_insert_ast_stmt, 4, ast_get_line(a));
    sqlite3_bind_text (_insert_ast_stmt, 5, ast_get_filename(a), -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_ast_stmt, 6, children[0]);
    sqlite3_bind_int64(_insert_ast_stmt, 7, children[1]);
    sqlite3_bind_int64(_insert_ast_stmt, 8, children[2]);
    sqlite3_bind_int64(_insert_ast_stmt, 9, children[3]);
    // 2
    sqlite3_bind_int64(_insert_ast_stmt, 10, P2ULL(type));
    sqlite3_bind_int64(_insert_ast_stmt, 11, P2ULL(sym));
    sqlite3_bind_int  (_insert_ast_stmt, 12, is_lvalue);
    sqlite3_bind_int  (_insert_ast_stmt, 13, is_const_val);
    sqlite3_bind_int64(_insert_ast_stmt, 14, const_val);
    sqlite3_bind_int  (_insert_ast_stmt, 15, is_value_dependent);

    int result_query = sqlite3_step(_insert_ast_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_ast_stmt);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    return result;
}

static sqlite3_uint64 insert_nodecl(sqlite3* handle, nodecl_t n)
{
    return insert_ast(handle, nodecl_get_ast(n));
}

static sqlite3_uint64 insert_type(sqlite3* handle, type_t* t)
{
    sqlite3_uint64 result = 0;
    if (t == NULL)
        return result;

    if (is_any_int_type(t))
    {
        result = insert_type_simple(handle, t, "INTEGER", type_get_size(t));
    }
    else if (is_character_type(t))
    {
        result = insert_type_simple(handle, t, "CHARACTER", type_get_size(t));
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
        sqlite3_uint64 parameter_types[num_parameters + 1];

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
    else if (is_fortran_character_type(t)
            || is_fortran_array_type(t))
    {
        const char *name = "ARRAY";

        sqlite3_uint64 lower_tree = insert_nodecl(handle, array_type_get_array_lower_bound(t));
        sqlite3_uint64 upper_tree = 0;
        if (!array_type_is_unknown_size(t))
        {
            upper_tree = insert_nodecl(handle, array_type_get_array_upper_bound(t));
        }

        sqlite3_uint64 element_type = insert_type(handle, array_type_get_element_type(t));

        result = insert_type_ref_to_ast(handle, t, name, element_type, lower_tree, upper_tree);
    }
    else if (is_unnamed_class_type(t))
    {
        const char* name = "CLASS";

        scope_entry_list_t* members = class_type_get_nonstatic_data_members(t);

        int num_fields = entry_list_size(members);

        sqlite3_uint64 field_list[num_fields+1];
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
        sqlite3_uint64 sym_oid = insert_symbol(handle, named_type_get_symbol(t));

        if (is_indirect_type(t))
        {
            result = insert_type_ref_to_symbol(handle, t, "INDIRECT", 0, sym_oid);
        }
        else
        {
            result = insert_type_ref_to_symbol(handle, t, "NAMED", 0, sym_oid);
        }
    }
    else
    {
        internal_error("Invalid type '%s'\n", print_declarator(t));
    }

    return result;
}

UNUSED_PARAMETER static void insert_extra_attr_int(sqlite3* handle, scope_entry_t* symbol, const char* name, sqlite3_uint64 value)
{
    sqlite3_bind_int64(_insert_extra_attr_stmt, 1, P2ULL(symbol));
    sqlite3_bind_text (_insert_extra_attr_stmt, 2, name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_extra_attr_stmt, 3, value);

    int result_query = sqlite3_step(_insert_extra_attr_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_extra_attr_stmt);
}

static void insert_extra_attr_data(sqlite3* handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_uint64 (*fun)(sqlite3* handle, void* data))
{
    sqlite3_uint64 m = fun(handle, data);

    sqlite3_bind_int64(_insert_extra_attr_stmt, 1, P2ULL(symbol));
    sqlite3_bind_text (_insert_extra_attr_stmt, 2, name, -1, SQLITE_STATIC);
    sqlite3_bind_int64(_insert_extra_attr_stmt, 3, m);

    int result_query = sqlite3_step(_insert_extra_attr_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_extra_attr_stmt);
}

static void insert_extra_gcc_attr(sqlite3* handle, scope_entry_t* symbol, const char *name, gather_gcc_attribute_t* gcc_attr)
{
    insert_ast(handle, nodecl_get_ast(gcc_attr->expression_list));
    char *name_and_tree = sqlite3_mprintf("%s|%llu", 
            gcc_attr->attribute_name,
            P2ULL(nodecl_get_ast(gcc_attr->expression_list)));

    sqlite3_bind_int64(_insert_extra_attr_stmt, 1, P2ULL(symbol));
    sqlite3_bind_text (_insert_extra_attr_stmt, 2, name,          -1, SQLITE_STATIC);
    sqlite3_bind_text (_insert_extra_attr_stmt, 3, name_and_tree, -1, SQLITE_STATIC);

    int result_query = sqlite3_step(_insert_extra_attr_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_extra_attr_stmt);
}

static void insert_extra_attr_symbol(sqlite3* handle, scope_entry_t* symbol, const char* name,
        scope_entry_t* ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_uint64(*)(sqlite3*, void*))(insert_symbol));
}

static void insert_extra_attr_type(sqlite3* handle, scope_entry_t* symbol, const char* name,
        type_t* ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_uint64(*)(sqlite3*, void*))(insert_type));
}

UNUSED_PARAMETER static void insert_extra_attr_ast(sqlite3* handle, scope_entry_t* symbol, const char* name,
        AST ref)
{
    insert_extra_attr_data(handle, symbol, name, ref, 
            (sqlite3_uint64(*)(sqlite3*, void*))(insert_ast));
}

UNUSED_PARAMETER
static void insert_extra_attr_nodecl(sqlite3* handle, scope_entry_t* symbol, const char* name,
        nodecl_t ref)
{
    insert_extra_attr_data(handle, symbol, name, nodecl_get_ast(ref), 
            (sqlite3_uint64(*)(sqlite3*, void*))(insert_ast));
}

static sqlite3_uint64 insert_default_argument_info_ptr(sqlite3* handle, void* p)
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

    P_LIST_ADD(p->syms, p->num_syms, load_symbol(p->handle, safe_atoull(attr_value)));

    return 0;
}

static int get_extra_types(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_types_t* p = (extra_types_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->types, p->num_types, load_type(p->handle, safe_atoull(attr_value)));

    return 0;
}

UNUSED_PARAMETER static int get_extra_trees(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_trees_t* p = (extra_trees_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->trees, p->num_trees, load_ast(p->handle, safe_atoull(attr_value)));

    return 0;
}

UNUSED_PARAMETER
static int get_extra_nodecls(void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_nodecls_t* p = (extra_nodecls_t*)datum;

    char *attr_value = values[0];

    P_LIST_ADD(p->nodecls, p->num_nodecls, load_nodecl(p->handle, safe_atoull(attr_value)));

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
    p->symbol->entity_specs.gcc_attributes = calloc(p->symbol->entity_specs.num_gcc_attributes, 
            sizeof(*p->symbol->entity_specs.gcc_attributes));
    p->symbol->entity_specs.gcc_attributes[p->symbol->entity_specs.num_gcc_attributes-1].attribute_name = uniquestr(attr_name);
    p->symbol->entity_specs.gcc_attributes[p->symbol->entity_specs.num_gcc_attributes-1].expression_list = 
        _nodecl_wrap(load_ast(p->handle, safe_atoull(tree)));

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
    d->argument = _nodecl_wrap(load_ast(p->handle, safe_atoull(values[0])));

    P_LIST_ADD(p->symbol->entity_specs.default_argument_info, p->symbol->entity_specs.num_parameters, d);

    return 0;
}

static void get_extended_attribute(sqlite3* handle, sqlite3_uint64 oid, const char* attr_name,
        void *extra_info,
        int (*get_extra_info_fun)(void *datum, int ncols, char **values, char **names))
{
    char * query = sqlite3_mprintf("SELECT value FROM attributes WHERE symbol = %llu AND name = " Q ";",
         oid, attr_name);

    char * errmsg = NULL;
    if (run_select_query(handle, query, get_extra_info_fun, extra_info, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    sqlite3_free(query);
}

static sqlite3_uint64 insert_scope(sqlite3* handle, scope_t* scope)
{
    if (scope == NULL)
        return 0;

    if (oid_already_inserted_scope(handle, scope))
    {
        return (sqlite3_uint64)(uintptr_t)scope;
    }

    char * insert_scope_query = sqlite3_mprintf("INSERT INTO scope(oid, kind, contained_in, related_entry) VALUES (%llu, %d, %llu, %llu);",
            P2ULL(scope),
            scope->kind,
            P2ULL(scope->contained_in),
            P2ULL(scope->related_entry));
    run_query(handle, insert_scope_query);
    sqlite3_free(insert_scope_query);

    sqlite3_uint64 oid = sqlite3_last_insert_rowid(handle);

    insert_symbol(handle, scope->related_entry);

    return oid;
}

static void insert_decl_context(sqlite3* handle, scope_entry_t* symbol, decl_context_t decl_context)
{
    char *insert_decl_context_query = sqlite3_mprintf("INSERT INTO decl_context (oid, " DECL_CONTEXT_FIELDS ") "
            "VALUES (%llu, %d, %llu, %llu, %llu, %llu, %llu, %llu, %llu);",
            P2ULL(symbol),
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

static sqlite3_uint64 insert_symbol(sqlite3* handle, scope_entry_t* symbol)
{
    if (symbol == NULL)
        return 0;

    if (oid_already_inserted_symbol(handle, symbol))
        return (sqlite3_uint64)(uintptr_t)symbol;

    char * insert_symbol_query = sqlite3_mprintf("INSERT INTO symbol(oid) "
            "VALUES (%llu);",
            P2ULL(symbol));

    run_query(handle, insert_symbol_query);
    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);

    char * attribute_values = symbol_get_attribute_values(handle, symbol);
    sqlite3_uint64 type_id = insert_type(handle, symbol->type_information);

    sqlite3_uint64 value_oid = insert_nodecl(handle, symbol->value);

    // We should be using UPDATE, but its syntax is so inconvenient here
    char * update_symbol_query = sqlite3_mprintf("INSERT OR REPLACE INTO symbol(oid, name, kind, type, file, line, value, %s) "
            "VALUES (%llu, " Q ", %d, %llu, " Q ", %d, %llu, %s);",
            attr_field_names,
            P2ULL(symbol), // oid
            symbol->symbol_name, // name
            symbol->kind, // kind
            type_id, // type
            symbol->file, // file
            symbol->line, // line
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

static nodecl_t load_nodecl(sqlite3* handle, sqlite3_uint64 oid);

static int get_symbol(void *datum, 
        int ncols,
        char **values, 
        char **names)
{
    symbol_handle_t* symbol_handle = (symbol_handle_t*)datum;

    sqlite3* handle = symbol_handle->handle;
    scope_entry_t** result = &(symbol_handle->symbol);

    sqlite3_uint64 oid = safe_atoull(values[0]);
    const char* name = values[1];
    int symbol_kind = safe_atoi(values[2]);
    sqlite3_uint64 type_oid = safe_atoull(values[3]);
    const char* filename = uniquestr(values[4]);
    int line = safe_atoi(values[5]);
    sqlite3_uint64 value_oid = safe_atoull(values[6]);

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

static char* safe_strdup(const char* c)
{
    if (c == NULL)
        return NULL;
    return strdup(c);
}

static scope_entry_t* load_symbol(sqlite3* handle, sqlite3_uint64 oid)
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

    // Bind the oid parameter
    sqlite3_bind_int64(_load_symbol_stmt, 1, oid);

    int result_query = sqlite3_step(_load_symbol_stmt);
    switch (result_query)
    {
        case SQLITE_ROW:
            {
                // OK
                break;
            }
        case SQLITE_DONE:
            {
                internal_error("Symbol with oid %llu not found\n", oid);
                break;
            }
        default:
            {
                internal_error("Unexpected error %d when running query '%s'", 
                        result_query,
                        sqlite3_errmsg(handle));
            }
    }

    int ncols = sqlite3_column_count(_load_symbol_stmt);
    char* values[ncols+1];
    char* names[ncols+1];

    int i;
    for (i = 0; i < ncols; i++)
    {
        values[i] = safe_strdup((const char*)sqlite3_column_text(_load_symbol_stmt, i));
        names[i] = safe_strdup((const char*)sqlite3_column_name(_load_symbol_stmt, i));
    }

    result_query = sqlite3_step(_load_symbol_stmt);
    switch (result_query)
    {
        case SQLITE_DONE:
            {
                // OK
                break;
            }
        case SQLITE_ROW:
            {
                // Too many!
                internal_error("Too many results from query of symbol oid %llu\n", oid);
                break;
            }
        default:
            {
                internal_error("Unexpected error when running query '%s'", 
                        sqlite3_errmsg(handle));
            }
    }

    // Release this prepared statement, from now we are reentrant
    sqlite3_reset(_load_symbol_stmt);

    symbol_handle_t symbol_handle;
    memset(&symbol_handle, 0, sizeof(symbol_handle));
    symbol_handle.handle = handle;

    get_symbol(&symbol_handle, ncols, (char**)values, (char**)names);

    for (i = 0; i < ncols; i++)
    {
        free(values[i]);
        free(names[i]);
    }

    return symbol_handle.symbol;
}


typedef
struct scope_info_tag
{
    sqlite3* handle;
    scope_t* scope;
} scope_info_t;

static scope_t* load_scope(sqlite3* handle, sqlite3_uint64 oid);

static int get_scope_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    scope_info_t* info = (scope_info_t*)datum;

    info->scope = _new_scope();

    insert_map_ptr(info->handle, safe_atoull(values[0]), info->scope);

    info->scope->kind = safe_atoull(values[1]);
    info->scope->contained_in = load_scope(info->handle, safe_atoull(values[2]));
    info->scope->related_entry = load_symbol(info->handle, safe_atoull(values[3]));

    return 0;
}

static scope_t* load_scope(sqlite3* handle, sqlite3_uint64 oid)
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
    char *query = sqlite3_mprintf("SELECT oid, kind, contained_in, related_entry FROM scope WHERE oid = %llu;\n", oid);
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

    p->decl_flags = safe_atoull(values[0]);
    p->namespace_scope = load_scope(handle, safe_atoull(values[1]));
    p->global_scope = load_scope(handle, safe_atoull(values[2]));
    p->block_scope = load_scope(handle, safe_atoull(values[3]));
    p->class_scope = load_scope(handle, safe_atoull(values[4]));
    p->function_scope = load_scope(handle, safe_atoull(values[5]));
    p->prototype_scope = load_scope(handle, safe_atoull(values[6]));
    p->current_scope = load_scope(handle, safe_atoull(values[7]));

    return 0;
}

static decl_context_t load_decl_context(sqlite3* handle, sqlite3_uint64 sym_oid)
{
    decl_context_t decl_context;
    memset(&decl_context, 0, sizeof(decl_context));

    if (sym_oid == 0)
        return decl_context;

    decl_context_t_info_t decl_context_info;
    decl_context_info.p_decl_context = &decl_context;
    decl_context_info.handle = handle;

    char *errmsg = NULL;
    char * query = sqlite3_mprintf("SELECT " DECL_CONTEXT_FIELDS " FROM decl_context WHERE oid = %llu;", sym_oid);
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

static AST load_ast(sqlite3* handle, sqlite3_uint64 oid);

static int get_ast(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    AST_query_handle_t *p = (AST_query_handle_t*)datum;
    sqlite3* handle = p->handle;

    sqlite3_uint64 oid = safe_atoull(values[0]);
    node_t node_kind = safe_atoull(values[1]);
    const char *filename = values[2];
    int line = safe_atoull(values[3]);
    const char* text = values[4];
    // Children: 5  + 0 -> 5 + MCXX_MAX_AST_CHILDREN - 1
    sqlite3_uint64 type_oid = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 0]);
    sqlite3_uint64 sym_oid = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 1]);
    char is_lvalue = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 2]);
    char is_const_val = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 3]);
    sqlite3_uint64 const_val = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 4]);
    // char is_value_dependent = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 5]);

    p->a = ASTLeaf(node_kind, filename, line, text);
    AST a = p->a;

    insert_map_ptr(handle, oid, a);

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        sqlite3_uint64 child_oid = safe_atoull(values[5 + i]);
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

static AST load_ast(sqlite3* handle, sqlite3_uint64 oid)
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
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent FROM ast WHERE oid = %llu;", oid);
    if (run_select_query(handle, select_ast_query, get_ast, &query_handle, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }
    sqlite3_free(select_ast_query);

    return query_handle.a;
}

static nodecl_t load_nodecl(sqlite3* handle, sqlite3_uint64 oid)
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

    sqlite3_uint64 current_oid = safe_atoull(values[0]);
    const char* kind = values[1];
    const char* cv_qualifier_name = values[2];
    int kind_size = safe_atoi(values[3]);
    sqlite3_uint64 ast0 = safe_atoull(values[4]);
    sqlite3_uint64 ast1 = safe_atoull(values[5]);
    sqlite3_uint64 ref = safe_atoull(values[6]);
    const char* types = values[7];
    const char* symbols = values[8];

    nodecl_t nodecl_fake = nodecl_make_text("", NULL, 0);

    if (strcmp(kind, "INTEGER") == 0)
    {
        *pt = choose_int_type_from_kind(nodecl_fake, kind_size);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "CHARACTER") == 0)
    {
        *pt = choose_character_type_from_kind(nodecl_fake, kind_size);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "REAL") == 0)
    {
        *pt = choose_float_type_from_kind(nodecl_fake, kind_size);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "LOGICAL") == 0)
    {
        *pt = choose_logical_type_from_kind(nodecl_fake, kind_size);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "COMPLEX") == 0)
    {
        *pt = get_complex_type(choose_float_type_from_kind(nodecl_fake, kind_size));
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "POINTER") == 0)
    {
        *pt = get_pointer_type(load_type(handle, ref));
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "REFERENCE") == 0)
    {
        *pt = get_lvalue_reference_type(load_type(handle, ref));
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
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
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "CLASS") == 0)
    {
        char *copy = strdup(symbols);

        *pt = get_new_class_type(CURRENT_COMPILED_FILE->global_decl_context, TT_STRUCT);

        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);

        char *context = NULL;
        char *field = strtok_r(copy, ",", &context);
        while (field != NULL)
        {
            scope_entry_t* member = load_symbol(handle, safe_atoull(field));

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

            parameter_info[num_parameters].type_info = load_type(handle, safe_atoull(field));

            num_parameters++;
            field = strtok_r(NULL, ",", &context);
        }
        free(copy);

        type_t* result = load_type(handle, ref);

        *pt = get_new_function_type(result, parameter_info, num_parameters);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "VOID") == 0)
    {
        *pt = get_void_type();
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "NAMED") == 0)
    {
        sqlite3_uint64 symbol_oid = safe_atoull(symbols);

        scope_entry_t* symbol = load_symbol(handle, symbol_oid);

        *pt = get_user_defined_type(symbol);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else if (strcmp(kind, "INDIRECT") == 0)
    {
        sqlite3_uint64 symbol_oid = safe_atoull(symbols);

        scope_entry_t* symbol = load_symbol(handle, symbol_oid);

        *pt = get_indirect_type(symbol);
        *pt = get_qualified_type_with_cv_qualifier_name(*pt, cv_qualifier_name);
        insert_map_ptr(handle, current_oid, *pt);
    }
    else
    {
        internal_error("Invalid type '%s'\n", kind);
    }

    return 0;
}

static type_t* load_type(sqlite3* handle, sqlite3_uint64 oid)
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
    char * select_type_query = sqlite3_mprintf("SELECT oid, kind, cv_qualifier, kind_size, ast0, ast1, ref_type, "
            "types, symbols FROM type WHERE oid = %llu;", oid);
    if (run_select_query(handle, select_type_query, get_type, &type_handle, &errmsg) != SQLITE_OK)
    {
        running_error("Error while running query: %s\n", errmsg);
    }

    return type_handle.type;
}

static sqlite3_uint64 insert_single_const_value(sqlite3* handle, const_value_t* v, const char* kind, int sign, int bytes, const char* literal_value)
{
    char * insert_value = sqlite3_mprintf("INSERT INTO const_value(oid, kind, sign, bytes, literal_value, compound_values) "
            "VALUES (%llu, " Q ", %d, %d, " Q ", NULL);",
            P2ULL(v),
            kind,
            sign,
            bytes,
            literal_value);
    run_query(handle, insert_value);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_value);
    return result;
}

static sqlite3_uint64 insert_multiple_const_value(sqlite3* handle, const_value_t* v, const char* kind)
{
    int num_elems = const_value_get_num_elements(v);
    int i;

    sqlite3_uint64 num_elements_oid[num_elems + 1];
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
            list = sqlite3_mprintf("%s,%llu", old_list, num_elements_oid[i]);
            sqlite3_free(old_list);
        }
        else
        {
            list = sqlite3_mprintf("%llu", num_elements_oid[i]);
        }
    }

    char* insert_values = sqlite3_mprintf("INSERT INTO const_value(oid, kind, sign, bytes, literal_value, compound_values) "
            "VALUES(%llu, " Q ", 0, 0, NULL, " Q ");",
            P2ULL(v),
            kind,
            list);

    run_query(handle, insert_values);
    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_values);
    sqlite3_free(list);

    return result;
}

#define FLOAT_FORMAT_STR "%.24f"
#define DOUBLE_FORMAT_STR "%.53f"
#define LONG_DOUBLE_FORMAT_STR "%.113Le"

static sqlite3_uint64 insert_const_value(sqlite3* handle, const_value_t* value)
{
    if (oid_already_inserted_const_value(handle, value))
        return (sqlite3_uint64)(uintptr_t)value;

    // Some floats can be really large
    char float_literal_value[2048] = { 0 };

    if (const_value_is_integer(value))
    {
        char * literal_value = sqlite3_mprintf("%llu", const_value_cast_to_8(value));

        sqlite3_uint64 result = insert_single_const_value(handle, value, 
                "INTEGER", 
                const_value_is_signed(value),
                const_value_get_bytes(value),
                literal_value);

        sqlite3_free(literal_value);
        return result;

    }
    else if (const_value_is_float(value))
    {
        snprintf(float_literal_value, 2047, FLOAT_FORMAT_STR, const_value_cast_to_float(value));
        float_literal_value[2047] = '\0';

        sqlite3_uint64 result = insert_single_const_value(handle, value, 
                "FLOAT", 
                0, 0,
                float_literal_value);

        return result;
    }
    else if (const_value_is_double(value))
    {
        snprintf(float_literal_value, 2047, DOUBLE_FORMAT_STR, const_value_cast_to_double(value));
        float_literal_value[2047] = '\0';

        sqlite3_uint64 result = insert_single_const_value(handle, value, 
                "DOUBLE", 
                0, 0,
                float_literal_value);

        return result;
    }
    else if (const_value_is_long_double(value))
    {
        snprintf(float_literal_value, 2047, LONG_DOUBLE_FORMAT_STR, const_value_cast_to_long_double(value));
        float_literal_value[2047] = '\0';

        sqlite3_uint64 result = insert_single_const_value(handle, value, 
                "LONG DOUBLE", 
                0, 0,
                float_literal_value);

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
    else if (const_value_is_range(value))
    {
        return insert_multiple_const_value(handle, value, "VALUE");
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

    sqlite3_uint64 oid = safe_atoull(values[0]);
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
            || (strcmp(kind, "COMPLEX") == 0)
            || (strcmp(kind, "RANGE") == 0))
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

            int i = 0;
            field = strtok_r(copy, ",", &context);
            while (field != NULL)
            {
                const_value_t* const_value = load_const_value(p->handle, safe_atoull(field));
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
        else if (strcmp(kind, "RANGE") == 0)
        {
            ERROR_CONDITION(num_elems != 3, "Invalid range constant!", 0);

            p->v = const_value_make_range(list[0], list[1], list[2]);
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

static const_value_t* load_const_value(sqlite3* handle, sqlite3_uint64 oid)
{
    void *p = get_ptr_of_oid(handle, oid);
    if (p != NULL)
    {
        return (const_value_t*)p;
    }

    char * select_const_value = sqlite3_mprintf("SELECT oid, kind, sign, bytes, literal_value, compound_values FROM const_value WHERE oid = %llu\n;",
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

static void dispose_storage(sqlite3* handle)
{
    int i;
    for (i = 0; _prepared_statements_registry[i] != NULL; i++)
    {
        sqlite3_finalize(*(_prepared_statements_registry[i]));
        *(_prepared_statements_registry[i]) = NULL;
    }

    if (sqlite3_close(handle) != SQLITE_OK)
    {
        running_error("Error while closing database (%s)\n", sqlite3_errmsg(handle));
    }
}


#ifdef DEBUG_SQLITE3_MPRINTF
 #error Disable DEBUG_SQLITE3_MPRINTF macro once no warnings for sqlite3_mprintf calls are signaled by gcc
#endif
