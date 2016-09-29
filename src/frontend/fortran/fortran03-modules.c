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

#include "fortran03-modules.h"
#include "fortran03-modules-data.h"
#include "fortran03-buildscope.h"
#include "cxx-limits.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "fortran03-typeutils.h"
#include "fortran03-scope.h"
#include "fortran03-intrinsics.h"
#include "cxx-exprtype.h"
#include "cxx-driver-build-info.h"
#include "cxx-driver-utils.h"
#include "cxx-driver-fortran.h"
#include "cxx-entrylist.h"
#include "cxx-asttype-str.h"

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

enum { CURRENT_MODULE_VERSION = 18 };

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

static void create_storage(sqlite3**, scope_entry_t*);
static void init_storage(sqlite3*);
static void dispose_storage(sqlite3*);
static void prepare_statements(sqlite3*);

static void start_transaction(sqlite3*);
static void end_transaction(sqlite3*);

UNUSED_PARAMETER
static const char* full_name_of_symbol(scope_entry_t* entry)
{
    if (entry == NULL)
    {
        return UNIQUESTR_LITERAL("<<NULL>>");
    }

    const char* result = NULL;
    if (symbol_entity_specs_get_in_module(entry))
    {
        if (symbol_entity_specs_get_from_module(entry) != NULL)
        {
            uniquestr_sprintf(&result, "%s.%s -> %s", 
                    symbol_entity_specs_get_in_module(entry)->symbol_name,
                    entry->symbol_name,
                    full_name_of_symbol(symbol_entity_specs_get_alias_to(entry)));
        }
        else
        {
            uniquestr_sprintf(&result, "%s.%s", 
                    symbol_entity_specs_get_in_module(entry)->symbol_name,
                    entry->symbol_name);
        }
    }
    else
    {
        if (symbol_entity_specs_get_from_module(entry) != NULL)
        {
            uniquestr_sprintf(&result, "%s -> %s", 
                    entry->symbol_name,
                    full_name_of_symbol(symbol_entity_specs_get_alias_to(entry)));
        }
        else
        {
            uniquestr_sprintf(&result, "%s", entry->symbol_name);
        }
    }

    return result;
}

static sqlite3_uint64 insert_symbol(sqlite3* handle, scope_entry_t* symbol);
static sqlite3_uint64 insert_type(sqlite3* handle, type_t* t);

static type_t* load_type(sqlite3* handle, sqlite3_uint64 oid);
static scope_entry_t* load_symbol(sqlite3* handle, sqlite3_uint64 oid);
static AST load_ast(sqlite3* handle, sqlite3_uint64 oid);
static nodecl_t load_nodecl(sqlite3* handle, sqlite3_uint64 oid);

static void load_extra_data_from_module(sqlite3* handle, scope_entry_t* module);

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
static void insert_extra_function_parameter_info(sqlite3* handle, scope_entry_t* symbol, 
        const char *name, function_parameter_info_t* parameter_info);
static void insert_extra_gcc_attr(sqlite3* handle, scope_entry_t* symbol, const char *name, 
        gcc_attribute_t* gcc_attr);
static void insert_extra_attr_data(sqlite3* handle, scope_entry_t* symbol, const char* name, void* data,
        sqlite3_uint64 (*fun)(sqlite3* handle, void* data));
static sqlite3_uint64 insert_default_argument_info_ptr(sqlite3* handle, void* p);
static char query_contains_field(int ncols, char** names, const char* field_name, int *result);
static void run_query(sqlite3* handle, const char* query);
static const decl_context_t* load_decl_context(sqlite3* handle, sqlite3_uint64 oid);

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

typedef
enum type_kind_table_tag
{
    TKT_INVALID = 0,
    TKT_INTEGER,
    TKT_CHARACTER,
    TKT_LOGICAL,
    TKT_REAL,
    TKT_COMPLEX,
    TKT_POINTER,
    TKT_REFERENCE,
    TKT_FUNCTION,
    TKT_NONPROTOTYPE_FUNCTION,
    TKT_ARRAY,
    TKT_ARRAY_DESCRIPTOR,
    TKT_CLASS,
    TKT_VOID,
    TKT_INDIRECT,
    TKT_NAMED,
    TKT_COMPUTED_FUNCTION
} type_kind_table_t;

typedef
enum const_kind_table_tag
{
    CKT_INVALID = 0,
    CKT_INTEGER,
    CKT_FLOAT,
    CKT_DOUBLE,
    CKT_LONG_DOUBLE,
    CKT_COMPLEX,
    CKT_STRUCT,
    CKT_STRING,
    CKT_ARRAY,
    CKT_VECTOR,
    CKT_RANGE,
} const_kind_table_t;

struct module_info_tag
{
    const char* module_name;
    const char* date;
    int version;
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

static int get_extra_function_parameter_info(void *datum, 
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

static scope_entry_t* module_being_emitted = NULL;
static sqlite3_uint64 module_oid_being_loaded = 0;

static rb_red_blk_tree * _oid_map = NULL;

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
    create_storage(&handle, module);

    start_transaction(handle);

    init_storage(handle);

    module_being_emitted = module;
    sqlite3_uint64 module_oid = insert_symbol(handle, module);
    module_being_emitted = NULL;

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

static void null_dtor_func(const void *v UNUSED_PARAMETER) { }

static int int64cmp_vptr(const void* ptr1, const void* ptr2)
{
    sqlite3_uint64 u1 = *(sqlite3_uint64*)ptr1;
    sqlite3_uint64 u2 = *(sqlite3_uint64*)ptr2;

    if (u1 < u2)
        return -1;
    else if (u1 > u2)
        return 1;
    else
        return 0;
}

static void load_storage(sqlite3** handle, const char* filename)
{
    sqlite3_uint64 result = sqlite3_open(filename, handle);

    if (result != SQLITE_OK)
    {
        fatal_error("Error while opening module database '%s' (%s)\n", filename, sqlite3_errmsg(*handle));
    }

    _oid_map = rb_tree_create(int64cmp_vptr, null_dtor_func, null_dtor_func);
}

void load_module_info(const char* module_name, scope_entry_t** module)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: Loading module '%s'\n", module_name);
    }

    ERROR_CONDITION(module == NULL, "Invalid parameter", 0);
    *module = NULL;

    const char *filename = NULL, *wrap_filename = NULL; 
    driver_fortran_retrieve_module(module_name, &filename, &wrap_filename);

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

    module_info_t minfo;
    memset(&minfo, 0, sizeof(minfo));

    get_module_info(handle, &minfo);

    if (minfo.version != CURRENT_MODULE_VERSION)
    {
        fatal_error("Module file '%s' is not compatible with this version of Mercurium (got version %d but expected version %d)\n",
                filename, minfo.version, CURRENT_MODULE_VERSION);
    }

    prepare_statements(handle);

    start_transaction(handle);

    module_oid_being_loaded = minfo.module_oid;
    *module = load_symbol(handle, minfo.module_oid);
    module_oid_being_loaded = 0;

    load_extra_data_from_module(handle, *module);

    end_transaction(handle);

    dispose_storage(handle);

    timing_end(&timing_load_module);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Module '%s' loaded in %.2f seconds\n", 
                module_name,
                timing_elapsed(&timing_load_module));
    }

    if (module != NULL
            && wrap_filename != NULL)
    {
        if (!symbol_entity_specs_get_is_builtin((*module)))
        {
            P_LIST_ADD(CURRENT_COMPILED_FILE->module_files_to_hide,
                    CURRENT_COMPILED_FILE->num_module_files_to_hide,
                    wrap_filename);
        }
    }

}

static void create_storage(sqlite3** handle, scope_entry_t* module)
{
    const char* filename = NULL;
    driver_fortran_register_module(module->symbol_name, &filename, 
            /* is_intrinsic */ symbol_entity_specs_get_is_builtin(module));

    DEBUG_CODE()
    {
        fprintf(stderr, "FORTRAN-MODULES: File used will be '%s'\n", filename);
    }

    // Make sure the file has been removed
    if (access(filename, F_OK) == 0)
    {
        if (remove(filename) != 0)
        {
            fatal_error("Error while removing old module '%s' (%s)\n", filename, strerror(errno));
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
        fatal_error("Error during query: %s\nQuery was: %s\n", *errmsg, query);
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
        const char * create_info = "CREATE TABLE info(INTEGER oid PRIMARY KEY, module, date, version, build, root_symbol);";
        run_query(handle, create_info);
    }

    {
        const char * create_string_table = "CREATE TABLE string_table(INTEGER oid PRIMARY KEY, string, UNIQUE(string));";
        run_query(handle, create_string_table);
    }

    {
        char * create_symbol = sqlite3_mprintf("CREATE TABLE symbol(INTEGER oid PRIMARY KEY, decl_context, name, kind, type, file, line, "
                "value, bit_entity_specs, related_decl_context, %s);", attr_field_names);
        run_query(handle, create_symbol);
        sqlite3_free(create_symbol);
    }

    {
        const char * create_attributes = "CREATE TABLE attributes(INTEGER oid PRIMARY KEY, name, symbol, value);";
        run_query(handle, create_attributes);

        // This index is crucial for fast loading of attributes of symbols
        const char * create_attr_index = "CREATE INDEX attributes_index ON attributes (symbol, name);";
        run_query(handle, create_attr_index);
    }

    {
        const char * create_types = "CREATE TABLE type(INTEGER oid PRIMARY KEY, kind, cv_qualifier, kind_size, ast0, ast1, ref_type, types, symbols);";
        run_query(handle, create_types);
    }

    {
        const char * create_ast = "CREATE TABLE ast(INTEGER oid PRIMARY KEY, kind, file, line, text, ast0, ast1, ast2, ast3, "
            "type, symbol, is_lvalue, is_const_val, const_val, is_value_dependent);";
        run_query(handle, create_ast);
    }

    {
        const char * create_context = "CREATE TABLE decl_context(INTEGER oid PRIMARY KEY, " DECL_CONTEXT_FIELDS ");";
        run_query(handle, create_context);

        const char * create_decl_context_index = "CREATE INDEX decl_context_index ON decl_context ( " DECL_CONTEXT_FIELDS " );";
        run_query(handle, create_decl_context_index);
    }

    {
        const char * create_context = "CREATE TABLE scope(INTEGER oid PRIMARY KEY, kind, contained_in, related_entry);";
        run_query(handle, create_context);
    }

    {
        const char* create_const_value = "CREATE TABLE const_value(INTEGER oid PRIMARY KEY, kind, raw_oid, struct_type);";
        run_query(handle, create_const_value);
    }

    {
        const char* create_const_value = "CREATE TABLE raw_const_value(INTEGER oid PRIMARY KEY, raw_bytes, UNIQUE(raw_bytes));";
        run_query(handle, create_const_value);
    }

    {
        const char* create_const_multivalue = "CREATE TABLE multi_const_value(INTEGER oid PRIMARY KEY, oid_object, oid_part);";
        run_query(handle, create_const_multivalue);

        const char * create_attr_index = "CREATE INDEX multi_const_value_index ON multi_const_value(oid_object);";
        run_query(handle, create_attr_index);
    }

    {
        const char* create_module_extra_name = "CREATE TABLE module_extra_name(name, PRIMARY KEY(name));";
        run_query(handle, create_module_extra_name);

        const char* create_module_extra_data = "CREATE TABLE module_extra_data(oid_name, order_, kind, value, PRIMARY KEY (oid_name, order_));";
        run_query(handle, create_module_extra_data);
    }
}

static sqlite3_uint64 run_insert_statement(sqlite3* handle, sqlite3_stmt* stmt)
{
    int result_query = sqlite3_step(stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(stmt);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);

    return result;
}

// List here all the prepared statements
#define PREPARED_STATEMENT_LIST \
    PREPARED_STATEMENT(_load_symbol_stmt) \
    PREPARED_STATEMENT(_oid_already_inserted_type) \
    PREPARED_STATEMENT(_oid_already_inserted_ast) \
    PREPARED_STATEMENT(_oid_already_inserted_scope) \
    PREPARED_STATEMENT(_oid_already_inserted_symbol) \
    PREPARED_STATEMENT(_oid_already_inserted_const_value) \
    PREPARED_STATEMENT(_oid_already_inserted_decl_context) \
    PREPARED_STATEMENT(_pre_insert_symbol_stmt) \
    PREPARED_STATEMENT(_insert_type_simple_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_list_types_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_list_symbols_stmt) \
    PREPARED_STATEMENT(_insert_type_ref_to_ast_stmt) \
    PREPARED_STATEMENT(_insert_ast_stmt) \
    PREPARED_STATEMENT(_insert_extra_attr_stmt) \
    PREPARED_STATEMENT(_insert_string_stmt) \
    PREPARED_STATEMENT(_insert_scope_stmt) \
    PREPARED_STATEMENT(_pre_insert_decl_context_stmt) \
    PREPARED_STATEMENT(_insert_decl_context_stmt) \
    PREPARED_STATEMENT(_insert_const_value_stmt) \
    PREPARED_STATEMENT(_insert_raw_const_value_stmt) \
    PREPARED_STATEMENT(_check_raw_const_value_stmt) \
    PREPARED_STATEMENT(_insert_multi_const_value_stmt) \
    PREPARED_STATEMENT(_insert_multi_const_value_part_stmt) \
    PREPARED_STATEMENT(_get_extended_attr_stmt) \
    PREPARED_STATEMENT(_select_string_stmt) \
    PREPARED_STATEMENT(_select_scope_stmt) \
    PREPARED_STATEMENT(_select_decl_context_stmt) \
    PREPARED_STATEMENT(_get_current_scope_of_decl_context_stmt) \
    PREPARED_STATEMENT(_select_ast_stmt) \
    PREPARED_STATEMENT(_select_type_stmt) \
    PREPARED_STATEMENT(_select_const_value_stmt) \
    PREPARED_STATEMENT(_select_raw_const_value_stmt) \
    PREPARED_STATEMENT(_select_multi_const_value_count) \
    PREPARED_STATEMENT(_select_multi_const_value_parts)

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

    char* load_symbol_stmt_str = sqlite3_mprintf(
            "SELECT s.oid, decl_context, str1.string AS name, str2.string AS kind, type, str3.string AS file, line,"
            " value, bit_entity_specs, related_decl_context, %s "
            "FROM symbol s, string_table str1, string_table str2, string_table str3 WHERE s.oid = $OID AND str1.oid = s.name AND str2.oid = s.kind AND str3.oid = s.file;", 
            attr_field_names);
    DO_PREPARE_STATEMENT(_load_symbol_stmt, load_symbol_stmt_str);
    sqlite3_free(load_symbol_stmt_str);

    // Already inserted statements
    DO_PREPARE_STATEMENT(_oid_already_inserted_type,        "SELECT oid FROM type WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_ast,         "SELECT oid FROM ast WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_scope,       "SELECT oid FROM scope WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_symbol,      "SELECT oid FROM symbol WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_const_value, "SELECT oid FROM const_value WHERE oid = $OID;");
    DO_PREPARE_STATEMENT(_oid_already_inserted_decl_context, "SELECT oid FROM decl_context WHERE oid = $OID;");

    // String table
    DO_PREPARE_STATEMENT(_insert_string_stmt, "INSERT INTO string_table(string) VALUES($NAME);");
    DO_PREPARE_STATEMENT(_select_string_stmt, "SELECT oid FROM string_table WHERE string = $NAME;");

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

    // Pre insert symbol
    DO_PREPARE_STATEMENT(_pre_insert_symbol_stmt, "INSERT INTO symbol(oid) VALUES ($OID);");

    // Insert extra attrs
    DO_PREPARE_STATEMENT(_insert_extra_attr_stmt, 
            "INSERT INTO attributes(symbol, name, value) VALUES($SYMBOL, $NAME, $VALUE);");

    // Insert scope
    DO_PREPARE_STATEMENT(_insert_scope_stmt, 
            "INSERT INTO scope(oid, kind, contained_in, related_entry) VALUES ($OID, $KIND, $CONTAINED, $ENTRY);");

    // Insert decl context
    DO_PREPARE_STATEMENT(_pre_insert_decl_context_stmt,
            "INSERT INTO decl_context (oid) VALUES (?);");
    DO_PREPARE_STATEMENT(_insert_decl_context_stmt,
            "INSERT OR REPLACE INTO decl_context (oid, " DECL_CONTEXT_FIELDS ") "
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);");

    // Const value
    DO_PREPARE_STATEMENT(_insert_const_value_stmt, "INSERT INTO const_value(oid, raw_oid) "
            "VALUES ($OID, $RAWOID);");

    // Raw values
    DO_PREPARE_STATEMENT(_insert_raw_const_value_stmt, "INSERT INTO raw_const_value(raw_bytes) "
            "VALUES ($RAWBYTES);");
    DO_PREPARE_STATEMENT(_check_raw_const_value_stmt, 
            "SELECT r.oid FROM raw_const_value r WHERE r.raw_bytes = $RAWBYTES;");

    // Multi const values
    DO_PREPARE_STATEMENT(_insert_multi_const_value_stmt, 
            "INSERT INTO const_value(oid, kind, struct_type) "
            "VALUES($OID, $KIND, $STRUCTTYPE);");
    DO_PREPARE_STATEMENT(_insert_multi_const_value_part_stmt, 
            "INSERT INTO multi_const_value(oid_object, oid_part) "
            "VALUES ($OBJECT, $PART);");

    // Get extended attr
    DO_PREPARE_STATEMENT(_get_extended_attr_stmt, 
            "SELECT a.value FROM attributes a, string_table str "
            "WHERE a.symbol = $SYMBOL AND a.name = str.oid AND str.string = $NAME;");

    DO_PREPARE_STATEMENT(_select_scope_stmt, 
            "SELECT oid, kind, contained_in, related_entry FROM scope WHERE oid = $OID;");

    DO_PREPARE_STATEMENT(_select_decl_context_stmt, "SELECT oid, " DECL_CONTEXT_FIELDS " FROM decl_context WHERE oid = $OID;");

    DO_PREPARE_STATEMENT(_get_current_scope_of_decl_context_stmt, "SELECT current_scope FROM decl_context WHERE oid = $OID;");

    DO_PREPARE_STATEMENT(_select_ast_stmt, "SELECT a.oid, str0.string AS kind, str1.string AS file, a.line, str2.string AS text, a.ast0, a.ast1, a.ast2, a.ast3, "
            "a.type, a.symbol, a.is_lvalue, a.is_const_val, a.const_val, a.is_value_dependent "
            "FROM ast a, string_table str0, string_table str1, string_table str2 WHERE a.oid = $OID AND a.kind = str0.oid AND a.file = str1.oid AND a.text = str2.oid;");

    DO_PREPARE_STATEMENT(_select_type_stmt, "SELECT oid, kind, cv_qualifier, kind_size, ast0, ast1, ref_type, "
            "types, symbols FROM type WHERE oid = $OID;");

    DO_PREPARE_STATEMENT(_select_const_value_stmt,
            "SELECT c.oid, c.kind, c.raw_oid, c.struct_type FROM const_value c "
            "WHERE c.oid = $OID;");

    DO_PREPARE_STATEMENT(_select_raw_const_value_stmt,
            "SELECT r.raw_bytes FROM raw_const_value r "
            "WHERE r.oid = $OID;");

    DO_PREPARE_STATEMENT(_select_multi_const_value_count,
            "SELECT COUNT(*) FROM multi_const_value WHERE oid_object = $OID\n;");

    DO_PREPARE_STATEMENT(_select_multi_const_value_parts,
            "SELECT oid_part FROM multi_const_value WHERE oid_object = $OID\n;");

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

    _oid_map = rb_tree_create(int64cmp_vptr, null_dtor_func, null_dtor_func);
}

static int get_module_info_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    module_info_t* p = (module_info_t*)datum;
    p->module_name = values[0];
    p->date = values[1];
    p->version = safe_atoull(values[2]);
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
        fatal_error("Error during query: %s\nQuery was: %s\n", errmsg, module_info_query);
    }
}

static void finish_module_file(sqlite3* handle, const char* module_name, sqlite3_uint64 module_symbol)
{
    char* insert_info = sqlite3_mprintf("INSERT INTO info(module, date, version, build, root_symbol) "
            "VALUES(" Q ", DATE(), %d, " Q ", %llu);", module_name, CURRENT_MODULE_VERSION, MCXX_BUILD_VERSION, 
            (long long int)module_symbol);
    run_query(handle, insert_info);
    sqlite3_free(insert_info);
}

static void* get_ptr_of_oid(sqlite3* handle UNUSED_PARAMETER, sqlite3_uint64 oid)
{
    ERROR_CONDITION(oid == 0, "Invalid zero OID", 0);

    rb_red_blk_node * n = rb_tree_query(_oid_map, &oid);

    void * p = NULL;
    if (n != NULL)
    {
        p = rb_node_get_info(n);
    }
    return p;
}

static void insert_map_ptr(sqlite3* handle UNUSED_PARAMETER, sqlite3_uint64 oid, void *ptr)
{
    sqlite3_int64* p = NEW0(sqlite3_int64);
    *p = oid;

    rb_tree_insert(_oid_map, p, ptr);
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

static char oid_already_inserted_decl_context(sqlite3* handle, void *ptr);
DEF_OID_ALREADY_INSERTED(decl_context);

static sqlite3_uint64 insert_string_in_string_table(sqlite3* handle, 
        const char* str)
{
    sqlite3_bind_text (_insert_string_stmt, 1, str, -1, SQLITE_STATIC);

    int result_query = sqlite3_step(_insert_string_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_string_stmt);

    sqlite3_uint64 result = sqlite3_last_insert_rowid(handle);
    return result;
}

static sqlite3_uint64 get_oid_from_string_table(sqlite3* handle,
        const char* str)
{
    // Simplify null strings to empty ones
    if (str == NULL)
        str = "";


    sqlite3_bind_text(_select_string_stmt, 1, str, -1, SQLITE_STATIC);
    int result_query = sqlite3_step(_select_string_stmt);

    sqlite3_uint64 result_oid = 0;

    switch (result_query)
    {
        case SQLITE_ROW:
            {
                // OK
                result_oid = sqlite3_column_int64(_select_string_stmt, 0);
                break;
            }
        case SQLITE_DONE:
            {
                // Continue
                break;
            }
        default:
            {
                internal_error("Unexpected error %d when running query '%s'", 
                        result_query,
                        sqlite3_errmsg(handle));
            }
    }

    sqlite3_reset(_select_string_stmt);

    if (result_oid == 0)
    {
        result_oid = insert_string_in_string_table(handle, str);
    }
    ERROR_CONDITION(result_oid == 0, "Invalid result oid", 0);

    return result_oid;
}

static sqlite3_uint64 insert_type_simple(sqlite3* handle, type_t* t, 
        type_kind_table_t type_kind_name, 
        sqlite3_uint64 kind_size)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);

    sqlite3_bind_int64(_insert_type_simple_stmt, 1, P2ULL(t));
    sqlite3_bind_int64(_insert_type_simple_stmt, 2, type_kind_name);
    sqlite3_bind_int64(_insert_type_simple_stmt, 3, cv_qualif);
    sqlite3_bind_int64(_insert_type_simple_stmt, 4, kind_size);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_type_simple_stmt);
    return result;
}

static sqlite3_uint64 insert_type_ref_to(sqlite3* handle, type_t* t, type_kind_table_t name, sqlite3_int64 ref_type)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);
    
    sqlite3_bind_int64(_insert_type_ref_to_stmt, 1, P2ULL(t));
    sqlite3_bind_int64(_insert_type_ref_to_stmt, 2, name);
    sqlite3_bind_int64(_insert_type_ref_to_stmt, 3, cv_qualif);
    sqlite3_bind_int64(_insert_type_ref_to_stmt, 4, ref_type);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_type_ref_to_stmt);
    return result;
}

static sqlite3_uint64 insert_type_ref_to_list_types(sqlite3* handle, 
        type_t* t,
        type_kind_table_t name, 
        sqlite3_uint64 ref_type, 
        sqlite3_uint64 num_parameters, 
        sqlite3_uint64 *parameter_types)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);
    
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
    sqlite3_bind_int64(_insert_type_ref_to_list_types_stmt, 2, name);
    sqlite3_bind_int64(_insert_type_ref_to_list_types_stmt, 3, cv_qualif);
    sqlite3_bind_int64(_insert_type_ref_to_list_types_stmt, 4, ref_type);
    sqlite3_bind_text (_insert_type_ref_to_list_types_stmt, 5, list, -1, SQLITE_STATIC);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_type_ref_to_list_types_stmt);
    sqlite3_free(list);

    return result;
}

static sqlite3_uint64 insert_type_ref_to_list_symbols(sqlite3* handle, 
        type_t* t,
        type_kind_table_t name,
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

    cv_qualifier_t cv_qualifier = get_cv_qualifier(t);
    
    sqlite3_bind_int64(_insert_type_ref_to_list_symbols_stmt, 1, P2ULL(t));
    sqlite3_bind_int64(_insert_type_ref_to_list_symbols_stmt, 2, name);
    sqlite3_bind_int64(_insert_type_ref_to_list_symbols_stmt, 3, cv_qualifier);
    sqlite3_bind_int64(_insert_type_ref_to_list_symbols_stmt, 4, ref_type);
    sqlite3_bind_text (_insert_type_ref_to_list_symbols_stmt, 5, list, -1, SQLITE_STATIC);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_type_ref_to_list_symbols_stmt);
    sqlite3_free(list);
    return result;
}

static sqlite3_uint64 insert_type_ref_to_symbol(sqlite3* handle, 
        type_t* t,
        type_kind_table_t name,
        sqlite3_uint64 ref_type,
        sqlite3_uint64 symbol)
{
    return insert_type_ref_to_list_symbols(handle, t, name, ref_type, 1, &symbol);
}

static sqlite3_uint64 insert_type_ref_to_ast(sqlite3* handle, 
        type_t* t,
        type_kind_table_t name, 
        sqlite3_uint64 ref_type, 
        sqlite3_uint64 ast0, 
        sqlite3_uint64 ast1)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    if (oid_already_inserted_type(handle, t))
        return (sqlite3_uint64)(uintptr_t)t;

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);
    
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 1, P2ULL(t));
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 2, name);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 3, cv_qualif);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 4, ref_type);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 5, ast0);
    sqlite3_bind_int64(_insert_type_ref_to_ast_stmt, 6, ast1);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_type_ref_to_ast_stmt);

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

    type_t* type = nodecl_get_type(_nodecl_wrap(a));
    scope_entry_t* sym = nodecl_get_symbol(_nodecl_wrap(a));

    if (type != NULL)
    {
        insert_type(handle, type);
    }
    if (sym != NULL)
    {
        insert_symbol(handle, sym);
    }

    char is_const_val = nodecl_is_constant(_nodecl_wrap(a));
    sqlite3_uint64 const_val = 0;
    if (is_const_val)
    {
        const_val = insert_const_value(handle, nodecl_get_constant(_nodecl_wrap(a)));
    }

    if (oid_already_inserted_ast(handle, a))
        return (sqlite3_uint64)(uintptr_t)a;

    char is_value_dependent = nodecl_expr_is_value_dependent(_nodecl_wrap(a));

    // 1
    sqlite3_bind_int64(_insert_ast_stmt, 1, P2ULL(a));
    sqlite3_bind_int64(_insert_ast_stmt, 2, get_oid_from_string_table(handle, ast_print_node_type(ast_get_kind(a))));
    sqlite3_bind_int64(_insert_ast_stmt, 3, get_oid_from_string_table(handle, ast_get_filename(a)));
    sqlite3_bind_int  (_insert_ast_stmt, 4, ast_get_line(a));
    sqlite3_bind_int64(_insert_ast_stmt, 5, get_oid_from_string_table(handle, ast_get_text(a)));
    sqlite3_bind_int64(_insert_ast_stmt, 6, children[0]);
    sqlite3_bind_int64(_insert_ast_stmt, 7, children[1]);
    sqlite3_bind_int64(_insert_ast_stmt, 8, children[2]);
    sqlite3_bind_int64(_insert_ast_stmt, 9, children[3]);
    // 2
    sqlite3_bind_int64(_insert_ast_stmt, 10, P2ULL(type));
    sqlite3_bind_int64(_insert_ast_stmt, 11, P2ULL(sym));
    sqlite3_bind_int  (_insert_ast_stmt, 12, 0); // Removed, kept here for historical reasons
    sqlite3_bind_int  (_insert_ast_stmt, 13, is_const_val);
    sqlite3_bind_int64(_insert_ast_stmt, 14, const_val);
    sqlite3_bind_int  (_insert_ast_stmt, 15, is_value_dependent);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_ast_stmt);

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
        result = insert_type_simple(handle, t, TKT_INTEGER, type_get_size(t));
    }
    else if (is_character_type(t))
    {
        result = insert_type_simple(handle, t, TKT_CHARACTER, type_get_size(t));
    }
    else if (is_bool_type(t))
    {
        result = insert_type_simple(handle, t, TKT_LOGICAL, type_get_size(t));
    }
    else if (is_floating_type(t))
    {
        result = insert_type_simple(handle, t, TKT_REAL, type_get_size(t));
    }
    else if (is_complex_type(t))
    {
        result = insert_type_simple(handle, t, TKT_COMPLEX, type_get_size(complex_type_get_base_type(t)));
    }
    else if (is_pointer_type(t))
    {
        result = insert_type(handle, pointer_type_get_pointee_type(t));
        result = insert_type_ref_to(handle, t, TKT_POINTER, result);
    }
    else if (is_lvalue_reference_type(t))
    {
        result = insert_type(handle, reference_type_get_referenced_type(t));
        result = insert_type_ref_to(handle, t, TKT_REFERENCE, result);
    }
    else if (is_function_type(t))
    {
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

        type_kind_table_t function_kind_type = TKT_FUNCTION;

        if (function_type_get_lacking_prototype(t))
        {
            function_kind_type = TKT_NONPROTOTYPE_FUNCTION;
        }
        result = insert_type_ref_to_list_types(handle, t, function_kind_type, result, num_parameters, parameter_types);
    }
    else if (fortran_is_character_type(t)
            || fortran_is_array_type(t))
    {
        sqlite3_uint64 lower_tree = insert_nodecl(handle, array_type_get_array_lower_bound(t));
        sqlite3_uint64 upper_tree = insert_nodecl(handle, array_type_get_array_upper_bound(t));

        sqlite3_uint64 element_type = insert_type(handle, array_type_get_element_type(t));


        type_kind_table_t kind = TKT_ARRAY;
        if (array_type_with_descriptor(t))
            kind = TKT_ARRAY_DESCRIPTOR;

        result = insert_type_ref_to_ast(handle, t, kind, element_type, lower_tree, upper_tree);
    }
    else if (is_unnamed_class_type(t))
    {
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

        result = insert_type_ref_to_list_symbols(handle, t, TKT_CLASS, 0, num_fields, field_list);
    }
    else if (is_void_type(t))
    {
        result = insert_type_simple(handle, t, TKT_VOID, 0);
    }
    else if (is_named_type(t))
    {
        sqlite3_uint64 sym_oid = insert_symbol(handle, named_type_get_symbol(t));

        if (is_indirect_type(t))
        {
            result = insert_type_ref_to_symbol(handle, t, TKT_INDIRECT, 0, sym_oid);
        }
        else
        {
            result = insert_type_ref_to_symbol(handle, t, TKT_NAMED, 0, sym_oid);
        }
    }
    else if (is_computed_function_type(t))
    {
        int id = fortran_intrinsic_get_id(computed_function_type_get_computing_function(t));
        ERROR_CONDITION((id == -1), "Attempt to store an unknown computed function type %p",
                computed_function_type_get_computing_function(t));

        result = insert_type_simple(handle, t, TKT_COMPUTED_FUNCTION, id);
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
    sqlite3_bind_int64(_insert_extra_attr_stmt, 2, get_oid_from_string_table(handle, name));
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
    sqlite3_bind_int64(_insert_extra_attr_stmt, 2, get_oid_from_string_table(handle, name));
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

static void insert_extra_function_parameter_info(sqlite3* handle, scope_entry_t* symbol, const char *name, 
        function_parameter_info_t* parameter_info)
{
    sqlite3_int64 function_id = insert_symbol(handle, parameter_info->function);
    char *function_and_position = sqlite3_mprintf("%llu|%d",
            P2ULL(function_id),
            parameter_info->position);

    sqlite3_bind_int64(_insert_extra_attr_stmt, 1, P2ULL(symbol));
    sqlite3_bind_int64(_insert_extra_attr_stmt, 2, get_oid_from_string_table(handle, name));
    sqlite3_bind_text (_insert_extra_attr_stmt, 3, function_and_position, -1, SQLITE_STATIC);

    int result_query = sqlite3_step(_insert_extra_attr_stmt);
    if (result_query != SQLITE_DONE)
    {
        internal_error("Unexpected error %d when running query '%s'", 
                result_query,
                sqlite3_errmsg(handle));
    }

    sqlite3_reset(_insert_extra_attr_stmt);
}

static void insert_extra_gcc_attr(sqlite3* handle, scope_entry_t* symbol, const char *name, gcc_attribute_t* gcc_attr)
{
    insert_ast(handle, nodecl_get_ast(gcc_attr->expression_list));
    char *name_and_tree = sqlite3_mprintf("%s|%llu", 
            gcc_attr->attribute_name,
            P2ULL(nodecl_get_ast(gcc_attr->expression_list)));

    sqlite3_bind_int64(_insert_extra_attr_stmt, 1, P2ULL(symbol));
    sqlite3_bind_int64(_insert_extra_attr_stmt, 2, get_oid_from_string_table(handle, name));
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
    // We cannot currently store the const decl_context_t*
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

    char *attr_value = xstrdup(values[0]);

    char *q = strchr(attr_value, '|');
    ERROR_CONDITION(p == NULL, "Wrong field!", 0);
    *q = '\0';

    const char* attr_name = attr_value;
    const char* tree = q+1;

    gcc_attribute_t gcc_attr;
    memset(&gcc_attr, 0, sizeof(gcc_attr));

    gcc_attr.attribute_name = uniquestr(attr_name);
    gcc_attr.expression_list = 
        _nodecl_wrap(load_ast(p->handle, safe_atoull(tree)));

    symbol_entity_specs_add_gcc_attributes(p->symbol, gcc_attr);

    DELETE(attr_value);

    return 0;
}

typedef
struct extra_function_parameter_info_tag
{
    sqlite3* handle;
    function_parameter_info_t *pf;
} extra_function_parameter_info_t;

static int get_extra_function_parameter_info_only(
        void *datum, 
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_function_parameter_info_t* p = (extra_function_parameter_info_t*)datum;

    char *attr_value = xstrdup(values[0]);

    char *q = strchr(attr_value, '|');
    ERROR_CONDITION(p == NULL, "Wrong field!", 0);
    *q = '\0';

    const char* function_id_str = attr_value;
    const char* position_str = q+1;

    sqlite3_uint64 function_id = 0;
    int position = -1;

    sscanf(function_id_str, "%llu", &function_id);
    sscanf(position_str, "%d", &position);

    scope_entry_t* function_symbol = load_symbol(p->handle, function_id);

    p->pf->function = function_symbol;
    p->pf->nesting = 0;
    p->pf->position = position;

    DELETE(attr_value);

    return 0;
}

static int get_extra_function_parameter_info(void *datum, 
        int ncols,
        char **values, 
        char **names)
{
    function_parameter_info_t parameter_info;
    memset(&parameter_info, 0, sizeof(parameter_info));

    extra_gcc_attrs_t* p = (extra_gcc_attrs_t*)datum;
    extra_function_parameter_info_t ef = { p->handle, &parameter_info };

    get_extra_function_parameter_info_only(&ef, ncols, values, names);

    symbol_entity_specs_add_function_parameter_info(p->symbol, parameter_info);
    return 0;
}

static int get_extra_default_argument_info(void *datum,
        int ncols UNUSED_PARAMETER,
        char **values, 
        char **names UNUSED_PARAMETER)
{
    extra_default_argument_info_t* p = (extra_default_argument_info_t*)datum;

    default_argument_info_t* d = NEW0(default_argument_info_t);
    // We are not storing the context yet
    d->context = CURRENT_COMPILED_FILE->global_decl_context;
    d->argument = _nodecl_wrap(load_ast(p->handle, safe_atoull(values[0])));

    symbol_entity_specs_add_default_argument_info(p->symbol,
        d);

    return 0;
}

typedef
struct sql_parameter_info_tag
{
    int ncols;
    char** values;
    char** names;
} sql_parameter_info_t;

static void free_param_info(sql_parameter_info_t *param_info, int num_rows)
{
    int i;
    for (i = 0; i < num_rows; i++)
    {
        DELETE(param_info[i].values);
        DELETE(param_info[i].names);
    }
    DELETE(param_info);
}

static char* safe_strdup(const char* c)
{
    if (c == NULL)
        return NULL;
    return xstrdup(c);
}

static int run_select_query_prepared(sqlite3* handle, sqlite3_stmt* prepared_stmt, 
        int (*fun)(void* datum, int ncols, char** values, char **names),
        void *datum,
        const char** errmsg)
{
    // This function avoids reentrancy issues caused by prepared statements
    //
    // The whole records are kept in a temporary buffer and then the callback is called per each row

    sql_parameter_info_t *param_info = NULL;
    int num_rows = 0;
    int result_set_size = 4;

    param_info = NEW_REALLOC(sql_parameter_info_t, param_info, result_set_size);

    int result_query = sqlite3_step(prepared_stmt);
    while(result_query != SQLITE_DONE)
    {
        switch (result_query)
        {
            case SQLITE_ROW:
                {
                    num_rows++;
                    if (num_rows > result_set_size)
                    {
                        result_set_size *= 2;
                        param_info = NEW_REALLOC(sql_parameter_info_t, param_info, result_set_size);
                    }

                    int current_row = num_rows - 1;

                    int ncols = sqlite3_column_count(prepared_stmt);

                    param_info[current_row].ncols = ncols;

                    param_info[current_row].values = NEW_VEC0(char*, ncols);
                    param_info[current_row].names = NEW_VEC0(char*, ncols);
                    int i;
                    for (i = 0; i < ncols; i++)
                    {
                        param_info[current_row].values[i] = safe_strdup((const char*)sqlite3_column_text(prepared_stmt, i));
                        param_info[current_row].names[i] = safe_strdup((const char*)sqlite3_column_name(prepared_stmt, i));
                    }

                    break;
                }
            case SQLITE_DONE:
                {
                    break;
                }
            default:
                {
                    *errmsg = sqlite3_errmsg(handle);
                    free_param_info(param_info, num_rows);
                    sqlite3_reset(prepared_stmt);
                    return result_query;
                }
        }
        result_query = sqlite3_step(prepared_stmt);
    }

    sqlite3_reset(prepared_stmt);

    // Run the callback
    int i;
    for (i = 0; i < num_rows; i++)
    {
        fun(datum, param_info[i].ncols, param_info[i].values, param_info[i].names);
    }

    free_param_info(param_info, num_rows);

    *errmsg = NULL;
    return SQLITE_OK;
}

static void get_extended_attribute(sqlite3* handle, sqlite3_uint64 oid, const char* attr_name,
        void *extra_info,
        int (*get_extra_info_fun)(void *datum, int ncols, char **values, char **names))
{
    sqlite3_bind_int64(_get_extended_attr_stmt, 1, oid);
    sqlite3_bind_text (_get_extended_attr_stmt, 2, attr_name, -1, SQLITE_STATIC);

    const char * errmsg = NULL;
    if (run_select_query_prepared(handle, _get_extended_attr_stmt, get_extra_info_fun, extra_info, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error while running query: %s\n", errmsg);
    }
}

static sqlite3_uint64 insert_scope(sqlite3* handle, scope_t* scope)
{
    if (scope == NULL)
        return 0;

    if (oid_already_inserted_scope(handle, scope))
    {
        return (sqlite3_uint64)(uintptr_t)scope;
    }
 
    sqlite3_bind_int64(_insert_scope_stmt, 1, P2ULL(scope));
    sqlite3_bind_int  (_insert_scope_stmt, 2, scope->kind);
    sqlite3_bind_int64(_insert_scope_stmt, 3, P2ULL(scope->contained_in));
    sqlite3_bind_int64(_insert_scope_stmt, 4, P2ULL(scope->related_entry));

    sqlite3_uint64 oid = run_insert_statement(handle, _insert_scope_stmt);

    insert_symbol(handle, scope->related_entry);

    return oid;
}

static sqlite3_uint64 insert_decl_context(sqlite3* handle, const decl_context_t* decl_context)
{
    if (decl_context == NULL)
        return 0;

    if (oid_already_inserted_decl_context(handle, (void*)decl_context))
    {
        return (sqlite3_uint64)(uintptr_t)decl_context;
    }

    sqlite3_bind_int64(_pre_insert_decl_context_stmt, 1, P2ULL(decl_context));
    sqlite3_uint64 decl_context_oid = run_insert_statement(handle, _pre_insert_decl_context_stmt);

    sqlite3_uint64 namespace_scope = insert_scope(handle, decl_context->namespace_scope);
    sqlite3_uint64 global_scope = insert_scope(handle, decl_context->global_scope);
    sqlite3_uint64 block_scope = insert_scope(handle, decl_context->block_scope);
    sqlite3_uint64 class_scope = insert_scope(handle, decl_context->class_scope);
    sqlite3_uint64 function_scope = insert_scope(handle, decl_context->function_scope);
    sqlite3_uint64 prototype_scope = insert_scope(handle, decl_context->prototype_scope);
    sqlite3_uint64 current_scope = insert_scope(handle, decl_context->current_scope);

    sqlite3_bind_int64(_insert_decl_context_stmt, 1, decl_context_oid);
    sqlite3_bind_int  (_insert_decl_context_stmt, 2, decl_context->decl_flags);
    sqlite3_bind_int64(_insert_decl_context_stmt, 3, namespace_scope);
    sqlite3_bind_int64(_insert_decl_context_stmt, 4, global_scope);
    sqlite3_bind_int64(_insert_decl_context_stmt, 5, block_scope);
    sqlite3_bind_int64(_insert_decl_context_stmt, 6, class_scope);
    sqlite3_bind_int64(_insert_decl_context_stmt, 7, function_scope);
    sqlite3_bind_int64(_insert_decl_context_stmt, 8, prototype_scope);
    sqlite3_bind_int64(_insert_decl_context_stmt, 9, current_scope);

    /* sqlite3_uint64 decl_context_oid = */ run_insert_statement(handle, _insert_decl_context_stmt);

    return decl_context_oid;
}

static const char* module_packed_bits_to_hexstr(module_packed_bits_t module_packed_bits)
{
    const char* c = "";
    unsigned char* p = (unsigned char*)&module_packed_bits;

    unsigned int i;
    for (i = 0; i < sizeof(module_packed_bits); i++)
    {
        const char* t = NULL;
        uniquestr_sprintf(&t, "%02x", p[i]);
        c = strappend(c, t);
    }

    return c;
}

module_packed_bits_t module_packed_bits_from_hexstr(const char* c)
{
    module_packed_bits_t module_packed_bits;
    unsigned char* p = (unsigned char*)&module_packed_bits;

    ERROR_CONDITION(strlen(c) != sizeof(module_packed_bits) * 2, "Invalid string", 0);

    unsigned int i;
    for (i = 0; i < sizeof(module_packed_bits); i++)
    {
        char t[3];
        t[0] = c[2*i];
        t[1] = c[2*i+1];
        t[2] = '\0';

        unsigned int v = 0;

        sscanf(t, "%x", &v);

        p[i] = (unsigned char)v;
    }

    return module_packed_bits;
}

static sqlite3_uint64 insert_symbol(sqlite3* handle, scope_entry_t* symbol)
{
    if (symbol == NULL)
        return 0;

    if (oid_already_inserted_symbol(handle, symbol))
        return (sqlite3_uint64)(uintptr_t)symbol;

    sqlite3_bind_int64(_pre_insert_symbol_stmt, 1, P2ULL(symbol));
    sqlite3_uint64 result = run_insert_statement(handle, _pre_insert_symbol_stmt);

    sqlite3_uint64 type_id = insert_type(handle, symbol->type_information);
    sqlite3_uint64 value_oid = insert_nodecl(handle, symbol->value);
    sqlite3_uint64 decl_context_oid = insert_decl_context(handle, symbol->decl_context);
    sqlite3_uint64 related_decl_context_oid = insert_decl_context(handle, symbol->related_decl_context);

    // module_packed_bits_t is declared in fortran03-modules-bits.h
    module_packed_bits_t module_packed_bits = synthesize_packed_bits(symbol);
    const char* bit_str = module_packed_bits_to_hexstr(module_packed_bits);

    char * attribute_values = symbol_get_attribute_values(handle, symbol);
    // FIXME - Devise ways to make this a prepared statement
    char * update_symbol_query = sqlite3_mprintf("INSERT OR REPLACE INTO symbol(oid, decl_context, name, kind, type, file, line, value, "
            "bit_entity_specs, related_decl_context, %s) "
            "VALUES (%llu, %llu, %llu, %llu, %llu, %llu, %u, %llu, " Q ", %llu, %s);",
            attr_field_names,
            P2ULL(symbol), // oid
            decl_context_oid, // decl_context
            get_oid_from_string_table(handle, symbol->symbol_name), // name
            get_oid_from_string_table(handle, symbol_kind_to_str(symbol->kind)), // kind
            type_id, // type
            get_oid_from_string_table(handle, locus_get_filename(symbol->locus)), // file
            locus_get_line(symbol->locus), // line
            value_oid,
            bit_str,
            related_decl_context_oid,
            attribute_values);
    sqlite3_free(attribute_values);

    run_query(handle, update_symbol_query);
    sqlite3_free(update_symbol_query);

    // fprintf(stderr, "-> INSERTING SYMBOL -> %p %s%s%s\n",
    //         symbol,
    //         symbol_entity_specs_get_in_module(symbol) != NULL ? symbol_entity_specs_get_in_module(symbol)->symbol_name : "",
    //         symbol_entity_specs_get_in_module(symbol) != NULL ? "." : "",
    //         symbol->symbol_name
    //         );

    if (symbol->kind == SK_MODULE
            && symbol != module_being_emitted)
    {
        // We leave module symbols empty, to avoid dragging everything into the module file
        // fprintf(stderr, "NOT INSERTING EXTRA DATA OF SYMBOL -> '%s'\n", symbol->symbol_name);
    }
    else
    {
        // fprintf(stderr, "INSERTING EXTRA DATA OF SYMBOL -> '%s%s%s'\n",
        //     symbol_entity_specs_get_in_module(symbol) != NULL ? symbol_entity_specs_get_in_module(symbol)->symbol_name : "",
        //     symbol_entity_specs_get_in_module(symbol) != NULL ? "." : "",
        //     symbol->symbol_name);
        insert_extended_attributes(handle, symbol);
    }

    // fprintf(stderr, "<- END INSERTING SYMBOL -> %s%s%s\n",
    //         symbol_entity_specs_get_in_module(symbol) != NULL ? symbol_entity_specs_get_in_module(symbol)->symbol_name : "",
    //         symbol_entity_specs_get_in_module(symbol) != NULL ? "." : "",
    //         symbol->symbol_name
    //         );

    return result;
}

typedef struct 
{
    sqlite3* handle;
    scope_entry_t* symbol;
} symbol_handle_t;

static nodecl_t load_nodecl(sqlite3* handle, sqlite3_uint64 oid);
static scope_t* load_scope(sqlite3* handle, sqlite3_uint64 oid);
static sqlite3_uint64 get_current_scope_oid_of_decl_context_oid(sqlite3* handle, sqlite3_uint64 decl_context_oid);

static int get_symbol(void *datum, 
        int ncols,
        char **values, 
        char **names)
{
    symbol_handle_t* symbol_handle = (symbol_handle_t*)datum;

    sqlite3* handle = symbol_handle->handle;
    scope_entry_t** result = &(symbol_handle->symbol);

    sqlite3_uint64 oid = safe_atoull(values[0]);
    sqlite3_uint64 decl_context_oid = safe_atoull(values[1]);
    const char* name = uniquestr(values[2]);
    const char* symbol_kind_str = uniquestr(values[3]);
    sqlite3_uint64 type_oid = safe_atoull(values[4]);
    const char* filename = uniquestr(values[5]);
    int line = safe_atoi(values[6]);
    sqlite3_uint64 value_oid = safe_atoull(values[7]);
    const char* bitfield_pack_str = uniquestr(values[8]);
    sqlite3_uint64 related_decl_context_oid = safe_atoull(values[9]);

    int symbol_kind = symbol_str_to_kind(symbol_kind_str);
    ERROR_CONDITION(symbol_kind == SK_UNDEFINED, "Invalid symbol '%s' loaded from module\n", symbol_kind_str);

    (*result) = NULL;

    // We need to load the bits for the early checks in the loaded symbol
    // see below
    module_packed_bits_t packed_bits = module_packed_bits_from_hexstr(bitfield_pack_str);

    // Early checks to use already loaded symbols
    if (symbol_kind == SK_MODULE)
    {
        rb_red_blk_node* query = rb_tree_query(CURRENT_COMPILED_FILE->module_file_cache, strtolower(name));
        // Check if this symbol is in the cache and reuse it 
        if (query != NULL)
        {
            // fprintf(stderr, "HIT FOR module '%s' (OID=%llu)\n", strtolower(name), oid);
            scope_entry_t* module_symbol = (scope_entry_t*)rb_node_get_info(query);
            (*result) = module_symbol;
            insert_map_ptr(handle, oid, (*result));

            if (oid != module_oid_being_loaded)
            {
                // If this is not the module being loaded, use the cached symbol
                return 0;
            }
            // otherwise continue loading it
        }
        else
        {
            // fprintf(stderr, "MISS FOR module '%s' (OID=%llu)\n", strtolower(name), oid);
        }
    }

    // Is this symbol in a module?
    int i;
    if (query_contains_field(ncols, names, "in_module", &i))
    {
        // Get the module
        scope_entry_t* in_module = load_symbol(handle, safe_atoull(values[i]));

        scope_entry_t* from_module = NULL;
        scope_entry_t* alias_to = NULL;
        if (query_contains_field(ncols, names, "from_module", &i))
        {
            from_module = load_symbol(handle, safe_atoull(values[i]));
            if (query_contains_field(ncols, names, "alias_to", &i))
            {
                alias_to = load_symbol(handle, safe_atoull(values[i]));
            }
        }

        if (in_module != NULL)
        {
            for (i = 0; i < symbol_entity_specs_get_num_related_symbols(in_module); i++)
            {
                scope_entry_t* member = symbol_entity_specs_get_related_symbols_num(in_module, i);
                if (strcasecmp(member->symbol_name, name) == 0
                        && member->kind == (enum cxx_symbol_kind)symbol_kind
                        && symbol_entity_specs_get_from_module(member) == from_module
                        && symbol_entity_specs_get_alias_to(member) == alias_to)
                {
                    (*result) = member;
                    insert_map_ptr(handle, oid, (*result));
                    return 0;
                }
            }
            // fprintf(stderr, "SYMBOL %lld '%s.%s' IS NOT ALREADY LOADED IN ITS MODULE\n", 
            //         oid,
            //         in_module->symbol_name, name);
        }
    }

    {
        function_parameter_info_t function_parameter_info;
        memset(&function_parameter_info, 0, sizeof(function_parameter_info));

        extra_function_parameter_info_t ef = { handle, &function_parameter_info };
        get_extended_attribute(handle, oid, "function_parameter_info",
                &ef, get_extra_function_parameter_info_only);
        if (function_parameter_info.function != NULL
                && symbol_entity_specs_get_in_module(function_parameter_info.function))
        {
            // If this is a dummy argument, reuse the existing symbol if any
            if (function_parameter_info.position < symbol_entity_specs_get_num_related_symbols(
                        function_parameter_info.function))
            {
                scope_entry_t* param = symbol_entity_specs_get_related_symbols_num(
                        function_parameter_info.function,
                        function_parameter_info.position);

                ERROR_CONDITION(param == NULL, "This cannot be NULL", 0);

                (*result) = param;
                insert_map_ptr(handle, oid, *result);
                return 0;
            }
        }
    }

    if (*result == NULL)
    {
        (*result) = NEW0(scope_entry_t);
    }

    insert_map_ptr(handle, oid, *result);

    (*result)->symbol_name = name;
    (*result)->kind = symbol_kind;
    (*result)->locus = make_locus(filename, line, 0);

    // static int level = 0;
    // {
    //     scope_entry_t* sym = *result;
    //     fprintf(stderr, "%d -> (OID=%llu) LOADING SYMBOL '%s' (%s) with PTR %llu\n", 
    //             level++,
    //             oid,
    //             sym->symbol_name,
    //             symbol_kind_name(sym),
    //             P2ULL(sym));
    // }

    // Unpack bits into the symbol
    unpack_bits(*result, packed_bits);
    get_extra_attributes(handle, ncols, values, names, oid, *result);

    (*result)->type_information = load_type(handle, type_oid);

    (*result)->decl_context = load_decl_context(handle, decl_context_oid);

    (*result)->related_decl_context = load_decl_context(handle, related_decl_context_oid);

    // Add it to its scope if it has name
    if ((*result)->symbol_name != NULL)
    {
        // Now by accessing (*result)->decl_context->current_scope we would
        // expose a cycle: (*result)->decl_context may have not been fully
        // loaded yet at this point, so what we have to do is to get the oid of
        // the current scope and get it, this will give us the correct scope
        scope_t* current_scope =
            load_scope(handle,
                    get_current_scope_oid_of_decl_context_oid(handle, decl_context_oid)
                    );

        insert_entry(current_scope, (*result));
    }

    (*result)->value = load_nodecl(handle, value_oid);


    // Classes require a bit more of work
    if ((*result)->kind == SK_CLASS)
    {
        // Avoid cycle (see explanation above)
        decl_context_t* fixed_decl_context = decl_context_clone((*result)->decl_context);
        fixed_decl_context->current_scope =
            load_scope(handle,
                    get_current_scope_oid_of_decl_context_oid(handle, decl_context_oid)
                    );

        const decl_context_t* class_context = new_class_context(fixed_decl_context, *result);
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
            insert_entry(class_context->current_scope, field);

            // Update field context
            field->decl_context = class_context;
        }
        entry_list_iterator_free(it);
        entry_list_free(members);
    }

    // This is a (top-level) module. Keep in the module symbol cache
    if ((*result)->kind == SK_MODULE)
    {
        rb_tree_insert(CURRENT_COMPILED_FILE->module_file_cache, strtolower((*result)->symbol_name), (*result));

        if (module_oid_being_loaded == oid)
        {
            // A module is defined once it is loaded
            (*result)->defined = 1;
        }
    }

    // Is this symbol in a module?
    if (query_contains_field(ncols, names, "in_module", &i))
    {
        // Get the module
        scope_entry_t* in_module = load_symbol(handle, safe_atoull(values[i]));

        // And add the current symbol (if not added yet)
        if (in_module != NULL)
        {
            // fprintf(stderr, "SYMBOL '%s' IS '%s.%s'\n", (*result)->symbol_name,
            //         in_module->symbol_name,
            //         (*result)->symbol_name);

            symbol_entity_specs_add_related_symbols(in_module, *result);
        }
    }

    // {
    //     scope_entry_t* sym = *result;
    //     fprintf(stderr, "%d <- (OID=%llu) FINISHED LOADING SYMBOL '%s' (%s) with PTR %llu\n", 
    //             --level,
    //             oid,
    //             sym->symbol_name,
    //             symbol_kind_name(sym),
    //             P2ULL(sym));
    // }

    return 0;
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
    memset(values, 0, sizeof(values));
    char* names[ncols+1];
    memset(names, 0, sizeof(names));

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
        DELETE(values[i]);
        DELETE(names[i]);
    }

    return symbol_handle.symbol;
}


typedef
struct scope_info_tag
{
    sqlite3* handle;
    scope_t* scope;
} scope_info_t;

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

    sqlite3_bind_int64(_select_scope_stmt, 1, oid);
    const char *errmsg = NULL;

    if (run_select_query_prepared(handle, _select_scope_stmt, get_scope_, &info, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error while running query: %s\n", errmsg);
    }

    return info.scope;
}

typedef
struct decl_context_info_tag
{
    sqlite3* handle;
    const decl_context_t* decl_context;
} decl_context_t_info_t;


static int get_current_scope_oid_of_decl_context_oid_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    sqlite3_uint64* oid = (sqlite3_uint64*)datum;

    *oid = safe_atoull(values[0]);

    return 0;
}

static sqlite3_uint64 get_current_scope_oid_of_decl_context_oid(sqlite3* handle, sqlite3_uint64 decl_context_oid)
{
    if (decl_context_oid == 0)
        return 0;

    sqlite3_uint64 result_oid = 0;

    const char *errmsg = NULL;
    sqlite3_bind_int64(_get_current_scope_of_decl_context_stmt, 1, decl_context_oid);
    if (run_select_query_prepared(handle, _get_current_scope_of_decl_context_stmt,
                get_current_scope_oid_of_decl_context_oid_,
                &result_oid, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error while running query: %s\n", errmsg);
    }

    return result_oid;
}

static int get_decl_context_(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    decl_context_t_info_t* decl_context_info = (decl_context_t_info_t*)datum;

    sqlite3* handle = decl_context_info->handle;
    sqlite3_uint64 oid = safe_atoull(values[0]);
    
    const decl_context_t* ptr = (const decl_context_t*)get_ptr_of_oid(handle, oid);
    if (ptr != NULL)
    {
        decl_context_info->decl_context = ptr;
        return 0;
    }

    decl_context_t* p = NULL;
    decl_context_info->decl_context = p = decl_context_empty();
    insert_map_ptr(handle, oid, p);

    p->decl_flags = safe_atoull(values[1]);
    p->namespace_scope = load_scope(handle, safe_atoull(values[2]));
    p->global_scope = load_scope(handle, safe_atoull(values[3]));
    p->block_scope = load_scope(handle, safe_atoull(values[4]));
    p->class_scope = load_scope(handle, safe_atoull(values[5]));
    p->function_scope = load_scope(handle, safe_atoull(values[6]));
    p->prototype_scope = load_scope(handle, safe_atoull(values[7]));
    p->current_scope = load_scope(handle, safe_atoull(values[8]));

    return 0;
}

static const decl_context_t* load_decl_context(sqlite3* handle, sqlite3_uint64 decl_context_oid)
{
    if (decl_context_oid == 0)
        return NULL;

    decl_context_t_info_t decl_context_info;
    decl_context_info.decl_context = NULL;
    decl_context_info.handle = handle;

    const char *errmsg = NULL;
    sqlite3_bind_int64(_select_decl_context_stmt, 1, decl_context_oid);
    if (run_select_query_prepared(handle, _select_decl_context_stmt, get_decl_context_, &decl_context_info, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error while running query: %s\n", errmsg);
    }

    return decl_context_info.decl_context;
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
    const char* node_kind_str = uniquestr(values[1]);
    const char *filename = uniquestr(values[2]);
    int line = safe_atoull(values[3]);
    const char* text = uniquestr(values[4]);
    // Children: 5  + 0 -> 5 + MCXX_MAX_AST_CHILDREN - 1
    sqlite3_uint64 type_oid = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 0]);
    sqlite3_uint64 sym_oid = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 1]);
    // char is_lvalue = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 2]);
    char is_const_val = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 3]);
    sqlite3_uint64 const_val = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 4]);
    // char is_value_dependent = safe_atoull(values[5 + MCXX_MAX_AST_CHILDREN + 5]);

    node_t node_kind = ast_node_name_to_kind(node_kind_str);
    ERROR_CONDITION(node_kind == AST_INVALID_NODE, "Invalid node '%s' loaded from module\n", 
            node_kind_str);

    p->a = ASTLeaf(node_kind, make_locus(filename, line, 0), text);
    AST a = p->a;

    insert_map_ptr(handle, oid, a);

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        sqlite3_uint64 child_oid = safe_atoull(values[5 + i]);
        AST child_tree = load_ast(handle, child_oid);

        ast_set_child(a, i, child_tree);
    }

    if (type_oid != 0)
    {
        nodecl_set_type(_nodecl_wrap(a), load_type(handle, type_oid));
    }

    scope_entry_t* entry = NULL;
    if (sym_oid != 0)
    {
        entry = load_symbol(handle, sym_oid);
        ERROR_CONDITION(entry == NULL, "INVALID!", 0);

        nodecl_set_symbol(_nodecl_wrap(a), entry);
    }

    if (is_const_val)
    {
        const_value_t* v = load_const_value(handle, const_val);
        nodecl_set_constant(_nodecl_wrap(a), v);
    }

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

    const char *errmsg = NULL;
    sqlite3_bind_int64(_select_ast_stmt, 1, oid);
    if (run_select_query_prepared(handle, _select_ast_stmt, get_ast, &query_handle, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error while running query: %s\n", errmsg);
    }

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

static type_t* type_t_oid(sqlite3* handle, uint64_t oid)
{
    type_t* ptr = (type_t*)get_ptr_of_oid(handle, oid);
    if (ptr != NULL)
    {
        return ptr;
    }
    return NULL;
}

static int get_type(void *datum, 
        int ncols UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    sqlite3* handle = ((type_handle_t*)datum)->handle;
    type_t** pt = &((type_handle_t*)datum)->type;

    sqlite3_uint64 current_oid = safe_atoull(values[0]);
    sqlite3_uint64 kind = safe_atoull(values[1]);
    sqlite3_uint64 cv_qualifier = safe_atoull(values[2]);
    int kind_size = safe_atoi(values[3]);
    sqlite3_uint64 ast0 = safe_atoull(values[4]);
    sqlite3_uint64 ast1 = safe_atoull(values[5]);
    sqlite3_uint64 ref = safe_atoull(values[6]);
    const char* types = values[7];
    const char* symbols = values[8];

    nodecl_t nodecl_fake = nodecl_make_text("", make_locus("", 0, 0));


    switch (kind)
    {
        case TKT_INTEGER:
        {
            *pt = choose_int_type_from_kind(nodecl_fake, kind_size);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_CHARACTER:
        {
            *pt = choose_character_type_from_kind(nodecl_fake, kind_size);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_REAL:
        {
            *pt = choose_float_type_from_kind(nodecl_fake, kind_size);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_LOGICAL:
        {
            *pt = choose_logical_type_from_kind(nodecl_fake, kind_size);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_COMPLEX:
        {
            *pt = get_complex_type(choose_float_type_from_kind(nodecl_fake, kind_size));
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_POINTER:
        {
            *pt = get_pointer_type(load_type(handle, ref));
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_REFERENCE:
        {
            *pt = get_lvalue_reference_type(load_type(handle, ref));
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_ARRAY:
        case TKT_ARRAY_DESCRIPTOR:
        {
            nodecl_t lower_bound = load_nodecl(handle, ast0);
            nodecl_t upper_bound = load_nodecl(handle, ast1);

            type_t* element_type = load_type(handle, ref);

            // At the moment we do not store the decl_context
            // Hopefully this will be enough
            const decl_context_t* decl_context = CURRENT_COMPILED_FILE->global_decl_context;
            if (kind == TKT_ARRAY)
            {
                *pt = get_array_type_bounds(element_type,
                        lower_bound, upper_bound, decl_context);
            }
            else if (kind == TKT_ARRAY_DESCRIPTOR)
            {
                *pt = get_array_type_bounds_with_descriptor(element_type,
                        lower_bound, upper_bound, decl_context);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_CLASS:
        {
            char *copy = xstrdup(symbols);

            *pt = get_new_class_type(CURRENT_COMPILED_FILE->global_decl_context, TT_STRUCT);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);

            // All classes are complete in fortran!
            set_is_complete_type(*pt, /* is_complete */ 1);

            char *context = NULL;
            char *field = strtok_r(copy, ",", &context);
            while (field != NULL)
            {
                scope_entry_t* member = load_symbol(handle, safe_atoull(field));

                ERROR_CONDITION(member == NULL, "Invalid member!\n", 0);
                class_type_add_member(*pt, member,
                        member->decl_context,
                        /* is_definition */ 1); // This mutates *pt

                field = strtok_r(NULL, ",", &context);
            }
            DELETE(copy);
            break;
        }
        case TKT_FUNCTION:
        case TKT_NONPROTOTYPE_FUNCTION:
        {
            int num_parameters = 0;
            parameter_info_t parameter_info[MCXX_MAX_FUNCTION_PARAMETERS];
            memset(parameter_info, 0, sizeof(parameter_info));

            if (types != NULL)
            {
                char *copy = xstrdup(types);

                char *context = NULL;
                char *field = strtok_r(copy, ",", &context);
                while (field != NULL)
                {
                    ERROR_CONDITION(num_parameters == MCXX_MAX_FUNCTION_PARAMETERS, "Too many parameters %d", num_parameters);

                    parameter_info[num_parameters].type_info = load_type(handle, safe_atoull(field));

                    num_parameters++;
                    field = strtok_r(NULL, ",", &context);
                }
                DELETE(copy);
            }

            type_t* result = load_type(handle, ref);

            type_t* new_function_type = NULL;
            if (kind == TKT_FUNCTION)
            {
                new_function_type = get_new_function_type(result, parameter_info, num_parameters, REF_QUALIFIER_NONE);
            }
            else if (kind == TKT_NONPROTOTYPE_FUNCTION)
            {
                new_function_type = get_nonproto_function_type(result, num_parameters);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            *pt = new_function_type;
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_VOID:
        {
            *pt = get_void_type();
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_NAMED:
        {
            sqlite3_uint64 symbol_oid = safe_atoull(symbols);

            scope_entry_t* symbol = load_symbol(handle, symbol_oid);

            *pt = get_user_defined_type(symbol);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_INDIRECT:
        {
            sqlite3_uint64 symbol_oid = safe_atoull(symbols);

            scope_entry_t* symbol = load_symbol(handle, symbol_oid);

            *pt = get_mutable_indirect_type(symbol);
            *pt = get_cv_qualified_type(*pt, cv_qualifier);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        case TKT_COMPUTED_FUNCTION:
        {
            // We keep the identifier in the kind_size field
            int id = kind_size;

            // We only allow Fortran intrinsics have computed function_types
            computed_function_type_t fun = fortran_intrinsic_get_ptr(id);

            ERROR_CONDITION((fun == NULL && id != 0), "Invalid intrinsic function id %d.\n"
                    "You may have to rebuild your Fortran modules\n", id);

            *pt = get_computed_function_type(fun);
            insert_map_ptr(handle, current_oid, *pt);
            break;
        }
        default:
        {
            internal_error("Invalid type '%d'\n", kind);
        }
    }

    return 0;
}

static type_t* load_type(sqlite3* handle, sqlite3_uint64 oid)
{
    if (oid == 0)
        return NULL;

    {
        type_t* ptr = type_t_oid(handle, oid);
        if (ptr != NULL)
        {
            return ptr;
        }
    }

    type_handle_t type_handle;
    memset(&type_handle, 0, sizeof(type_handle));
    type_handle.handle = handle;

    const char* errmsg = NULL;
    sqlite3_bind_int64(_select_type_stmt, 1, oid);
    if (run_select_query_prepared(handle, _select_type_stmt, get_type, &type_handle, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error while running query: %s\n", errmsg);
    }

    return type_handle.type;
}

static sqlite3_uint64 insert_single_const_value(sqlite3* handle, const_value_t* v)
{
    // Check if the blob is already there
    sqlite3_bind_blob(_check_raw_const_value_stmt, 1, v, const_value_get_raw_data_size(), SQLITE_STATIC);

    sqlite3_uint64 raw_oid = 0;

    int result_check = sqlite3_step(_check_raw_const_value_stmt);
    if (result_check == SQLITE_DONE)
    {
        sqlite3_bind_blob(_insert_raw_const_value_stmt, 1, v, const_value_get_raw_data_size(), SQLITE_STATIC);
        raw_oid = run_insert_statement(handle, _insert_raw_const_value_stmt);
    }
    else if (result_check == SQLITE_ROW)
    {
        raw_oid = sqlite3_column_int64(_check_raw_const_value_stmt, 0);
    }
    else
    {
        internal_error("Unexpected query result", 0);
    }
    sqlite3_reset(_check_raw_const_value_stmt);

    ERROR_CONDITION(raw_oid == 0, "Invalid OID\n", 0);

    sqlite3_bind_int64(_insert_const_value_stmt, 1, P2ULL(v));
    sqlite3_bind_int64(_insert_const_value_stmt, 2, raw_oid);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_const_value_stmt);

    return result;
}

static sqlite3_uint64 insert_multiple_const_value(sqlite3* handle, const_value_t* v, const_kind_table_t kind, type_t* struct_type)
{
    sqlite3_uint64 struct_type_id = insert_type(handle, struct_type);

    sqlite3_bind_int64(_insert_multi_const_value_stmt, 1, P2ULL(v));
    sqlite3_bind_int  (_insert_multi_const_value_stmt, 2, kind);
    sqlite3_bind_int64(_insert_multi_const_value_stmt, 3, struct_type_id);

    sqlite3_uint64 result = run_insert_statement(handle, _insert_multi_const_value_stmt);

    int i, num_elems = const_value_get_num_elements(v);
    for (i = 0; i < num_elems; i++)
    {
        sqlite3_uint64 current_part_oid = insert_const_value(handle, const_value_get_element_num(v, i));

        sqlite3_bind_int64(_insert_multi_const_value_part_stmt, 1, result);
        sqlite3_bind_int64(_insert_multi_const_value_part_stmt, 2, current_part_oid);

        run_insert_statement(handle, _insert_multi_const_value_part_stmt);
    }

    return result;
}

#define FLOAT_FORMAT_STR "%.24f"
#define DOUBLE_FORMAT_STR "%.53f"
#define LONG_DOUBLE_FORMAT_STR "%.113Le"

static sqlite3_uint64 insert_const_value(sqlite3* handle, const_value_t* value)
{
    if (value == NULL)
        return 0;

    if (oid_already_inserted_const_value(handle, value))
        return (sqlite3_uint64)(uintptr_t)value;

    if (const_value_is_integer(value)
            || const_value_is_float(value)
            || const_value_is_double(value)
            || const_value_is_long_double(value))
    {
        sqlite3_uint64 result = insert_single_const_value(handle, value);
        return result;
    }
    else if (const_value_is_complex(value))
    {
        return insert_multiple_const_value(handle, value, CKT_COMPLEX, NULL);
    }
    else if (const_value_is_structured(value))
    {
        type_t* struct_type = const_value_get_struct_type(value);
        return insert_multiple_const_value(handle, value, CKT_STRUCT, struct_type);
    }
    else if (const_value_is_array(value))
    {
        return insert_multiple_const_value(handle, value, CKT_ARRAY, NULL);
    }
    else if (const_value_is_vector(value))
    {
        return insert_multiple_const_value(handle, value, CKT_VECTOR, NULL);
    }
    else if (const_value_is_string(value))
    {
        return insert_multiple_const_value(handle, value, CKT_STRING, NULL);
    }
    else if (const_value_is_range(value))
    {
        return insert_multiple_const_value(handle, value, CKT_RANGE, NULL);
    }
    else
    {
        internal_error("Invalid const value kind", 0);
    }
    return 0;
}

static const_value_t* load_const_value(sqlite3* handle, sqlite3_uint64 oid)
{
    void *p = get_ptr_of_oid(handle, oid);
    if (p != NULL)
    {
        return (const_value_t*)p;
    }

    const_value_t* result = NULL;

    sqlite3_bind_int64(_select_const_value_stmt, 1, oid);

    int result_query = sqlite3_step(_select_const_value_stmt);
    if (result_query == SQLITE_ROW)
    {
        int column_type = sqlite3_column_type(_select_const_value_stmt, 2);
        // Single values have a raw_oid
        if (column_type == SQLITE_INTEGER)
        {
            // Single value
            sqlite_uint64 raw_oid = sqlite3_column_int64(_select_const_value_stmt, 2);
            sqlite3_reset(_select_const_value_stmt);

            sqlite3_bind_int64(_select_raw_const_value_stmt, 1, raw_oid);
            result_query = sqlite3_step(_select_raw_const_value_stmt);

            if (result_query == SQLITE_ROW)
            {
                result = const_value_build_from_raw_data(sqlite3_column_blob(_select_raw_const_value_stmt, 0));
                sqlite3_reset(_select_raw_const_value_stmt);
            }
            else
            {
                internal_error("Unexpected query result", 0);
            }
        }
        // Multi values do not have raw_oid
        else if (column_type == SQLITE_NULL)
        {
            // Multi value
            int multival_kind = sqlite3_column_int(_select_const_value_stmt, 1);
            type_t* struct_type = load_type(handle, sqlite3_column_int64(_select_const_value_stmt, 3));
            sqlite3_reset(_select_const_value_stmt);

            // Get the number of items
            sqlite3_bind_int64(_select_multi_const_value_count, 1, oid);
            result_query = sqlite3_step(_select_multi_const_value_count);
            if (result_query != SQLITE_ROW)
            {
                internal_error("Unexpected query count", 0);
            }

            int num_elems = sqlite3_column_int(_select_multi_const_value_count, 0);
            sqlite3_reset(_select_multi_const_value_count);

            ERROR_CONDITION(num_elems < 0, "Invalid number (%d) of elements for multi const value", num_elems);

            // Get the oids
            sqlite_uint64 oids[num_elems + 1];
            memset(oids, 0, sizeof(oids));

            sqlite3_bind_int64(_select_multi_const_value_parts, 1, oid);

            result_query = sqlite3_step(_select_multi_const_value_parts);
            int i = 0;
            while(result_query != SQLITE_DONE)
            {
                switch (result_query)
                {
                    case SQLITE_ROW:
                        {
                            ERROR_CONDITION(i >= num_elems, "Too many %d >= %d rows!\n", i, num_elems);
                            oids[i] = sqlite3_column_int64(_select_multi_const_value_parts, 0);
                            i++;
                            break;
                        }
                    case SQLITE_DONE:
                        {
                            break;
                        }
                    default:
                        {
                            const char* error_msg = sqlite3_errmsg(handle);
                            internal_error("Unexpected result during query '%s'\n", error_msg);
                            break;
                        }
                }
                result_query = sqlite3_step(_select_multi_const_value_parts);
            }
            sqlite3_reset(_select_multi_const_value_parts);

            // Now load every const_value_t using its oid

            const_value_t* list[num_elems + 1];
            for (i =0; i < num_elems; i++)
            {
                list[i] = load_const_value(handle, oids[i]);
            }

            // Finally build the multi const value
            switch (multival_kind)
            {
                case CKT_ARRAY:
                    {
                        result = const_value_make_array(num_elems, list);
                        break;
                    }
                case CKT_VECTOR:
                    {
                        result = const_value_make_vector(num_elems, list);
                        break;
                    }
                case CKT_STRUCT:
                    {
                        result = const_value_make_struct(num_elems, list, struct_type);
                        break;
                    }
                case CKT_COMPLEX:
                    {
                        ERROR_CONDITION(num_elems != 2, "Invalid complex constant!", 0);

                        result = const_value_make_complex(list[0], list[1]);
                        break;
                    }
                case CKT_STRING:
                    {
                        result = const_value_make_string_from_values(num_elems, list);
                        break;
                    }
                case CKT_RANGE:
                    {
                        ERROR_CONDITION(num_elems != 3, "Invalid range constant!", 0);

                        result = const_value_make_range(list[0], list[1], list[2]);
                        break;
                    }
                default:
                    {
                        internal_error("Code unreachable", 0);
                    }
            }
        }
        else
        {
            internal_error("Invalid column", 0);
        }
    }
    else
    {
        internal_error("Unexpected query result", 0);
    }

    insert_map_ptr(handle, oid, result);

    return result;
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
        fatal_error("Error while closing database (%s)\n", sqlite3_errmsg(handle));
    }
}

struct get_module_extra_data_tag
{
    sqlite3* handle;
    tl_type_t* current_item;
};

static int get_module_extra_data(void *data, 
        int num_columns UNUSED_PARAMETER, 
        char **values,
        char **columns UNUSED_PARAMETER)
{
    struct get_module_extra_data_tag* p = (struct get_module_extra_data_tag*)data;

    int kind = safe_atoi(values[0]);

    switch (kind)
    {
        case TL_UNSIGNED_INTEGER : 
            {
                *(p->current_item) = tl_unsigned_integer(safe_atoi(values[1]));
                break;
            }
        case TL_INTEGER : 
            {
                *(p->current_item) = tl_integer(safe_atoi(values[1]));
                break;
            }
        case TL_BOOL : 
            {
                *(p->current_item) = tl_bool(safe_atoi(values[1]));
                break;
            }
        case TL_STRING : 
            {
                *(p->current_item) = tl_string(uniquestr(values[1]));
                break;
            }
        case TL_SYMBOL : 
            {
                scope_entry_t* loaded_symbol = load_symbol(p->handle, safe_atoull(values[1]));
                *(p->current_item) = tl_symbol(loaded_symbol);
                break;
            }
        case TL_TYPE : 
            {
                type_t* loaded_type = load_type(p->handle, safe_atoull(values[1]));
                *(p->current_item) = tl_type(loaded_type);
                break;
            }
        case TL_NODECL:
            {
                nodecl_t node = load_nodecl(p->handle, safe_atoull(values[1]));
                *(p->current_item) = tl_nodecl(node);
                break;
            }
        case TL_DECL_CONTEXT:
            {
                const decl_context_t* decl_context = load_decl_context(p->handle, safe_atoull(values[1]));
                *(p->current_item) = tl_decl_context(decl_context);
                break;
            }
        default:
            {
                internal_error("Invalid data type %d when loading extra module information", kind);
            }
    }

    (p->current_item)++;

    return 0;
}

struct get_module_extra_name_tag
{
    sqlite3* handle;
    scope_entry_t* module;
};

static int count_module_extra_name(void *data, 
        int num_columns UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    uint64_t* value = (uint64_t*)data;
    *value = safe_atoull(values[0]);
    return 0;
}

static int get_module_extra_name(void *data, 
        int num_columns UNUSED_PARAMETER, 
        char **values, 
        char **names UNUSED_PARAMETER)
{
    struct get_module_extra_name_tag* p = (struct get_module_extra_name_tag*)data;

    char* count_query = sqlite3_mprintf(
            "SELECT COUNT(*) FROM module_extra_data WHERE oid_name = %llu;",
            safe_atoull(values[0]));

    char* errmsg = NULL;

    uint64_t num_items = 0;
    if (run_select_query(p->handle, count_query, count_module_extra_name, &num_items, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error during query: %s\n", errmsg);
    }
    sqlite3_free(count_query);

    if (num_items == 0)
        return 0;

    fortran_modules_data_t *module_data = NEW0(fortran_modules_data_t);
    module_data->name = uniquestr(values[1]);
    module_data->num_items = num_items;
    module_data->items = NEW_VEC0(tl_type_t, num_items);

    char* query = sqlite3_mprintf("SELECT kind, value FROM module_extra_data WHERE oid_name = %llu ORDER BY (order_);",
            safe_atoull(values[0]));

    struct get_module_extra_data_tag extra_data;

    extra_data.handle = p->handle;
    extra_data.current_item = module_data->items;

    if (run_select_query(p->handle, query, get_module_extra_data, &extra_data, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error during query: %s\n", errmsg);
    }

    sqlite3_free(query);

    fortran_modules_data_set_t* extra_info_attr = symbol_entity_specs_get_module_extra_info(p->module);
    if (extra_info_attr == NULL)
    {
        extra_info_attr = NEW0(fortran_modules_data_set_t);
        symbol_entity_specs_set_module_extra_info(p->module, extra_info_attr);
    }

    P_LIST_ADD(extra_info_attr->data, extra_info_attr->num_data, module_data);

    return 0;
}

static void load_extra_data_from_module(sqlite3* handle, scope_entry_t* module)
{
    struct get_module_extra_name_tag module_extra_name;

    module_extra_name.handle = handle;
    module_extra_name.module = module;

    char* errmsg = NULL;
    if (run_select_query(handle, "SELECT oid, name FROM module_extra_name", get_module_extra_name, &module_extra_name, &errmsg) != SQLITE_OK)
    {
        fatal_error("Error during query: %s\n", errmsg);
    }
}

void extend_module_info(scope_entry_t* module, const char* domain, int num_items, tl_type_t* info)
{
    ERROR_CONDITION(module->kind != SK_MODULE, "This is not a module!\n", 0);

    const char* module_name = strtolower(module->symbol_name);

    sqlite3* handle = NULL;
    const char* filename = NULL;

    driver_fortran_register_module(module_name, &filename, 
            /* is_intrinsic */ symbol_entity_specs_get_is_builtin(module));
    load_storage(&handle, filename);

    prepare_statements(handle);

    start_transaction(handle);

    // Insert domain
    char* insert_domain = sqlite3_mprintf("INSERT OR REPLACE INTO module_extra_name(name) VALUES (" Q ");",  domain);
    run_query(handle, insert_domain);
    sqlite3_uint64 domain_oid = sqlite3_last_insert_rowid(handle);
    sqlite3_free(insert_domain);

    int i;
    for (i = 0; i < num_items; i++)
    {
        int kind = info[i].kind;
        char* query = NULL;
        switch (kind)
        {
            case TL_UNSIGNED_INTEGER : 
            {
                query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %u);", 
                            domain_oid, i, kind, info[i].data._unsigned_integer);
                break;
            }
            case TL_INTEGER : 
                {
                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %d);", 
                            domain_oid, i, kind, info[i].data._integer);
                    break;
                }
            case TL_BOOL : 
                {
                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %d);", 
                            domain_oid, i, kind, (int)info[i].data._boolean);
                    break;
                }
            case TL_STRING : 
                {
                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, " Q " );", 
                            domain_oid, i, kind, info[i].data._string);
                    break;
                }
            case TL_SYMBOL : 
                {
                    sqlite3_uint64 sym_oid = insert_symbol(handle, info[i].data._entry);

                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %llu);", 
                            domain_oid, i, kind, sym_oid);
                    break;
                }
            case TL_TYPE : 
                {
                    sqlite3_uint64 type_oid = insert_type(handle, info[i].data._type);

                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %llu);", 
                            domain_oid, i, kind, type_oid);
                    break;
                }
            case TL_NODECL:
                {
                    sqlite3_uint64 nodecl_oid = insert_nodecl(handle, info[i].data._nodecl);

                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %llu);", 
                            domain_oid, i, kind, nodecl_oid);
                    break;
                }
            case TL_DECL_CONTEXT:
                {
                    sqlite3_uint64 decl_context_oid = insert_decl_context(handle, info[i].data._decl_context);

                    query = sqlite3_mprintf("INSERT INTO module_extra_data(oid_name, order_, kind, value) "
                            "VALUES (%llu, %d, %d, %llu);",
                            domain_oid, i, kind, decl_context_oid);
                    break;
                }
            default:
                {
                    internal_error("Invalid data type %d when storing extra module information", kind);
                }
        }

        run_query(handle, query);
        sqlite3_free(query);
    }

    end_transaction(handle);

    dispose_storage(handle);
}

scope_entry_t* get_module_in_cache(const char* module_name)
{
    rb_red_blk_node* query = rb_tree_query(CURRENT_COMPILED_FILE->module_file_cache, module_name);
    ERROR_CONDITION(query == NULL, "Module '%s' has not been registered", module_name);
    scope_entry_t* module_sym = (scope_entry_t*)rb_node_get_info(query);
    return module_sym;
}


#ifdef DEBUG_SQLITE3_MPRINTF
 #error Disable DEBUG_SQLITE3_MPRINTF macro once no warnings for sqlite3_mprintf calls are signaled by gcc
#endif
