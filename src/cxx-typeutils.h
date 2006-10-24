#ifndef CXX_TYPEUTILS_H
#define CXX_TYPEUTILS_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

enum cv_equivalence_t
{
    CVE_UNKNOWN = 0,
    CVE_IGNORE_OUTERMOST,
    CVE_CONSIDER
};

char equivalent_types(type_t* t1, type_t* t2, scope_t* st, 
        enum cv_equivalence_t cv_equiv, decl_context_t decl_context);
char overloaded_function(type_t* f1, type_t* f2, scope_t* st, 
        decl_context_t decl_context);

/* Copy functions */
class_info_t* copy_class_info(class_info_t* class_info);
simple_type_t* copy_simple_type(simple_type_t* type_info);
type_t* copy_type(type_t* type);
function_info_t* copy_function_info(function_info_t* function_info);
array_info_t* copy_array_info(array_info_t* array_info);
pointer_info_t* copy_pointer_info(pointer_info_t* pointer_info);
enum_info_t* copy_enum_info(enum_info_t* enum_info);
template_argument_list_t* copy_template_argument_list(template_argument_list_t* template_argument_list);

// Equality functions
char equivalent_builtin_type(simple_type_t *t1, simple_type_t *t2);

// Conversion functions
type_t* simple_type_to_type(simple_type_t* simple_type_info);
char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2, scope_t* st,
        decl_context_t decl_context);

cv_qualifier_t* get_outermost_cv_qualifier(type_t* t);

// Query functions
const char* get_builtin_type_name(simple_type_t* simple_type_info, scope_t* st);
type_t* base_type(type_t* t);

type_t* advance_over_typedefs(type_t* t);

// Debug purpose functions
void print_declarator(type_t* printed_declarator, scope_t* st);

// Query functions
char is_fundamental_type(type_t* t);
char is_integral_type(type_t* t);
char is_floating_type(type_t* t);
char is_enumerated_type(type_t* t);

char can_be_promoted_to_dest(type_t* orig, type_t* dest);
char can_be_converted_to_dest(type_t* orig, type_t* dest);

char is_reference_type(type_t* t1);
char is_reference_related(type_t* rt1, type_t* rt2, 
        scope_t* st, decl_context_t decl_context);
char is_reference_compatible(type_t* t1, type_t* t2, 
        scope_t* st, decl_context_t decl_context);

char pointer_can_be_converted_to_dest(type_t* orig, type_t* dest, scope_t* st, 
        char* to_void, char* derived_to_base, char* cv_adjust,
        decl_context_t decl_context);

char* get_type_spec_name(AST type_spec, scope_t* st);

char is_class_type(type_t* possible_class);
char is_unnamed_class_type(type_t* possible_class);
char is_named_class_type(type_t* possible_class);
char is_base_class_of(type_t* possible_base, type_t* possible_derived);
type_t* get_class_type(type_t* class_type);

char is_dependent_type(type_t* type, decl_context_t decl_context);
char is_dependent_expression(AST expr, scope_t* st, decl_context_t decl_context);

cv_qualifier_t get_cv_qualifier(type_t* type_info);

char is_bool_type(type_t* t1);
char is_pointer_type(type_t* t1);
char is_pointer_to_member_type(type_t* t);
char is_array_type(type_t* t1);

char is_void_pointer_type(type_t* t1);
char is_pointer_to_class_type(type_t* t1);
char is_reference_to_class_type(type_t* t1);

char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);

#if 0
char is_dependent_tree(AST tree, scope_t* st) __attribute__((deprecated));
#endif

scope_entry_t* give_real_entry(scope_entry_t* entry);

cv_qualifier_t* get_innermost_cv_qualifier(type_t* t);

MCXX_END_DECLS

#endif // CXX_TYPEUTILS_H
