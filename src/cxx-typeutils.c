#include <stdio.h>
#include <string.h>
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-prettyprint.h"
#include "cxx-driver.h"

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static char is_typedef_type(type_t* t);
static type_t* aliased_type(type_t* t);
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2, 
        scope_t* st, decl_context_t decl_context);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2, scope_t* st, 
        decl_context_t decl_context);
static char equivalent_function_type(type_t* t1, type_t* t2, scope_t* st, decl_context_t decl_context);
static char compatible_parameters(function_info_t* t1, function_info_t* t2, scope_t* st,
        decl_context_t decl_context);
static char compare_template_dependent_types(simple_type_t* t1, simple_type_t* t2, scope_t* st,
        decl_context_t decl_context);
static type_t* get_type_of_dependent_typename(simple_type_t* t1, decl_context_t decl_context);

type_t* advance_over_typedefs_with_cv_qualif(type_t* t1, cv_qualifier_t* cv_qualif)
{
    if (cv_qualif != NULL)
    {
        *cv_qualif = t1->cv_qualifier;
    }
    // Advance over typedefs
    while (is_typedef_type(t1))
    {
        t1 = aliased_type(t1);
        if (cv_qualif != NULL)
        {
            *cv_qualif |= t1->cv_qualifier;
        }
    }

    return t1;
}

type_t* advance_over_typedefs(type_t* t1)
{
    return advance_over_typedefs_with_cv_qualif(t1, NULL);
}

/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
char equivalent_types(type_t* t1, type_t* t2, scope_t* st, 
        enum cv_equivalence_t cv_equiv, decl_context_t decl_context)
{
    if (t1 == NULL || t2 == NULL)
        return 1;

    cv_qualifier_t cv_qualifier_t1, cv_qualifier_t2;

    cv_qualifier_t qualif_t1 = *(get_outermost_cv_qualifier(t1));
    cv_qualifier_t qualif_t2 = *(get_outermost_cv_qualifier(t2));
    if (cv_equiv == CVE_IGNORE_OUTERMOST)
    {
        // Remove the outermost cv_qualifier
        *(get_outermost_cv_qualifier(t1)) = CV_NONE;
        *(get_outermost_cv_qualifier(t2)) = CV_NONE;
    }
    
    // Advance over typedefs
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualifier_t1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualifier_t2);

    if (t1->kind != t2->kind)
    {
        // They cannot be the same
        simple_type_t* dependent_simple_type = NULL;
        type_t* other_type = NULL;
        cv_qualifier_t qualif_depend = CV_NONE;
        if (t1->kind == TK_DIRECT && t1->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            dependent_simple_type = t1->type;
            other_type = t2;

            qualif_depend = *(get_outermost_cv_qualifier(t1));
        }
        else if (t2->kind == TK_DIRECT && t2->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            dependent_simple_type = t2->type;
            other_type = t1;

            qualif_depend = *(get_outermost_cv_qualifier(t2));
        }

        if (dependent_simple_type != NULL)
        {
            type_t* dependent_type = get_type_of_dependent_typename(dependent_simple_type, decl_context);

            if (dependent_type != NULL)
            {
                dependent_type = advance_over_typedefs(dependent_type);
                cv_qualifier_t saved_cv_qualif = *(get_outermost_cv_qualifier(dependent_type));

                *(get_outermost_cv_qualifier(dependent_type)) = qualif_depend;
                char result = equivalent_types(other_type, dependent_type, st, cv_equiv, decl_context);
                *(get_outermost_cv_qualifier(dependent_type)) = saved_cv_qualif;

                return result;
            }
        }
        return 0;
    }

    char result = 0;

    switch (t1->kind)
    {
        case TK_DIRECT :
            result = equivalent_simple_types(t1->type, t2->type, st, decl_context);
            break;
        case TK_POINTER :
            result = equivalent_pointer_type(t1->pointer, t2->pointer, st, decl_context);
            break;
        case TK_REFERENCE :
            result = equivalent_pointer_type(t1->pointer, t2->pointer, st, decl_context);
            break;
        case TK_POINTER_TO_MEMBER :
            break;
        case TK_ARRAY :
            result = equivalent_array_type(t1->array, t2->array, st, decl_context);
            break;
        case TK_FUNCTION :
            result = equivalent_function_type(t1, t2, st, decl_context);
            break;
        default :
            internal_error("Unknown type kind (%d)\n", t1->kind);
    }

    result &= equivalent_cv_qualification(cv_qualifier_t1, cv_qualifier_t2);

    if (cv_equiv == CVE_IGNORE_OUTERMOST)
    {
        *(get_outermost_cv_qualifier(t1)) = qualif_t1;
        *(get_outermost_cv_qualifier(t2)) = qualif_t2;
    }

    return result;
}

static type_t* get_type_of_dependent_typename(simple_type_t* t1, decl_context_t decl_context)
{
    if (t1->kind != STK_TEMPLATE_DEPENDENT_TYPE)
    {
        internal_error("This is not a dependent typename\n", 0);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Getting the type of the dependent typename '%s'\n", 
                prettyprint_in_buffer(t1->typeof_expr));
    }

    scope_t* t1_scope = t1->typeof_scope;

    AST t1_expr = t1->typeof_expr;
    AST t1_global_op = ASTSon0(t1_expr);
    AST t1_nested_name_spec = ASTSon1(t1_expr);
    AST t1_symbol = ASTSon2(t1_expr);

    decl_context.decl_flags |= DF_NO_FAIL;

    scope_entry_list_t* result_t1 = query_nested_name(t1_scope, t1_global_op, t1_nested_name_spec, t1_symbol,
                FULL_UNQUALIFIED_LOOKUP, decl_context);

    if (result_t1 == NULL)
        return NULL;

    if (result_t1->entry->type_information == NULL)
        return NULL;

    return result_t1->entry->type_information;
}

char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2, scope_t* st,
        decl_context_t decl_context)
{
    char result = 0;
    if (t1->kind != t2->kind)
    {
        // typedefs have been handled in an earlier place, so 
        // this cannot be the same type unless one is a template dependent type

        simple_type_t* simple_dependent_type = NULL;
        simple_type_t* other_type = NULL;
        if (t1->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            simple_dependent_type = t1;
            other_type = t2;
        }
        else if (t2->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            simple_dependent_type = t2;
            other_type = t1;
        }

        if (simple_dependent_type != NULL)
        {
            type_t* dep_type = get_type_of_dependent_typename(simple_dependent_type, decl_context);
            
            if (dep_type == NULL)
            {
                return 0;
            }
            else
            {
                // Try to solve this type
                char result;
                result = equivalent_types(dep_type, simple_type_to_type(other_type), st, CVE_CONSIDER, decl_context);
                return result;
            }
        }

        return 0;
    }

    switch (t1->kind)
    {
        case STK_BUILTIN_TYPE :
            result = equivalent_builtin_type(t1, t2);
            break;
        case STK_CLASS :
            /* Fall-through */
        case STK_ENUM :
            // Pointer comparison MUST work
            // (if not, something is broken)
            result = (t1 == t2);
            break;
        case STK_USER_DEFINED :
            result = equivalent_types(t1->user_defined_type->type_information, 
                    t2->user_defined_type->type_information, st, CVE_CONSIDER,
                    decl_context);
            break;
        case STK_TYPE_TEMPLATE_PARAMETER :
        case STK_TEMPLATE_TEMPLATE_PARAMETER : // Fix this case
            result = ((t1 == t2) || 
                    ((t1->template_parameter_num == t2->template_parameter_num)
                     && (t1->template_parameter_nesting == t2->template_parameter_nesting)));
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            result = compare_template_dependent_types(t1, t2, st, decl_context);
            break;
        case STK_TYPEDEF :
            internal_error("A typedef cannot reach here", 0);
            break;
        case STK_TYPEOF :
            internal_error("__typeof__ comparison still not implemented", 0);
            break;
        case STK_VA_LIST :
            {
                // If both are __builtin_va_list, this is trivially true
                return 1;
            }
        default :
            internal_error("Unknown simple type kind (%d)", t1->kind);
            return 0;
    }

    return result;
}

char equivalent_builtin_type(simple_type_t* t1, simple_type_t *t2)
{
    if (t1->builtin_type != t2->builtin_type)
    {
        return 0;
    }

    // Ok, up to here "unsigned int" and "signed int" are the same
    // The same happens with "long int" and "int"
    //
    // long
    if (t1->builtin_type == BT_INT
            || t1->builtin_type == BT_DOUBLE)
    {
        if (t1->is_long != t2->is_long)
            return 0;
    }

    // short
    if (t1->builtin_type == BT_INT)
    {
        if (t1->is_short != t2->is_short)
            return 0;
    }

    // unsigned
    if (t1->builtin_type == BT_INT
            || t1->builtin_type == BT_CHAR)
    {
        if (t1->is_unsigned != t2->is_unsigned)
            return 0;
    }
    
    // signed
    if (t1->builtin_type == BT_INT
            || t1->builtin_type == BT_CHAR)
    {
        if (t1->is_signed != t2->is_signed)
            return 0;
    }

    // GCC extension for complex 
    if (t1->is_complex != t2->is_complex)
    {
        return 0;
    }
    
    // Ok, nothing makes us think they might be different
    return 1;
}

static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2, scope_t* st,
        decl_context_t decl_context)
{
    if (!equivalent_types(t1->pointee, t2->pointee, st, 
                CVE_CONSIDER, decl_context))
    {
        return 0;
    }

    return 1;
}

static char equivalent_array_type(array_info_t* t1, array_info_t* t2, scope_t* st, 
        decl_context_t decl_context)
{
    if (!equivalent_types(t1->element_type, t2->element_type, st, 
                CVE_CONSIDER, decl_context))
        return 0;

    if (t1->array_expr != NULL
            && t2->array_expr != NULL)
    {
        literal_value_t v1 = evaluate_constant_expression(t1->array_expr, st, decl_context);
        literal_value_t v2 = evaluate_constant_expression(t2->array_expr, st, decl_context);
        if (!equal_literal_values(v1, v2, st))
            return 0;
    }
    else
    {
        // int a[] does not match with int a[10]; (it will match via
        // array-to-pointer, but this is not the case we are handling now)
        if ((t1->array_expr == NULL
                && t2->array_expr != NULL)
                || (t1->array_expr != NULL
                    && t2->array_expr == NULL))
        {
            return 0;
        }
    }
    
    return 1;
}

cv_qualifier_t* get_outermost_cv_qualifier(type_t* t)
{
    // For types that do not have a cv qualifier on their own
    static cv_qualifier_t dummy_cv_qualif = CV_NONE;

    // This will avoid accidental modifications from outside
    dummy_cv_qualif = CV_NONE;

    switch (t->kind)
    {
        case TK_FUNCTION :
        case TK_DIRECT :
        case TK_POINTER :
        case TK_POINTER_TO_MEMBER :
            {
                return (&(t->cv_qualifier));
            }
        case TK_ARRAY :
        case TK_REFERENCE :
            {
                return (&dummy_cv_qualif);
            }
        default:
            {
                internal_error("Unexpected node type %d\n", t->kind);
            }
    }
}

cv_qualifier_t* get_innermost_cv_qualifier(type_t* t)
{
    // For types that do not have a cv qualifier on their own
    static cv_qualifier_t dummy_cv_qualif = CV_NONE;

    // This will avoid accidental modifications from outside
    dummy_cv_qualif = CV_NONE;

    switch (t->kind)
    {
        case TK_DIRECT :
            {
                return &(t->cv_qualifier);
                break;
            }
        case TK_ARRAY :
            {
                return get_innermost_cv_qualifier(t->array->element_type);
            }
        case TK_POINTER :
        case TK_POINTER_TO_MEMBER :
        case TK_REFERENCE :
            {
                return get_innermost_cv_qualifier(t->pointer->pointee);
            }
        case TK_FUNCTION :
            {
                return get_innermost_cv_qualifier(t->function->return_type);
            }
        default:
            {
                internal_error("Unexpected node type %d\n", t->kind);
            }
    }
}

char overloaded_function(type_t* ft1, type_t* ft2, scope_t* st, 
        decl_context_t decl_context)
{
    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (t1->template_nesting != t2->template_nesting)
        return 1;

    if (t1->num_template_parameters_in_scope != t2->num_template_parameters_in_scope)
        return 1;

    int i = 0;
    for (i = 0; i < t1->num_template_parameters_in_scope; i++)
    {
        template_parameter_t* t1_param = t1->template_parameter_in_scope_info[i];
        template_parameter_t* t2_param = t2->template_parameter_in_scope_info[i];

        if (t1_param->kind != t2_param->kind)
        {
            return 1;
        }
    }

    if (!compatible_parameters(t1, t2, st, decl_context))
        return 1;

    // If one has return type but the other does not this is an overload
    // (technically this is ill-formed)
    if (((t1->return_type == NULL)
                && (t2->return_type != NULL))
            || ((t2->return_type == NULL)
                && (t1->return_type != NULL)))
        return 1;

    if (!equivalent_cv_qualification(ft1->cv_qualifier, 
                ft2->cv_qualifier))
        return 1;
            

    // Destructors, constructors, operator functions and conversion functions
    // will not have a full direct type
    if (t1->return_type == NULL 
            && t2->return_type == NULL)
        return 0;

    if (!equivalent_types(t1->return_type, t2->return_type, st, CVE_CONSIDER, decl_context))
    {
        if (!is_dependent_type(t1->return_type, decl_context)
                && !is_dependent_type(t2->return_type, decl_context))
        {
            internal_error("You are trying to overload a function by only modifying its return type", 0);
        }
        else
        {
            return 1;
        }
    }

    return 0;
}

static char equivalent_function_type(type_t* ft1, type_t* ft2, 
        scope_t* st, decl_context_t decl_context)
{
    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (!equivalent_types(t1->return_type, t2->return_type, st, CVE_CONSIDER,
                decl_context))
        return 0;

    if (!compatible_parameters(t1, t2, st, decl_context))
        return 0;

    if (!equivalent_cv_qualification(ft1->cv_qualifier, ft2->cv_qualifier))
        return 0;

    return 1;
}

char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    // Oh, this turned to be that easy
    return (cv1 == cv2);
}

static char compatible_parameters(function_info_t* t1, function_info_t* t2, scope_t* st, 
        decl_context_t decl_context)
{
    if (t1->num_parameters != t2->num_parameters)
        return 0;

    char still_compatible = 1;
    int i;

    for (i = 0; (i < t1->num_parameters) && still_compatible; i++)
    {
        if (t1->parameter_list[i]->is_ellipsis
                || t2->parameter_list[i]->is_ellipsis)
        {
            still_compatible = (t1->parameter_list[i]->is_ellipsis && t2->parameter_list[i]->is_ellipsis);
            continue;
        }

        type_t* par1 = t1->parameter_list[i]->type_info;
        type_t* par2 = t2->parameter_list[i]->type_info;

        if (!equivalent_types(par1, par2, st, CVE_IGNORE_OUTERMOST, decl_context))
        {
            // They are not equivalent types.
            //
            // Try to apply criteria of compatibility as defined in clause 13
            // of C++ standard

            /*
             * Compatibility between pointers and first dimension of an array
             *
             * i.e.  
             *       'int (*k)[10]' is compatible with     'int k[5][10]'
             *       'int (*k)[10]' is NOT compatible with 'int k[5][15]'
             */
            if ((par1->kind == TK_ARRAY && 
                        par2->kind == TK_POINTER)
                    || (par1->kind == TK_POINTER && 
                        par2->kind == TK_ARRAY))
            {
                type_t* array_type = (par1->kind == TK_ARRAY) ? par1 : par2;
                type_t* pointer_type = (par1->kind == TK_POINTER) ? par1 : par2;

                if (!equivalent_types(array_type->array->element_type, pointer_type->pointer->pointee, 
                            st, CVE_CONSIDER, decl_context))
                {
                    still_compatible = 0;
                }
            }
            /*
             * Compatibility between pointer to function and function parameter
             *
             * i.e.
             *    'void f(int k(bool))' is compatible with 'void g(int (*t)(bool)'
             */
            else if ((par1->kind == TK_FUNCTION &&
                        par2->kind == TK_POINTER)
                    || (par1->kind == TK_POINTER &&
                        par2->kind == TK_FUNCTION))
            {
                type_t* pointer_type = (par1->kind == TK_POINTER) ? par1 : par2;
                type_t* function_type = (par1->kind == TK_FUNCTION) ? par1 : par2;

                // Let's avoid unnecessary work
                if (pointer_type->pointer->pointee->kind != TK_FUNCTION)
                {
                    still_compatible = 0;
                }
                else
                {
                    if (!equivalent_types(pointer_type->pointer->pointee, function_type, st, CVE_CONSIDER,
                                decl_context))
                    {
                        still_compatible = 0;
                    }
                }
            }
            else // No other applies
            {
                still_compatible = 0;
            }
        }
    }

    return still_compatible;
}

static char compare_template_dependent_types(simple_type_t* t1, simple_type_t* t2, scope_t* st,
        decl_context_t decl_context)
{
    AST t1_expr = t1->typeof_expr;
    AST t2_expr = t2->typeof_expr;
    DEBUG_CODE()
    {
        fprintf(stderr, "We are going to compare dependent type '%s' with '%s'\n",
                prettyprint_in_buffer(t1_expr),
                prettyprint_in_buffer(t2_expr));
    }

    // Shortcut
    if (t1 == t2 
            || t1_expr == t2_expr)
    {
        DEBUG_CODE()
        {
            if (t1 == t2)
            {
                fprintf(stderr, "Dependent types are the same, trivially are the same\n");
            }
            else
            {
                fprintf(stderr, "Expressions are the same, trivially the types are the same\n");
            }
        }
        return 1;
    }

    scope_t* t1_scope = t1->typeof_scope;
    scope_t* t2_scope = t2->typeof_scope;

    AST t1_global_op = ASTSon0(t1_expr);
    AST t2_global_op = ASTSon0(t2_expr);

    AST t1_nested_name_spec = ASTSon1(t1_expr);
    AST t2_nested_name_spec = ASTSon1(t2_expr);

    AST t1_symbol = ASTSon2(t1_expr);
    AST t2_symbol = ASTSon2(t2_expr);

    // First try to solve them via the type system
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Checking if '%s' solves to the same as '%s'\n", 
                    prettyprint_in_buffer(t1_expr), 
                    prettyprint_in_buffer(t2_expr));
        }
        decl_context_t new_decl_context = decl_context;
        new_decl_context.decl_flags |= DF_NO_FAIL;

        scope_entry_list_t* result_t1 = query_nested_name_flags(t1_scope, t1_global_op, t1_nested_name_spec, t1_symbol,
                FULL_UNQUALIFIED_LOOKUP, LF_NONE, decl_context);

        if (result_t1 != NULL)
        {
            scope_entry_t* entry_t1 = result_t1->entry;
            if (entry_t1->type_information != NULL)
            {
                if (equivalent_types(entry_t1->type_information, simple_type_to_type(t2), 
                            entry_t1->scope, CVE_CONSIDER, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Type '%s' solves to the same as '%s'\n", 
                                prettyprint_in_buffer(t1_expr), 
                                prettyprint_in_buffer(t2_expr));
                    }
                    return 1;
                }
            }
        }
    }

    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Checking if '%s' solves to the same as '%s'\n", 
                    prettyprint_in_buffer(t2_expr),
                    prettyprint_in_buffer(t1_expr));
        }

        decl_context_t new_decl_context = decl_context;
        new_decl_context.decl_flags |= DF_NO_FAIL;

        scope_entry_list_t* result_t2 = query_nested_name_flags(t2_scope, t2_global_op, t2_nested_name_spec, t2_symbol,
                FULL_UNQUALIFIED_LOOKUP, LF_NONE, new_decl_context);
        if (result_t2 != NULL)
        {
            scope_entry_t* entry_t2 = result_t2->entry;
            if (entry_t2->type_information != NULL)
            {
                if (equivalent_types(entry_t2->type_information, simple_type_to_type(t1), 
                            entry_t2->scope, CVE_CONSIDER, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Type '%s' solves to the same as '%s'\n", 
                                prettyprint_in_buffer(t2_expr), 
                                prettyprint_in_buffer(t1_expr));
                    }
                    return 1;
                }
            }
        }
    }

    // Fallback to syntactical comparison
    DEBUG_CODE()
    {
        fprintf(stderr, "We fall back to syntactical comparison step-by-step\n");
    }

    // One has :: and the other not
    if ((t1_global_op == NULL && t2_global_op != NULL)
            || (t1_global_op != NULL && t2_global_op == NULL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "One type has global qualification and the other does not, thus they are different\n");
        }
        return 0;
    }

    if (t1_global_op != NULL) // t2_global_op != NULL too
    {
        st = compilation_options.global_scope;
    }


    int qualification_level = 0;

    char dependent_qualification = 0;

    while (t1_nested_name_spec != NULL
            && t2_nested_name_spec != NULL)
    {
        AST t1_class_or_namespace = ASTSon0(t1_nested_name_spec);
        AST t2_class_or_namespace = ASTSon0(t2_nested_name_spec);

        DEBUG_CODE()
        {
            fprintf(stderr, "Considering qualification part '%s' vs '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
        }

        if (ASTType(t1_class_or_namespace) != ASTType(t2_class_or_namespace))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Qualification is '%s' that is syntactically different to '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }
            return 0;
        }

        if (dependent_qualification)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Since this is dependent, we simply compare syntactic trees '%s' and '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }

            if (!ast_equal(t1_class_or_namespace, t2_class_or_namespace))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are not equal\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
                return 0;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are identic\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
            }
        }
        else if (ASTType(t1_class_or_namespace) == AST_SYMBOL)
        {
            // enum cxx_symbol_kind filter_class_or_namespace[3] = {SK_NAMESPACE, SK_CLASS, SK_TEMPLATE_TYPE_PARAMETER};

            scope_entry_list_t* t1_name_list = query_unqualified_name(t1_scope, ASTText(t1_class_or_namespace));
            scope_entry_list_t* t2_name_list = query_unqualified_name(t2_scope, ASTText(t2_class_or_namespace));

            if (t1_name_list == NULL || t2_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p scope_t1=%p t2=%p scope_t2=%p\n",
                        t1_name_list, t1_scope, t2_name_list, t2_scope);
            }

            scope_entry_t* t1_name = t1_name_list->entry;
            scope_entry_t* t2_name = t2_name_list->entry;

            if (t1_name->kind != t2_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Kind of '%s' is different to '%s'\n", 
                            t1_name->symbol_name,
                            t2_name->symbol_name);
                }
                return 0;
            }


            if (t1_name->kind == SK_NAMESPACE
                    && t2_name->kind == SK_NAMESPACE)
            {
                if (t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Namespace '%s' is not the same namespace '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }
            }
            else
            {
                t1_name = give_real_entry(t1_name);
                t2_name = give_real_entry(t2_name);

                type_t* t1_type = t1_name->type_information;
                type_t* t2_type = t2_name->type_information;

                t1_type = advance_over_typedefs(t1_type);
                t2_type = advance_over_typedefs(t2_type);

                if (t1_type->kind == TK_DIRECT
                        && t2_type->kind == TK_DIRECT)
                {
                    if ((t1_type->type->kind == STK_TYPE_TEMPLATE_PARAMETER
                                && t2_type->type->kind == STK_TYPE_TEMPLATE_PARAMETER)
                            || (t1_type->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER
                                && t2_type->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER))
                    {
                        if (!equivalent_types(t1_type, t2_type, st, CVE_CONSIDER, decl_context))
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Type '");
                                print_declarator(t1_type, st);
                                fprintf(stderr, "' is not the same as '");
                                print_declarator(t2_type, st);
                                fprintf(stderr, "'\n");
                            }
                            return 0;
                        }
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template parameter\n");
                        }
                        dependent_qualification = 1;
                    }
                    else if (t1_type->type->kind == STK_TEMPLATE_DEPENDENT_TYPE
                            && t2_type->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template dependent type\n");
                        }
                        dependent_qualification = 1;
                    }
                }
                if (!dependent_qualification 
                        && t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Symbol '%s' is not the same as '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }

                if (!dependent_qualification 
                        && is_dependent_type(t1_type, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template dependent type\n");
                    }
                    dependent_qualification = 1;
                }
            }
            t1_scope = t1_name->related_scope;
            t2_scope = t2_name->related_scope;
        }
        else if (ASTType(t1_class_or_namespace) == AST_TEMPLATE_ID)
        {
            scope_entry_list_t* t1_template_name_list;
            scope_entry_list_t* t2_template_name_list;

            scope_t* extended_scope1 = copy_scope(st);
            extended_scope1->template_scope = t1_scope->template_scope;
            scope_t* extended_scope2 = copy_scope(st);
            extended_scope2->template_scope = t2_scope->template_scope;

            if (qualification_level > 0)
            {
                t1_template_name_list = query_template_id(t1_class_or_namespace, 
                        extended_scope1, t1_scope, decl_context);
                t2_template_name_list = query_template_id(t2_class_or_namespace, 
                        extended_scope2, t2_scope, decl_context);
            }
            else
            {
                t1_template_name_list = query_unqualified_template_id(t1_class_or_namespace, 
                        extended_scope1, t1_scope, decl_context);
                t2_template_name_list = query_unqualified_template_id(t2_class_or_namespace, 
                        extended_scope2, t2_scope, decl_context);
            }

            if (t1_template_name_list == NULL || t2_template_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p t2=%p\n",
                        t1_template_name_list, t2_template_name_list);
            }

            scope_entry_t* t1_template_name = t1_template_name_list->entry;
            scope_entry_t* t2_template_name = t2_template_name_list->entry;

            if (t1_template_name->kind != t2_template_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Template symbol '%s' is not the same as '%s'\n",
                            t1_template_name->symbol_name,
                            t2_template_name->symbol_name);
                }
                return 0;
            }

            if (t1_template_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                if ((t1_template_name->type_information->type->template_parameter_num != 
                            t2_template_name->type_information->type->template_parameter_num)
                        || (t1_template_name->type_information->type->template_parameter_nesting != 
                            t2_template_name->type_information->type->template_parameter_nesting))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Template template parameter '%s' (%d-%d) is not the same as the\
                                template template parameter '%s' (%d-%d)\n",
                                t1_template_name->symbol_name,
                                t1_template_name->type_information->type->template_parameter_nesting,
                                t1_template_name->type_information->type->template_parameter_num,
                                t2_template_name->symbol_name,
                                t2_template_name->type_information->type->template_parameter_nesting,
                                t2_template_name->type_information->type->template_parameter_num);
                    }
                    return 0;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "From now this typename is dependent due to a template template parameter\n");
                }
                dependent_qualification = 1;
            }
            else
            {
                // TODO - Check this because i'm unsure this is totally true
                if (t1_template_name != t2_template_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "The template symbols '%s' and '%s' do not refer to the same entity\n",
                                t1_template_name->symbol_name,
                                t2_template_name->symbol_name);
                    }
                    return 0;
                }
                else if (is_dependent_type(t1_template_name->type_information, decl_context)
                        && is_dependent_type(t2_template_name->type_information, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now this typename is dependent due to a dependent template-id\n");
                    }
                    dependent_qualification = 1;
                }
            }
        }

        t1_nested_name_spec = ASTSon1(t1_nested_name_spec);
        t2_nested_name_spec = ASTSon1(t2_nested_name_spec);
        qualification_level++;
    }

    if ((t1_nested_name_spec == NULL && t2_nested_name_spec != NULL) 
            || (t1_nested_name_spec != NULL && t2_nested_name_spec == NULL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Types differ in qualification level\n");
        }
        return 0;
    }

    // Check the final part of the qualified-id
    {
        AST t1_class_or_namespace = ASTSon2(t1_expr);
        AST t2_class_or_namespace = ASTSon2(t2_expr);

        DEBUG_CODE()
        {
            fprintf(stderr, "Considering qualification part '%s' vs '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
        }

        if (ASTType(t1_class_or_namespace) != ASTType(t2_class_or_namespace))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Qualification is '%s' that is syntactically different to '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }
            return 0;
        }

        if (dependent_qualification)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Since this is dependent, we simply compare syntactic trees '%s' and '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }

            if (!ast_equal(t1_class_or_namespace, t2_class_or_namespace))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are not equal\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
                return 0;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are identic\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
            }
        }
        else if (ASTType(t1_class_or_namespace) == AST_SYMBOL)
        {
            // enum cxx_symbol_kind filter_class_or_namespace[3] = {SK_NAMESPACE, SK_CLASS, SK_TEMPLATE_TYPE_PARAMETER};

            scope_entry_list_t* t1_name_list = query_unqualified_name(t1_scope, ASTText(t1_class_or_namespace));
            scope_entry_list_t* t2_name_list = query_unqualified_name(t2_scope, ASTText(t2_class_or_namespace));

            if (t1_name_list == NULL || t2_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p scope_t1=%p t2=%p scope_t2=%p, t1='%s' in %s, t2='%s' in %s\n",
                        t1_name_list, t1_scope, t2_name_list, t2_scope,
                        prettyprint_in_buffer(t1_class_or_namespace), 
                        node_information(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace),
                        node_information(t2_class_or_namespace));
            }

            scope_entry_t* t1_name = t1_name_list->entry;
            scope_entry_t* t2_name = t2_name_list->entry;

            if (t1_name->kind != t2_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Kind of '%s' is different to '%s'\n", 
                            t1_name->symbol_name,
                            t2_name->symbol_name);
                }
                return 0;
            }


            if (t1_name->kind == SK_NAMESPACE
                    && t2_name->kind == SK_NAMESPACE)
            {
                if (t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Namespace '%s' is not the same namespace '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }
            }
            else
            {
                t1_name = give_real_entry(t1_name);
                t2_name = give_real_entry(t2_name);

                type_t* t1_type = t1_name->type_information;
                type_t* t2_type = t2_name->type_information;

                t1_type = advance_over_typedefs(t1_type);
                t2_type = advance_over_typedefs(t2_type);

                if (t1_type->kind == TK_DIRECT
                        && t2_type->kind == TK_DIRECT)
                {
                    if ((t1_type->type->kind == STK_TYPE_TEMPLATE_PARAMETER
                                && t2_type->type->kind == STK_TYPE_TEMPLATE_PARAMETER)
                            || (t1_type->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER
                                && t2_type->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER))
                    {
                        if (!equivalent_types(t1_type, t2_type, st, CVE_CONSIDER, decl_context))
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Type '");
                                print_declarator(t1_type, st);
                                fprintf(stderr, "' is not the same as '");
                                print_declarator(t2_type, st);
                                fprintf(stderr, "'\n");
                            }
                            return 0;
                        }
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template parameter\n");
                        }
                        dependent_qualification = 1;
                    }
                    else if (t1_type->type->kind == STK_TEMPLATE_DEPENDENT_TYPE
                            && t2_type->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template dependent type\n");
                        }
                        dependent_qualification = 1;
                    }
                }

                if (t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Symbol '%s' is not the same as '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }
            }
            t1_scope = t1_name->related_scope;
            t2_scope = t2_name->related_scope;
        }
        else if (ASTType(t1_class_or_namespace) == AST_TEMPLATE_ID)
        {
            scope_entry_list_t* t1_template_name_list;
            scope_entry_list_t* t2_template_name_list;

            scope_t* extended_scope1 = copy_scope(st);
            extended_scope1->template_scope = t1_scope->template_scope;
            scope_t* extended_scope2 = copy_scope(st);
            extended_scope2->template_scope = t2_scope->template_scope;

            if (qualification_level > 0)
            {
                t1_template_name_list = query_template_id(t1_class_or_namespace, 
                        extended_scope1, t1_scope, decl_context);
                t2_template_name_list = query_template_id(t2_class_or_namespace, 
                        extended_scope2, t2_scope, decl_context);
            }
            else
            {
                t1_template_name_list = query_unqualified_template_id(t1_class_or_namespace, 
                        extended_scope1, t1_scope, decl_context);
                t2_template_name_list = query_unqualified_template_id(t2_class_or_namespace, 
                        extended_scope2, t2_scope, decl_context);
            }

            if (t1_template_name_list == NULL || t2_template_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p t2=%p\n",
                        t1_template_name_list, t2_template_name_list);
            }

            scope_entry_t* t1_template_name = t1_template_name_list->entry;
            scope_entry_t* t2_template_name = t2_template_name_list->entry;

            if (t1_template_name->kind != t2_template_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Template symbol '%s' is not the same as '%s'\n",
                            t1_template_name->symbol_name,
                            t2_template_name->symbol_name);
                }
                return 0;
            }

            if (t1_template_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                if ((t1_template_name->type_information->type->template_parameter_num != 
                            t2_template_name->type_information->type->template_parameter_num)
                        || (t1_template_name->type_information->type->template_parameter_nesting != 
                            t2_template_name->type_information->type->template_parameter_nesting))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Template template parameter '%s' (%d-%d) is not the same as the\
                                template template parameter '%s' (%d-%d)\n",
                                t1_template_name->symbol_name,
                                t1_template_name->type_information->type->template_parameter_nesting,
                                t1_template_name->type_information->type->template_parameter_num,
                                t2_template_name->symbol_name,
                                t2_template_name->type_information->type->template_parameter_nesting,
                                t2_template_name->type_information->type->template_parameter_num);
                    }
                    return 0;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "From now this typename is dependent due to a template template parameter\n");
                }
                dependent_qualification = 1;
            }
            else
            {
                // TODO - Check this because i'm unsure this is totally true
                if (t1_template_name != t2_template_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "The template symbols '%s' and '%s' do not refer to the same entity\n",
                                t1_template_name->symbol_name,
                                t2_template_name->symbol_name);
                    }
                    return 0;
                }
                else if (is_dependent_type(t1_template_name->type_information, decl_context)
                        && is_dependent_type(t2_template_name->type_information, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now this typename is dependent due to a dependent template-id\n");
                    }
                    dependent_qualification = 1;
                }
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Dependent types are the same\n");
    }
    return 1;
}

static char is_typedef_type(type_t* t1)
{
    if ((t1->kind == TK_DIRECT 
            && t1->type->kind == STK_TYPEDEF))
    {
        return 1;
    }

    if (t1->kind == TK_DIRECT
            && t1->type->kind == STK_USER_DEFINED)
    {
        scope_entry_t* user_defined_entry = t1->type->user_defined_type;
        type_t* user_defined_type = user_defined_entry->type_information;

        if (user_defined_type != NULL 
                && user_defined_type->kind == TK_DIRECT 
                && user_defined_type->type != NULL 
                && user_defined_type->type->kind == STK_TYPEDEF)
        {
            return 1;
        }
    }

    return 0;
}

static type_t* aliased_type(type_t* t1)
{
    if (!is_typedef_type(t1))
        internal_error("This is not a 'typedef' type", 0);

    if (t1->kind == TK_DIRECT && t1->type->kind == STK_TYPEDEF)
    {
        return (t1->type->aliased_type);
    }
    else
    {
        scope_entry_t* user_defined_entry = t1->type->user_defined_type;
        type_t* user_defined_type = user_defined_entry->type_information;

        return user_defined_type->type->aliased_type;
    }
}

type_t* base_type(type_t* t1)
{
    while (t1->kind != TK_DIRECT)
    {
        switch (t1->kind)
        {
            case TK_POINTER :
            case TK_REFERENCE :
            case TK_POINTER_TO_MEMBER :
                t1 = t1->pointer->pointee;
                break;
            case TK_FUNCTION :
                t1 = t1->function->return_type;
                break;
            case TK_ARRAY :
                t1 = t1->array->element_type;
                break;
            default:
                internal_error("Unknown type kind %d", t1->kind);
        }
    }

    return t1;
}

char is_fundamental_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE);
}

char is_direct_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_DIRECT);
}

char is_integral_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT);
}

char is_pointer_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_POINTER);
}

char is_function_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t->kind == TK_FUNCTION);
}

type_t* function_return_type(type_t* t)
{
    t = advance_over_typedefs(t);

    if (t->kind == TK_FUNCTION)
    {
        return t->function->return_type;
    }
    else 
        return NULL;
}

type_t** function_parameter_types(type_t* t, int* num_params, char* has_ellipsis)
{
    *has_ellipsis = 0;
    *num_params = 0;
    type_t** result = calloc(1, sizeof(*result));

    int i;
    for (i = 0; i < t->function->num_parameters; i++)
    {
        parameter_info_t* param_info = t->function->parameter_list[i];
        if (!param_info->is_ellipsis && param_info->type_info != NULL)
        {
            P_LIST_ADD(result, (*num_params), param_info->type_info);
        }
        else if (param_info->is_ellipsis)
        {
            *has_ellipsis = 1;
        }
    }

    return result;
}

type_t* pointer_pointee_type(type_t* t)
{
    t = advance_over_typedefs(t);

    if (t->kind == TK_POINTER)
    {
        return t->pointer->pointee;
    }
    else 
        return NULL;
}

type_t* array_element_type(type_t* t)
{
    t = advance_over_typedefs(t);

    if (t->kind == TK_ARRAY)
    {
        return t->array->element_type;
    }
    else 
        return NULL;
}

char is_array_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_ARRAY);
}

char is_pointer_to_class_type(type_t* t1)
{
    return (is_pointer_type(t1) && is_class_type(t1->pointer->pointee));
}

char is_reference_to_class_type(type_t* t1)
{
    return (is_reference_type(t1) && is_class_type(t1->pointer->pointee));
}


char is_void_pointer_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_POINTER
            && t->pointer->pointee->kind == TK_DIRECT
            && t->pointer->pointee->type->kind == STK_BUILTIN_TYPE
            && t->pointer->pointee->type->builtin_type == BT_VOID);
}

char is_pointer_to_member_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_POINTER_TO_MEMBER);
}

char is_enumerated_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_DIRECT
            && ((t->type->kind == STK_USER_DEFINED
                    && t->type->user_defined_type != NULL
                    && t->type->user_defined_type->type_information->kind == TK_DIRECT
                    && t->type->user_defined_type->type_information->type->kind == STK_ENUM)
                || (t->type->kind == STK_ENUM)));
}

char is_floating_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && (t->type->builtin_type == BT_FLOAT
                || t->type->builtin_type == BT_DOUBLE));
}

char can_be_promoted_to_dest(type_t* orig, type_t* dest)
{
    simple_type_t* orig_simple_type = orig->type;
    simple_type_t* dest_simple_type = dest->type;

    // A float always can be promoted to double
    if (orig_simple_type->builtin_type == BT_FLOAT
            && dest_simple_type->builtin_type == BT_DOUBLE)
    {
        return 1;
    }

    // A wchar_t can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_WCHAR
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

    // A bool can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_BOOL
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

    // A short, either signed or unsigned, can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_INT
            && orig_simple_type->is_short
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

    // A char, either signed or unsigned, can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_CHAR
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

// #warning Missing the case for bitfields

    // Doesn't look promotionable to me
    return 0;
}

type_t* reference_referenced_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    if (t1->kind == TK_REFERENCE)
    {
        return t1->pointer->pointee;
    }
    else
        return NULL;
}

char is_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1->kind == TK_REFERENCE);
}

char is_reference_related(type_t* t1, type_t* t2, scope_t* st, 
        decl_context_t decl_context)
{
    // cv1 t1 and cv2 t2 are reference related if
    //
    // a) t1 == t2, or if not
    // b) t1 belongs to base(t2), provided t1 and t2 are of class type
    
    cv_qualifier_t cv1 = base_type(t1)->cv_qualifier;
    cv_qualifier_t cv2 = base_type(t2)->cv_qualifier;

    // Ignore outermost
    base_type(t1)->cv_qualifier = CV_NONE;
    base_type(t2)->cv_qualifier = CV_NONE;
    
    if (equivalent_types(t1, t2, st, CVE_CONSIDER, decl_context))
    {
        return 1;
    }
    else if (is_class_type(t1)
            && is_class_type(t2))
    {
        if (is_base_class_of(t1, t2))
        {
            return 1;
        }
    }

    base_type(t1)->cv_qualifier = cv1;
    base_type(t2)->cv_qualifier = cv2;

    return 0;
}

char is_reference_compatible(type_t* t1, type_t* t2, scope_t* st,
        decl_context_t decl_context)
{
    // cv1 t1 and cv2 t2 are reference compatible if
    //
    // a) cv1 t1 and cv2 t2 are reference related
    // b) and cv1 is greater or equal to cv2
    
    if (is_reference_related(t1, t2, st, decl_context))
    {
        // They are references
        // cv_qualifier_t cv1 = base_type(t1)->type->cv_qualifier;
        // cv_qualifier_t cv2 = base_type(t2)->type->cv_qualifier;

        // Fix this
        //
        // cv1 is more qualified if everything in cv2 is also in cv1
        // if ((cv1 | cv2) == cv1)
        // {
        //  return 1;
        // }
        // else
        // {
        //  return 0;
        // }
        return 1;
    }
    else
    {
        return 0;
    }
}

char is_bool_type(type_t* t1)
{
    // Advance over typedefs
    t1 = advance_over_typedefs(t1);

    return (t1->kind == TK_DIRECT
            && t1->type->kind == STK_BUILTIN_TYPE
            && t1->type->builtin_type == BT_BOOL);
}

char can_be_converted_to_dest(type_t* orig, type_t* dest)
{
    simple_type_t* orig_simple_type = orig->type;
    simple_type_t* dest_simple_type = dest->type;

    // Anything can be converted to anything fundamental (except for void
    // types, that in general should not appear in the code as rvalues ...)
    if (orig_simple_type->builtin_type != BT_VOID
            && dest_simple_type->builtin_type != BT_VOID)
    {
        return 1;
    }

    // Does not look convertible
    return 0;
}

scope_entry_t* get_class_symbol(type_t* class_type)
{
    if (is_named_class_type(class_type))
    {
        return class_type->type->user_defined_type;
    }
    else 
        return NULL;
}

type_t* get_class_type(type_t* class_type)
{
    if (is_named_class_type(class_type))
    {
        return class_type->type->user_defined_type->type_information;
    }
    else if (is_unnamed_class_type(class_type))
    {
        return class_type;
    }
    else
    {
        internal_error("This is not a class type!", 0);
    }
}

char is_class_type(type_t* possible_class)
{
    return (is_named_class_type(possible_class) || is_unnamed_class_type(possible_class));
}

char is_unnamed_class_type(type_t* possible_class)
{
    return (possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_CLASS);
}

char is_named_class_type(type_t* possible_class)
{
    return (possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_USER_DEFINED
            && possible_class->type->user_defined_type != NULL
            && possible_class->type->user_defined_type->type_information->kind == TK_DIRECT
            && possible_class->type->user_defined_type->type_information->type->kind == STK_CLASS);
}

char is_base_class_of(type_t* possible_base, type_t* possible_derived)
{
    if (!is_named_class_type(possible_base)
            || !is_named_class_type(possible_derived))
    {
        internal_error("This function expects named class types", 0);
    }

    simple_type_t* derived_class_info = possible_derived->type->user_defined_type->type_information->type;

    int i;
    for (i = 0; i < derived_class_info->class_info->num_bases; i++)
    {
        type_t* current_base = derived_class_info->class_info->base_classes_list[i]->class_type;
        type_t* base_class_info = current_base->type->user_defined_type->type_information;
        
        if (base_class_info == possible_base)
        {
            return 1;
        }
    }

    for (i = 0; i < derived_class_info->class_info->num_bases; i++)
    {
        type_t* current_base = derived_class_info->class_info->base_classes_list[i]->class_type;
        type_t* base_class_info = current_base;
        
        // Now search recursively in the bases of this base
        if (is_base_class_of(possible_base, base_class_info))
        {
            return 1;
        }
    }

    // Not found
    return 0;
}

char pointer_can_be_converted_to_dest_rec(type_t* orig, type_t* dest, scope_t* st, 
        char* all_previous_are_const, char* to_void, char* derived_to_base, char* cv_adjustment,
        decl_context_t decl_context)
{
    /*
     * orig is the original pointer type
     *
     *   int * * b;
     *
     * and dest is the destination pointer type
     *
     *   int * * const c;
     *
     * Example:
     *
     *   void f(int * * const c);
     *   void g()
     *   {
     *      int * * b;
     *      f(b); <-- Valid
     *   }
     *
     * dest has to be more cv-qualified in general than b
     */

    orig = advance_over_typedefs(orig);
    dest = advance_over_typedefs(dest);

    if (orig->kind != dest->kind)
    {
        return 0;
    }

    if (orig->kind != TK_POINTER) 
    {
        if (equivalent_types(orig, dest, st, CVE_IGNORE_OUTERMOST, decl_context)
                || (dest->type->kind == STK_BUILTIN_TYPE
                    && dest->type->builtin_type == BT_VOID))
        {
            // We should ensure that dest is equal or more cv-qualified
            cv_qualifier_t cv_qualif_dest = *(get_outermost_cv_qualifier(dest));
            cv_qualifier_t cv_qualif_orig = *(get_outermost_cv_qualifier(orig));

            *to_void = (dest->type->kind == STK_BUILTIN_TYPE 
                    && dest->type->builtin_type == BT_VOID);

            if ((cv_qualif_dest | cv_qualif_orig) == cv_qualif_dest)
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }
        else if (is_named_class_type(orig) && is_named_class_type(dest))
        {
            // If both are classes check if dest is a base of orig
            // B* can be pointer qualified to A* if A is a base of B
            if (is_base_class_of(dest, orig))
            {
                *derived_to_base = 1;
                return 1;
            }
        }
    }

    // orig->kind == dest->kind == TK_POINTER
    // Example:
    //    orig:  int * * const * *       a;
    //    dest:  int * * const * const * const a;
    //
    //  (orig can be converted to dest)

    // If the orig pointer is qualified, so does have to the dest one
    if ((orig->cv_qualifier | dest->cv_qualifier) != 
            orig->cv_qualifier)
    {
        return 0;
    }

    // If the dest pointer is const-qualified every previous pointer
    // should have been const-qualified
    if ((dest->cv_qualifier & CV_CONST) == CV_CONST)
    {
        if (!(*all_previous_are_const))
        {
            return 0;
        }
        *cv_adjustment = 1;
    }
    else
    {
        *all_previous_are_const = 0;
    }

    return pointer_can_be_converted_to_dest_rec(orig->pointer->pointee, dest->pointer->pointee, st, 
            all_previous_are_const, to_void, derived_to_base, cv_adjustment, decl_context);
}

char pointer_can_be_converted_to_dest(type_t* orig, type_t* dest, scope_t* st, 
        char* to_void, char* derived_to_base, char* cv_adjust,
        decl_context_t decl_context)
{
    // This holds for the first pointer
    char all_previous_are_const = 1;

    *to_void = 0;
    *derived_to_base = 0;
    *cv_adjust = 0;

    return pointer_can_be_converted_to_dest_rec(orig, dest, st,
            &all_previous_are_const, to_void, derived_to_base, 
            cv_adjust, decl_context);
}

/*
 * This function just creates a full type_t from a simple_type_t.
 * It is useful when no declarator information is available.
 */
type_t* simple_type_to_type(simple_type_t* simple_type_info)
{
    type_t* result = calloc(1, sizeof(*result));
    result->kind = TK_DIRECT;
    // result->type = copy_simple_type(simple_type_info);
    result->type = simple_type_info;

    return result;
}

/* Copy functions */

// This function copies the type information of an enum
enum_info_t* copy_enum_info(enum_info_t* enum_info)
{
    enum_info_t* result = calloc(1, sizeof(*result));

    *result = *enum_info;

    result->enumeration_list = 
        calloc(result->num_enumeration, sizeof(*(result->enumeration_list)));

    int i;
    for (i = 0; i < result->num_enumeration; i++)
    {
        // Note, we copy the references here
        result->enumeration_list[i] = enum_info->enumeration_list[i];
    }

    return result;
}

// This function copies the type information of a pointer
pointer_info_t* copy_pointer_info(pointer_info_t* pointer_info)
{
    pointer_info_t* result = calloc(1, sizeof(*result));
    *result = *pointer_info;
    
    result->pointee = copy_type(result->pointee);

    return result;
}

// This function copies the type information of an array
array_info_t* copy_array_info(array_info_t* array_info)
{
    array_info_t* result = calloc(1, sizeof(*result));
    *result = *array_info;
    
    result->array_expr = duplicate_ast(array_info->array_expr);
    result->element_type = copy_type(array_info->element_type);
    
    return result;
}

// This function copies the type information of a function
function_info_t* copy_function_info(function_info_t* function_info)
{
    function_info_t* result = calloc(1, sizeof(*result));
    *result = *function_info;

    result->return_type = copy_type(function_info->return_type);

    result->parameter_list = calloc(function_info->num_parameters,
            sizeof(*(result->parameter_list)));
    
    int i;
    for (i = 0; i < function_info->num_parameters; i++)
    {
        result->parameter_list[i] = calloc(1, sizeof(*(result->parameter_list[i])));
        result->parameter_list[i]->type_info = copy_type(function_info->parameter_list[i]->type_info);
        if (function_info->parameter_list[i]->default_argument != NULL)
        {
            result->parameter_list[i]->default_argument = duplicate_ast(function_info->parameter_list[i]->default_argument);
        }
    }
    
    return result;
}

// This function copies a full fledged type
type_t* copy_type(type_t* type)
{
    type_t* result = calloc(1, sizeof(*result));

    *result = *type;

    if (result->pointer != NULL)
    {
        result->pointer = copy_pointer_info(type->pointer);
    }

    if (result->array != NULL)
    {
        result->array = copy_array_info(type->array);
    }

    if (result->function != NULL)
    {
        result->function = copy_function_info(type->function);
    }

    if (result->type != NULL)
    {
        result->type = copy_simple_type(type->type);
    }

    return result;
}

// This function copies class type information
class_info_t* copy_class_info(class_info_t* class_info)
{
    class_info_t* result = calloc(1, sizeof(*result));

    *result = *class_info;

    return result;
}

template_argument_t* copy_template_argument(template_argument_t* template_argument)
{
    template_argument_t* result = calloc(1, sizeof(*result));

    *result = *template_argument;

    if (template_argument->type != NULL)
    {
        result->type = copy_type(template_argument->type);
    }

    if (template_argument->argument_tree != NULL)
    {
        result->argument_tree = duplicate_ast(template_argument->argument_tree);
    }

    return result;
}

template_argument_list_t* copy_template_argument_list(template_argument_list_t* template_argument_list)
{
    template_argument_list_t* result = calloc(1, sizeof(*result));

    *result = *template_argument_list;

    result->argument_list = calloc(template_argument_list->num_arguments, sizeof(*(result->argument_list)));

    int i;
    for (i = 0; i < template_argument_list->num_arguments; i++)
    {
        result->argument_list[i] = copy_template_argument(template_argument_list->argument_list[i]);
    }

    return result;
}

// This function copies a simple type
simple_type_t* copy_simple_type(simple_type_t* type_info)
{
    simple_type_t* result = calloc(1, sizeof(*result));

    // Bitwise copy for every thing that can be directly copied
    *result = *type_info;

    if (result->enum_info != NULL)
    {
        result->enum_info = copy_enum_info(type_info->enum_info);
    }

    if (result->class_info != NULL)
    {
        result->class_info = copy_class_info(type_info->class_info);
    }

    if (result->template_arguments != NULL)
    {
        result->template_arguments = copy_template_argument_list(type_info->template_arguments);
    }

    return result;
}

char* get_type_spec_name(AST type_spec, scope_t* st)
{
// #warning Improve this function
    char* result = "";
    switch (ASTType(type_spec))
    {
        case AST_SIMPLE_TYPE_SPECIFIER :
            {
                AST global_op = ASTSon0(type_spec);
                AST nested_name = ASTSon1(type_spec);
                AST type_name = ASTSon2(type_spec);

                if (global_op != NULL)
                {
                    result = strappend(result, "::");
                }

                while (nested_name != NULL)
                {
                    AST class_or_namespace = ASTSon0(nested_name);
                    if (ASTType(class_or_namespace) == AST_SYMBOL)
                    {
// #warning Check for template parameters symbols
                        result = strappend(result, ASTText(class_or_namespace));
                    }
                    else // template-id
                    {
                        AST template_name = ASTSon0(class_or_namespace);
                        result = strappend(result, ASTText(template_name));
                        result = strappend(result, "<");
// #warning Add support for template parameters
                        result = strappend(result, ">");
                    }
                    result = strappend(result, "::");

                    nested_name = ASTSon1(nested_name);
                }

                if (ASTType(type_name) == AST_SYMBOL)
                {
// #warning Check for template parameters symbols
                    result = strappend(result, ASTText(type_name));
                }
                else // template-id
                {
                    AST template_name = ASTSon0(type_name);
                    result = strappend(result, ASTText(template_name));
                    result = strappend(result, "<");
// #warning Add support for template parameters
                    result = strappend(result, ">");
                }
                break;
            }
        default:
            {
                internal_error("Unsupported node type '%s'", ast_print_node_type(ASTType(type_spec)));
            }
    }

    return result;
}


cv_qualifier_t get_cv_qualifier(type_t* type_info)
{
    return type_info->cv_qualifier;
}

/** 
 * Debugging functions
 * **/

// Gives the name of a builtin type
char* get_builtin_type_name(simple_type_t* simple_type_info, scope_t* st)
{
    char* result = "";

    if (simple_type_info->is_long)
    {
        result = strappend(result, "long ");
    }

    if (simple_type_info->is_short)
    {
        result = strappend(result, "short ");
    }

    if (simple_type_info->is_unsigned)
    {
        result = strappend(result, "unsigned ");
    }

    switch (simple_type_info->kind)
    {
        case STK_BUILTIN_TYPE :
            {
                switch (simple_type_info->builtin_type)
                {
                    case BT_INT :
                        result = strappend(result, "int");
                        break;
                    case BT_BOOL :
                        result = strappend(result, "bool");
                        break;
                    case BT_FLOAT :
                        result = strappend(result, "float");
                        break;
                    case BT_DOUBLE :
                        result = strappend(result, "double");
                        break;
                    case BT_WCHAR :
                        result = strappend(result, "wchar_t");
                        break;
                    case BT_CHAR :
                        result = strappend(result, "char");
                        break;
                    case BT_VOID :
                        result = strappend(result, "void");
                        break;
                    case BT_UNKNOWN :
                    default :
                        result = strappend(result, "¿¿¿unknown builtin type???");
                        break;
                }
                break;
            }
        case STK_USER_DEFINED :
            {
                const int MAX_LENGTH = 1023;
                char* user_defined_str = calloc(MAX_LENGTH + 1, sizeof(char));
                scope_entry_t* user_defined_type = simple_type_info->user_defined_type;
                switch (user_defined_type->kind)
                {
                    case SK_ENUM :
                        {
                            int max_level = 0;
                            char is_dependent = 0;
                            snprintf(user_defined_str, MAX_LENGTH, "enum %s", 
                                    get_fully_qualified_symbol_name(user_defined_type, user_defined_type->scope, 
                                        &is_dependent, &max_level));
                            break;
                        }
                    case SK_CLASS :
                        {
                            int max_level = 0;
                            char is_dependent = 0;
                            snprintf(user_defined_str, MAX_LENGTH, "class %s", 
                                    get_fully_qualified_symbol_name(user_defined_type, user_defined_type->scope,
                                    &is_dependent, &max_level));
                            break;
                        }
                    case SK_TYPEDEF :
                        snprintf(user_defined_str, MAX_LENGTH, "%s", 
                                print_declarator(advance_over_typedefs(user_defined_type->type_information), st));
                        // snprintf(user_defined_str, MAX_LENGTH, "typedef %s (aliased type: %s)", user_defined_type->symbol_name,
                        //         print_declarator(advance_over_typedefs(user_defined_type->type_information), st));
                        break;
                    case SK_TEMPLATE_TYPE_PARAMETER :
                        snprintf(user_defined_str, MAX_LENGTH, "type template parameter %s #%d nesting=%d", 
                                user_defined_type->symbol_name,
                                user_defined_type->type_information->type->template_parameter_num,
                                user_defined_type->type_information->type->template_parameter_nesting);
                        break;
                    case SK_TEMPLATE_TEMPLATE_PARAMETER :
                        snprintf(user_defined_str, MAX_LENGTH, "template template parameter %s #%d nesting=%d",
                                user_defined_type->symbol_name,
                                user_defined_type->type_information->type->template_parameter_num,
                                user_defined_type->type_information->type->template_parameter_nesting);
                        break;
                    case SK_TEMPLATE_PARAMETER :
                        snprintf(user_defined_str, MAX_LENGTH, "nontype template parameter %s #%d nesting=%d", 
                                user_defined_type->symbol_name,
                                user_defined_type->type_information->type->template_parameter_num,
                                user_defined_type->type_information->type->template_parameter_nesting);
                        break;
                    case SK_TEMPLATE_PRIMARY_CLASS :
                        snprintf(user_defined_str, MAX_LENGTH, "primary template class %s (%p)", 
                                user_defined_type->symbol_name, user_defined_type);
                        break;
                    case SK_TEMPLATE_SPECIALIZED_CLASS :
                        snprintf(user_defined_str, MAX_LENGTH, "specialized template class %s (%p)", 
                                user_defined_type->symbol_name, user_defined_type);
                        break;
                    case SK_GCC_BUILTIN_TYPE :
                        snprintf(user_defined_str, MAX_LENGTH, "__builtin_va_list");
                        break;
                    case SK_DEPENDENT_ENTITY :
                        snprintf(user_defined_str, MAX_LENGTH, "dependent entity");
                        break;
                    default :
                        snprintf(user_defined_str, MAX_LENGTH, "¿¿¿unknown user defined type??? (kind=%d)", user_defined_type->kind);
                }
                result = strappend(result, user_defined_str);
                break;
            }
        case STK_ENUM :
            result = strappend(result, "enum <anonymous>");
            break;
        case STK_CLASS :
            result = strappend(result, "class <anonymous>");
            break;
        case STK_TEMPLATE_TEMPLATE_PARAMETER :
            {
                char temp[256];
                snprintf(temp, 255, "template template parameter #%d nesting=%d", 
                        simple_type_info->template_parameter_num, simple_type_info->template_parameter_nesting);

                result = strappend(result, temp);
            }
            break;
        case STK_TYPE_TEMPLATE_PARAMETER :
            {
                char temp[256];
                snprintf(temp, 255, "template type parameter #%d nesting=%d", 
                        simple_type_info->template_parameter_num, simple_type_info->template_parameter_nesting);

                result = strappend(result, temp);
            }
            break;
        case STK_VA_LIST :
            result = strappend(result, "__builtin_va_list");
            break;
        case STK_TYPEOF :
            result = strappend(result, "__typeof");
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            result = strappend(result, "template dependent type");
            break;
        case STK_TYPEDEF :
            result = strappend(result, print_declarator(advance_over_typedefs(simple_type_info->aliased_type), st));
            break;
        default :
            {
                char c[50];
                snprintf(c, 49, "(unknown simple type = %d)", simple_type_info->kind);
                result = strappend(result, c);
                break;
            }
    }

    return result;
}

// This prints a declarator in English. It is intended for debugging purposes
char* print_declarator(type_t* printed_declarator, scope_t* st)
{
    char* tmp_result = "";

    do 
    {
        if ((printed_declarator->cv_qualifier & CV_CONST) == CV_CONST)
        {
            tmp_result = strappend(tmp_result, "const ");
        }
        if ((printed_declarator->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
        {
            tmp_result = strappend(tmp_result, "volatile ");
        }
        switch (printed_declarator->kind)
        {
            case TK_DIRECT :
                if (printed_declarator->type != NULL)
                {
                    tmp_result = strappend(tmp_result, get_builtin_type_name(printed_declarator->type, st));
                }
                else
                {
                    tmp_result = strappend(tmp_result, "(nothing)");
                }
                printed_declarator = NULL;
                break;
            case TK_POINTER :
                tmp_result = strappend(tmp_result, "pointer to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_REFERENCE :
                tmp_result = strappend(tmp_result, "reference to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_POINTER_TO_MEMBER :
                tmp_result = strappend(tmp_result, "pointer to member of ");
                if (printed_declarator->pointer->pointee_class != NULL)
                {
                    // print_declarator(printed_declarator->pointer->pointee_class->type_information, st);
                    tmp_result = strappend(tmp_result,
                            print_declarator(printed_declarator->pointer->pointee_class->type_information, st)
                          );
                }
                else
                {
                    tmp_result = strappend(tmp_result, "(unknown class)");
                }
                tmp_result = strappend(tmp_result, " to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_ARRAY :
                tmp_result = strappend(tmp_result, "array ");
                if (printed_declarator->array->array_expr != NULL)
                {
                    tmp_result = strappend(tmp_result, prettyprint_in_buffer(printed_declarator->array->array_expr));
                    tmp_result = strappend(tmp_result, " of ");
                }
                else
                {
                    tmp_result = strappend(tmp_result, " of ");
                }
                printed_declarator = printed_declarator->array->element_type;
                break;
            case TK_FUNCTION :
                {
                    int i;
                    tmp_result = strappend(tmp_result, "function");
                    
                    if (printed_declarator->function->num_template_parameters > 0)
                    {
                        tmp_result = strappend(tmp_result, "<");
                        for (i = 0; i < printed_declarator->function->num_template_parameters; i++)
                        {
                            template_parameter_t* template_param = printed_declarator->function->template_parameter_info[i];
                            if (template_param->template_parameter_name != NULL)
                            {
                                tmp_result = strappend(tmp_result, template_param->template_parameter_name);
                            }
                            else
                            {
                                switch (template_param->kind)
                                {
                                    case TPK_NONTYPE :
                                        {
                                            tmp_result = strappend(tmp_result, "(non-type)");
                                            break;
                                        }
                                    case TPK_TYPE :
                                        {
                                            tmp_result = strappend(tmp_result, "(type)");
                                            break;
                                        }
                                    case TPK_TEMPLATE :
                                        {
                                            tmp_result = strappend(tmp_result, "(template)");
                                            break;
                                        }
                                    default :
                                        {
                                        }
                                }
                            }

                            if ((i + 1) < printed_declarator->function->num_template_parameters)
                            {
                                tmp_result = strappend(tmp_result, ", ");
                            }
                        }
                        tmp_result = strappend(tmp_result, ">");
                    }

                    tmp_result = strappend(tmp_result, " (");
                    for (i = 0; i < printed_declarator->function->num_parameters; i++)
                    {
                        if (!printed_declarator->function->parameter_list[i]->is_ellipsis)
                        {
                            tmp_result = strappend(tmp_result, 
                                    print_declarator(printed_declarator->function->parameter_list[i]->type_info, st)
                                  );
                        }
                        else
                        {
                            tmp_result = strappend(tmp_result, "...");
                        }
                        if ((i+1) < printed_declarator->function->num_parameters)
                        {
                            tmp_result = strappend(tmp_result, ", ");
                        }
                    }
                    tmp_result = strappend(tmp_result, ")");
                    tmp_result = strappend(tmp_result, " returning ");
                    printed_declarator = printed_declarator->function->return_type;
                    break;
                }
                // GCC Extension
            default :
                internal_error("Unhandled type kind '%d'\n", printed_declarator->kind);
                break;
        }
    } while (printed_declarator != NULL);

    return tmp_result;
}

// Expression that may appear here are of very limited nature
char is_dependent_expression(AST expression, scope_t* st, decl_context_t decl_context)
{
    switch (ASTType(expression))
    {
        case AST_EXPRESSION : 
        case AST_INITIALIZER :
        case AST_INITIALIZER_EXPR :
        case AST_CONSTANT_INITIALIZER : 
        case AST_CONSTANT_EXPRESSION : 
        case AST_PARENTHESIZED_EXPRESSION :
            {
                return is_dependent_expression(ASTSon0(expression), st, decl_context);
            }
        case AST_INITIALIZER_BRACES :
            {
                AST initializer_list = ASTSon0(expression);
                AST iter;

                for_each_element(initializer_list, iter)
                {
                    AST initializer = ASTSon1(iter);

                    if (is_dependent_expression(initializer, st, decl_context))
                    {
                        return 1;
                    }
                }
                return 0;
            }
        case AST_DESIGNATED_INITIALIZER :
            {
                // [1][2] = 3
                // a.b = 4
                // AST designation = ASTSon0(expression);
                // AST initializer_clause = ASTSon1(expression);

                // TODO - Complete this
                return 0;
                break;
            }
        case AST_DESIGNATION : 
            {
                // [1][2] {= 3}
                // a.b {= 3}
                AST designator_list = ASTSon0(expression);
                AST iter;

                for_each_element(designator_list, iter)
                {
                    AST designator = ASTSon1(iter);

                    if (is_dependent_expression(designator, st, decl_context))
                    {
                        return 1;
                    }
                }

                return 0;
                break;
            }
        case AST_INDEX_DESIGNATOR :
            {
                // [1]{[2] = 3}
                return is_dependent_expression(ASTSon0(expression), st, decl_context);
            }
        case AST_FIELD_DESIGNATOR :
            {
                // a{.b = 3}
                return 0;
            }
            // Primaries
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
        case AST_FLOATING_LITERAL :
        case AST_BOOLEAN_LITERAL :
        case AST_CHARACTER_LITERAL :
        case AST_STRING_LITERAL :
        case AST_THIS_VARIABLE :
            {
                return 0;
            }
        case AST_SYMBOL :
        case AST_QUALIFIED_ID :
            {
                scope_entry_list_t* entry_list = 
                    query_id_expression_flags(st, expression, FULL_UNQUALIFIED_LOOKUP, LF_EXPRESSION, decl_context);

                if (entry_list == NULL)
                {
                    internal_error("Symbol '%s' in '%s' not found\n", prettyprint_in_buffer(expression),
                            node_information(expression));
                }
                scope_entry_t* entry = entry_list->entry;

                if (entry->kind == SK_DEPENDENT_ENTITY
                        || entry->kind == SK_TEMPLATE_PARAMETER)
                {
                    entry->dependency_info = DI_DEPENDENT;
                    return 1;
                }

                // Maybe this is a const-variable initialized with a dependent expression
                if ((entry->kind == SK_VARIABLE
                        || entry->kind == SK_ENUMERATOR))
                {
                    if(entry->dependency_info == DI_UNKNOWN)
                    {
                        if (entry->expression_value != NULL)
                        {
                            // if (is_dependent_expression(entry->expression_value, st, decl_context))
                            if (is_dependent_expression(entry->expression_value, entry->scope, decl_context))
                            {
                                entry->dependency_info = DI_DEPENDENT;
                                return 1;
                            }
                        }
                    }
                    else
                    {
                        // Dependency information has already been computed or
                        // it is being computed now
                        return (entry->dependency_info == DI_DEPENDENT);
                    }
                }

                char result;
                result = is_dependent_type(entry->type_information, decl_context);
                entry->dependency_info = result ? DI_DEPENDENT : DI_NOT_DEPENDENT;
                return result;
            }
            // Postfix expressions
        case AST_ARRAY_SUBSCRIPT :
            {
                return is_dependent_expression(ASTSon0(expression), st, decl_context)
                    || is_dependent_expression(ASTSon1(expression), st, decl_context);
            }
        case AST_FUNCTION_CALL :
            {
                char invoked_dependent = is_dependent_expression(ASTSon0(expression), st, decl_context);

                if (invoked_dependent)
                    return 1;

                AST expression_list = ASTSon1(expression);

                if (expression_list != NULL)
                {
                    AST iter;
                    for_each_element(expression_list, iter)
                    {
                        AST current_expression = ASTSon1(iter);

                        if (is_dependent_expression(current_expression, st, decl_context))
                        {
                            return 1;
                        }
                    }
                }

                return 0;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                AST type_specifier = duplicate_ast(ASTSon0(expression));

                // Create a full-fledged type_specifier_seq
                AST type_specifier_seq = ASTMake3(AST_TYPE_SPECIFIER_SEQ, NULL, 
                        type_specifier, NULL, ASTLine(type_specifier), NULL);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;

                // Fix this
                build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &simple_type_info, 
                        decl_context);

                if (is_dependent_type(simple_type_info, decl_context))
                {
                    return 1;
                }

                AST expression_list = ASTSon1(expression);

                if (expression_list != NULL)
                {
                    AST iter;
                    for_each_element(expression_list, iter)
                    {
                        AST current_expression = ASTSon1(iter);

                        if (is_dependent_expression(current_expression, st, decl_context))
                        {
                            return 1;
                        }
                    }
                }

                return 0;
            }
        case AST_TYPENAME_EXPLICIT_TYPE_CONVERSION :
            {
                // This typename denotes that this will be dependent
                return 1;
            }
        case AST_TYPENAME_TEMPLATE :
        case AST_TYPENAME_TEMPLATE_TEMPLATE :
            {
                // This is always dependent
                return 1;
            }
        case AST_SIZEOF :
            {
                return is_dependent_expression(ASTSon0(expression), st, decl_context);
            }
        case AST_SIZEOF_TYPEID :
            {
                AST type_id = ASTSon0(expression);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;
                // Fix this
                build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
                        decl_context);

                if (abstract_declarator != NULL)
                {
                    type_t* declarator_type = NULL;
                    // Fix this
                    build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
                            &declarator_type, decl_context);
                }

                return is_dependent_type(simple_type_info, decl_context);
            }
        case AST_DERREFERENCE :
        case AST_REFERENCE :
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
                return is_dependent_expression(ASTSon0(expression), st, decl_context);
            }
            // Cast expression
        case AST_CAST_EXPRESSION :
            {
                AST type_id = ASTSon0(expression);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;
                // Fix this
                build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
                        decl_context);

                if (abstract_declarator != NULL)
                {
                    type_t* declarator_type = NULL;
                    // Fix this
                    build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
                            &declarator_type, decl_context);
                }

                if (is_dependent_type(simple_type_info, decl_context))
                {
                    return 1;
                }
                else
                {
                    return is_dependent_expression(ASTSon1(expression), st, decl_context);
                }
            }
        case AST_MULT_OP :
        case AST_DIV_OP :
        case AST_MOD_OP :
        case AST_ADD_OP :
        case AST_MINUS_OP :
        case AST_SHL_OP :
        case AST_SHR_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_EQUAL_OP :
        case AST_DIFFERENT_OP :
        case AST_BITWISE_AND :
        case AST_BITWISE_XOR :
        case AST_BITWISE_OR :
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
            {
                return is_dependent_expression(ASTSon0(expression), st, decl_context)
                    || is_dependent_expression(ASTSon1(expression), st, decl_context);
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
                return is_dependent_expression(ASTSon0(expression), st, decl_context)
                    || is_dependent_expression(ASTSon1(expression), st, decl_context)
                    || is_dependent_expression(ASTSon2(expression), st, decl_context);
            }
            // GCC Extension
        default :
            {
                internal_error("Unexpected node '%s' %s", ast_print_node_type(ASTType(expression)), 
                        node_information(expression));
                break;
            }
            return 0;
    }
}

char is_dependent_simple_type(type_t* type_info, decl_context_t decl_context)
{
    if (type_info->kind != TK_DIRECT)
    {
        internal_error("This function expects a direct type\n", 0);
    }

    simple_type_t* simple_type = type_info->type;

    switch (simple_type->kind)
    {
        case STK_TEMPLATE_DEPENDENT_TYPE :
        case STK_TYPE_TEMPLATE_PARAMETER :
        case STK_TEMPLATE_TEMPLATE_PARAMETER :
            {
                return 1;
                break;
            }
        case STK_TYPEDEF :
            {
                return is_dependent_type(simple_type->aliased_type, decl_context);
                break;
            }
        case STK_USER_DEFINED :
            {
                if (simple_type->user_defined_type->dependency_info == DI_UNKNOWN)
                {
                    // It is being calculated now
                    simple_type->user_defined_type->dependency_info = DI_BUSY;
                    char result = is_dependent_type(simple_type->user_defined_type->type_information, decl_context);

                    simple_type->user_defined_type->dependency_info =
                        (result ? DI_DEPENDENT : DI_NOT_DEPENDENT);
                    return result;
                }
                else
                {
                    return (simple_type->user_defined_type->dependency_info == DI_DEPENDENT);
                }
                break;
            }
        case STK_ENUM :
            {
                enum_info_t* enum_info = simple_type->enum_info;

                int i;
                for (i = 0; i < enum_info->num_enumeration; i++)
                {
                    scope_entry_t* entry = enum_info->enumeration_list[i];

                    if (entry->expression_value != NULL
                            && entry->dependency_info == DI_UNKNOWN)
                    {
                        if (is_dependent_expression(entry->expression_value, entry->scope, decl_context))
                        {
                            entry->dependency_info = DI_DEPENDENT;
                            return 1;
                        }
                        else
                        {
                            entry->dependency_info = DI_NOT_DEPENDENT;
                        }
                    }
                    else
                    {
                        if (entry->dependency_info == DI_DEPENDENT)
                        {
                            return 1;
                        }
                    }
                }

                return 0;
                break;
            }
        case STK_CLASS :
            {
                if (simple_type->template_arguments != NULL)
                {
                    int i;
                    for (i = 0; i < simple_type->template_arguments->num_arguments; i++)
                    {
                        template_argument_t* curr_argument = simple_type->template_arguments->argument_list[i];

                        if (curr_argument->kind != TAK_NONTYPE)
                        {
                            if (is_dependent_type(curr_argument->type, decl_context))
                            {
                                return 1;
                            }
                        }
                        else
                        {
                            if (is_dependent_expression(curr_argument->argument_tree,
                                        curr_argument->scope, decl_context))
                            {
                                return 1;
                            }
                        }
                    }
                }
                return 0;
                break;
            }
        case STK_BUILTIN_TYPE :
        case STK_VA_LIST :
            {
                return 0;
            }
        case STK_TYPEOF :
            {
                internal_error("Not implemented yet\n", 0);
            }
        default :
            {
                internal_error("Unknown simple type kind=%d\n", simple_type->kind);
            }
    }
}

char is_dependent_type(type_t* type, decl_context_t decl_context)
{
    type = advance_over_typedefs(type);

    switch (type->kind)
    {
        case TK_DIRECT :
            {
                return is_dependent_simple_type(type, decl_context);
                break;
            }
        case TK_ARRAY :
            {
                return is_dependent_type(type->array->element_type, decl_context)
                    || is_dependent_expression(type->array->array_expr,
                            type->array->array_expr_scope, decl_context);
                break;
            }
        case TK_FUNCTION :
            {
                if (type->function->return_type != NULL
                        && is_dependent_type(type->function->return_type, decl_context))
                {
                    return 1;
                }

                int i;
                for (i = 0; i < type->function->num_parameters; i++)
                {
                    if (!type->function->parameter_list[i]->is_ellipsis
                            && is_dependent_type(type->function->parameter_list[i]->type_info, decl_context))
                    {
                        return 1;
                    }
                }

                return 0;
                break;
            }
        case TK_POINTER :
        case TK_REFERENCE :
            {
                return is_dependent_type(type->pointer->pointee, decl_context);
                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                return is_dependent_type(type->pointer->pointee, decl_context)
                    || is_dependent_type(type->pointer->pointee_class->type_information, decl_context);
                break;
            }
        default:
            {
                internal_error("Unknown type kind %d\n", type->kind);
                break;
            }
    }
}

#if 0
char is_dependent_tree(AST tree, scope_t* st)
{
    if (tree == NULL)
    {
        return 0;
    }

    if (ASTType(tree) == AST_SYMBOL)
    {
        char* name = ASTText(tree);

        DEBUG_CODE()
        {
            fprintf(stderr, "Checking if '%s' is dependent\n", name);
        }

        scope_entry_list_t* result_list = query_unqualified_name(st, name);

        // Ignore things not found
        if (result_list == NULL)
        {
            return 0;
        }

        scope_entry_t* result = result_list->entry;

        if (result->kind == SK_TEMPLATE_TYPE_PARAMETER
                || result->kind == SK_TEMPLATE_PARAMETER
                || result->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Name '%s' is dependent\n", name);
            }
            return 1;
        }
        else if (result->kind == SK_TYPEDEF)
        {
        }
        else if (result->kind == SK_TEMPLATE_PRIMARY_CLASS)
        {
            // This is always something with dependent nature !
            // because primary templates only match with themselves
            DEBUG_CODE()
            {
                fprintf(stderr, "Primary template '%s' is dependent\n", name);
            }
        }
        else if (result->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
        {
            // A template specialized class can be dependent or not depending
            // on its arguments
            DEBUG_CODE()
            {
                fprintf(stderr, "Symbol '%s' is a specialized template, checking if it is dependent\n", 
                        name);
            }

            int i;
            char is_dependent_arg = 0;
            for (i = 0; 
                    (i < result->type_information->type->template_arguments->num_arguments) &&
                    !is_dependent_arg; i++)
            {
                template_argument_t* curr_argument = 
                    result->type_information->type->template_arguments->argument_list[i];

                if (curr_argument->argument_tree != NULL
                        && curr_argument->scope != NULL)
                {
                    is_dependent_arg |= is_dependent_tree(curr_argument->argument_tree, curr_argument->scope);
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "This template argument is incorrect\n");
                    }
                }
            }

            DEBUG_CODE()
            {
                if (is_dependent_arg)
                {
                    fprintf(stderr, "This specialized template is dependent\n");
                }
                else
                {
                    fprintf(stderr, "This specialized template is not dependent\n");
                }
            }

            return is_dependent_arg;
        }
        else
        {
            type_t* t = result->type_information;

            // Some entities do not have type information (like namespaces)
            if (t != NULL)
            {
                t = advance_over_typedefs(t);

                if (t->kind == TK_DIRECT 
                        && t->type->kind == STK_USER_DEFINED)
                {
                    t = t->type->user_defined_type->type_information;
                }

                t = advance_over_typedefs(t);

                if (t->kind == TK_DIRECT
                        && (t->type->kind == STK_TEMPLATE_DEPENDENT_TYPE
                            || t->type->kind == STK_TYPE_TEMPLATE_PARAMETER
                            || t->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Type of name '%s' is dependent\n", name);
                    }
                    return 1;
                }
                DEBUG_CODE()
                {
                    print_declarator(t, st);
                    fprintf(stderr, "\n");
                }
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Name '%s' is not dependent\n", name);
        }
        return 0;
    }
    else
    {
        return is_dependent_tree(ASTSon0(tree), st)
            || is_dependent_tree(ASTSon1(tree), st)
            || is_dependent_tree(ASTSon2(tree), st)
            || is_dependent_tree(ASTSon3(tree), st);
    }
}
#endif

// This jumps over user defined types and typedefs
scope_entry_t* give_real_entry(scope_entry_t* entry)
{
    scope_entry_t* result = entry;

    type_t* t = entry->type_information;

    if (t != NULL)
    {
        t = advance_over_typedefs(t);
    }

    while (t != NULL 
            && t->kind == TK_DIRECT
            && t->type->kind == STK_USER_DEFINED)
    {
        result = t->type->user_defined_type;
        t = result->type_information;
        if (t != NULL)
        {
            t = advance_over_typedefs(t);
        }
    }

    if (result->injected_class_name)
    {
        result = result->injected_class_referred_symbol;
    }

    return result;
}

static char* get_cv_qualifier_string(type_t* type_info)
{
    char* result = "";

    if (BITMAP_TEST(type_info->cv_qualifier, CV_CONST))
    {
        result = strappend(result, "const ");
    }

    if (BITMAP_TEST(type_info->cv_qualifier, CV_VOLATILE))
    {
        result = strappend(result, "volatile ");
    }

    if (BITMAP_TEST(type_info->cv_qualifier, CV_RESTRICT))
    {
        // Be conservative for now
        // C_LANGUAGE()
        // {
        //     result = strappend(result, "restrict ");
        // }
        // CXX_LANGUAGE()
        {
            result = strappend(result, "__restrict ");
        }
    }

    return result;
}


// States if a declarator of this type will need parentheses
static char declarator_needs_parentheses(type_t* type_info)
{
    char result = 0;
    if (type_info->kind == TK_POINTER_TO_MEMBER
            || type_info->kind == TK_POINTER
            || type_info->kind == TK_REFERENCE)
    {
        type_t* pointee = type_info->pointer->pointee;
        result = (pointee->kind != TK_POINTER_TO_MEMBER
                && pointee->kind != TK_POINTER
                && pointee->kind != TK_REFERENCE
                && pointee->kind != TK_DIRECT);
    }

    return result;
}

// Gives a string with the name of this simple type
static char* get_simple_type_name_string_internal(scope_t* st, simple_type_t* simple_type)
{
    char* result = strdup("");
    switch ((int)simple_type->kind)
    {
        case STK_USER_DEFINED :
            {
                // Fix this
                char is_dependent = 0;
                int max_level = 0;
                result = get_fully_qualified_symbol_name(simple_type->user_defined_type,
                        st, &is_dependent, &max_level);

                if (is_dependent && max_level >= 1)
                {
                    result = strappend("typename ", result);
                }
                fprintf(stderr, "-> %s [is_dep=%d][max_level=%d]\n", result, is_dependent, max_level);
                break;
            }
        case STK_TYPEOF :
            {
                result = "__typeof_not_supported_yet__";
                break;
            }
        case STK_VA_LIST :
            {
                result = "__builtin_va_list";
                break;
            }
        case STK_BUILTIN_TYPE :
            {
                if (simple_type->is_unsigned)
                {
                    result = "unsigned ";
                }
                else if (simple_type->is_signed)
                {
                    result = "signed ";
                }

                if (simple_type->is_long == 1)
                {
                    result = strappend(result, "long ");
                }
                else if (simple_type->is_long >= 2)
                {
                    result = strappend(result, "long long ");
                }
                else if (simple_type->is_short)
                {
                    result = strappend(result, "short ");
                }

                switch ((int)simple_type->builtin_type)
                {
                    case BT_INT :
                        {
                            result = strappend(result, "int ");
                            break;
                        }
                    case BT_CHAR :
                        {
                            result = strappend(result, "char ");
                            break;
                        }
                    case BT_WCHAR :
                        {
                            result = strappend(result, "wchar_t ");
                            break;
                        }
                    case BT_FLOAT :
                        {
                            result = strappend(result, "float ");
                            break;
                        }
                    case BT_DOUBLE :
                        {
                            result = strappend(result, "double ");
                            break;
                        }
                    case BT_BOOL :
                        {
                            result = strappend(result, "bool ");
                            break;
                        }
                    case BT_VOID :
                        {
                            result = strappend(result, "void ");
                            break;
                        }
                    case BT_UNKNOWN :
                        {
                            result = strappend(result, " ");
                            break;
                        }
                    default :
                        break;
                }
                break;
            }
        case STK_CLASS :
            {
                internal_error("Type STK_CLASS invalid\n", 0);
                break;
            }
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                // internal_error("STK_TEMPLATE_DEPENDENT_TYPE not implemented\n", 0);
                result = prettyprint_in_buffer(simple_type->typeof_expr);
                break;
            }
        default:
            {
                internal_error("Unknown simple type kind '%d'\n", simple_type->kind);
                break;
            }
    }

    return result;
}

// Gives the simple type name of a full fledged type
char* get_simple_type_name_string(scope_t* st, type_t* type_info)
{
    char* result = strdup("");
    switch ((int)(type_info->kind))
    {
        case TK_DIRECT :
            {
                result = get_cv_qualifier_string(type_info);
                result = strappend(result, get_simple_type_name_string_internal(st, type_info->type));
                return result;
                break;
            }
        case TK_FUNCTION :
            {
                result = get_simple_type_name_string(st, type_info->function->return_type);
                break;
            }
        case TK_POINTER :
        case TK_REFERENCE :
        case TK_POINTER_TO_MEMBER :
            {
                result = get_simple_type_name_string(st, type_info->pointer->pointee);
                break;
            }
        case TK_ARRAY :
            {
                result = get_simple_type_name_string(st, type_info->array->element_type);
                break;
            }
        default:
            break;
    }
    return result;
}

static char* get_type_name_string(scope_t* st,
        type_t* type_info, 
        const char* symbol_name,
        int* num_parameter_names,
        char*** parameter_names);

// Returns a declaration string given a type, a symbol name, an optional initializer
// and a semicolon
char* get_declaration_string_internal(type_t* type_info, 
        scope_t* st,
        const char* symbol_name, const char* initializer, 
        char semicolon,
        int* num_parameter_names,
        char*** parameter_names)
{
    char* base_type_name = get_simple_type_name_string(st, type_info);
    char* declarator_name = get_type_name_string(st, type_info, symbol_name, 
            num_parameter_names, parameter_names);

    char* result;

    result = base_type_name;
    result = strappend(result, " ");

    result = strappend(result, declarator_name);

    // FIXME Should check if copy-constructor is not flagged as "explicit"
    // (for parameters this can be useful to declare default arguments)
    if (strcmp(initializer, "") != 0)
    {
        result = strappend(result, " = ");
        result = strappend(result, initializer);
    }

    if (semicolon)
    {
        result = strappend(result, ";");
    }

    return result;
}

static void get_type_name_str_internal(scope_t* st,
        type_t* type_info, 
        char** left,
        char** right,
        int* num_parameter_names,
        char*** parameter_names);

static char* get_type_name_string(scope_t* st,
        type_t* type_info, 
        const char* symbol_name,
        int* num_parameter_names,
        char*** parameter_names)
{
    char* left = strdup("");
    char* right = strdup("");
    get_type_name_str_internal(st, type_info, &left, &right, 
            num_parameter_names, parameter_names);

    char* result = strappend(left, symbol_name);
    result = strappend(result, right);

    return result;
}


// Constructs a proper declarator
static void get_type_name_str_internal(scope_t* st,
        type_t* type_info, 
        char** left,
        char** right,
        int* num_parameter_names,
        char*** parameter_names)
{
    switch (type_info->kind)
    {
        case TK_DIRECT :
            {
                break;
            }
        case TK_POINTER :
            {
                get_type_name_str_internal(st, type_info->pointer->pointee, left, right, 
                        num_parameter_names, parameter_names);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), "*");
                (*left) = strappend((*left), get_cv_qualifier_string(type_info));

                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                get_type_name_str_internal(st, type_info->pointer->pointee, left, right, 
                        num_parameter_names,
                        parameter_names);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), type_info->pointer->pointee_class->symbol_name);

                (*left) = strappend((*left), "::");
                (*left) = strappend((*left), "*");
                (*left) = strappend((*left), get_cv_qualifier_string(type_info));


                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_REFERENCE :
            {
                get_type_name_str_internal(st, type_info->pointer->pointee, left, right, 
                        num_parameter_names, parameter_names);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), "&");

                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_ARRAY :
            {
                char* array_expr = strappend("[", prettyprint_in_buffer(type_info->array->array_expr));
                array_expr = strappend(array_expr, "]");

                (*right) = strappend((*right), array_expr);

                get_type_name_str_internal(st, type_info->array->element_type, left, right, 
                        num_parameter_names, parameter_names);

                break;
            }
        case TK_FUNCTION :
            {
                get_type_name_str_internal(st, type_info->function->return_type, left, right, 
                        num_parameter_names, parameter_names);

                char* prototype;
                prototype = "(";
                int i;
                for (i = 0; i < type_info->function->num_parameters; i++)
                {
                    if (i > 0)
                    {
                        prototype = strappend(prototype, ", ");
                    }

                    if (type_info->function->parameter_list[i]->is_ellipsis)
                    {
                        prototype = strappend(prototype, "...");
                    }
                    else
                    {
                        if (parameter_names == NULL)
                        {
                            // Abstract declarator
                            prototype = strappend(prototype,
                                    get_declaration_string_internal(type_info->function->parameter_list[i]->type_info, st, 
                                        "", "", 0, NULL, NULL));
                        }
                        else
                        {
                            // We create a name
                            char* parameter_name = calloc(20, sizeof(char));
                            snprintf(parameter_name, 19, "_p_%d", i);

                            P_LIST_ADD((*parameter_names), (*num_parameter_names), parameter_name);

                            prototype = strappend(prototype,
                                    get_declaration_string_internal(type_info->function->parameter_list[i]->type_info, st, 
                                        parameter_name, "", 0, NULL, NULL));
                        }
                    }
                }
                prototype = strappend(prototype, ") ");
                prototype = strappend(prototype, get_cv_qualifier_string(type_info));

                (*right) = strappend((*right), prototype);
                break;
            }
        default:
            {
                fprintf(stderr, "Unknown type kind '%d'\n", (int)type_info->type);
                break;
            }
    }
}
