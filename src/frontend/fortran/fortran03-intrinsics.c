#include "fortran03-intrinsics.h"
#include "cxx-ast.h"
#include "cxx-utils.h"
#include "cxx-scope-decls.h"
#include "cxx-entrylist.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"
#include <string.h>
#include "red_black_tree.h"

#define FORTRAN_INTRINSIC_GENERIC_LIST \
FORTRAN_GENERIC_INTRINSIC(abs, "A") \
FORTRAN_GENERIC_INTRINSIC(achar, "I,?KIND") \
FORTRAN_GENERIC_INTRINSIC(acos, "X") \
FORTRAN_GENERIC_INTRINSIC(acosh, "X") \
FORTRAN_GENERIC_INTRINSIC(adjustl, "STRING") \
FORTRAN_GENERIC_INTRINSIC(adjustr, "STRING") \
FORTRAN_GENERIC_INTRINSIC(aimag, "Z") \
FORTRAN_GENERIC_INTRINSIC(aint, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC(all, "MASK,?DIM") \
FORTRAN_GENERIC_INTRINSIC_2(allocated, "ARRAY", "SCALAR") \
FORTRAN_GENERIC_INTRINSIC(anint, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC(any, "MASK,?DIM") \
FORTRAN_GENERIC_INTRINSIC(asin, "X") \
FORTRAN_GENERIC_INTRINSIC(asinh, "X") \
FORTRAN_GENERIC_INTRINSIC(associated, "POINTER,?TARGET") \
FORTRAN_GENERIC_INTRINSIC_2(atan, "X", "Y,X") \
FORTRAN_GENERIC_INTRINSIC(atan2, "Y,X") \
FORTRAN_GENERIC_INTRINSIC(atanh, "X") \
FORTRAN_GENERIC_INTRINSIC(atomic_define, "ATOM,VALUE") \
FORTRAN_GENERIC_INTRINSIC(atomic_ref, "VALUE,ATOM") \
FORTRAN_GENERIC_INTRINSIC(bessel_j0, "X") \
FORTRAN_GENERIC_INTRINSIC(bessel_j1, "X") \
FORTRAN_GENERIC_INTRINSIC_2(bessel_jn, "N,X", "N1,N2,X") \
FORTRAN_GENERIC_INTRINSIC(bessel_y0, "X") \
FORTRAN_GENERIC_INTRINSIC(bessel_y1, "X") \
FORTRAN_GENERIC_INTRINSIC_2(bessel_yn, "N,X", "N1,N2,X") \
FORTRAN_GENERIC_INTRINSIC(bge, "I,J") \
FORTRAN_GENERIC_INTRINSIC(bgt, "I,J") \
FORTRAN_GENERIC_INTRINSIC(ble, "I,J") \
FORTRAN_GENERIC_INTRINSIC(blt, "I,J") \
FORTRAN_GENERIC_INTRINSIC(bit_size, "I") \
FORTRAN_GENERIC_INTRINSIC(btest, "I,POS") \
FORTRAN_GENERIC_INTRINSIC(ceiling, "A,KIND") \
FORTRAN_GENERIC_INTRINSIC(char, "I,KIND") \
FORTRAN_GENERIC_INTRINSIC(cmplx, "X,?Y,?KIND") \
FORTRAN_GENERIC_INTRINSIC(command_argument_count, "") \
FORTRAN_GENERIC_INTRINSIC(conjg, "Z") \
FORTRAN_GENERIC_INTRINSIC(cos, "X") \
FORTRAN_GENERIC_INTRINSIC(cosh, "X") \
FORTRAN_GENERIC_INTRINSIC(count, "MASK,?DIM,?KIND") \
FORTRAN_GENERIC_INTRINSIC(cpu_time, "TIME") \
FORTRAN_GENERIC_INTRINSIC(cshift, "ARRAY,SHIFT,?DIM") \
FORTRAN_GENERIC_INTRINSIC(date_and_time, "?DATE,?TIME,?ZONE,?VALUES") \
FORTRAN_GENERIC_INTRINSIC(dble, "A") \
FORTRAN_GENERIC_INTRINSIC(digits, "X") \
FORTRAN_GENERIC_INTRINSIC(dim, "X,Y") \
FORTRAN_GENERIC_INTRINSIC(dot_product, "VECTOR_A,VECTOR_B") \
FORTRAN_GENERIC_INTRINSIC(dprod, "X,Y") \
FORTRAN_GENERIC_INTRINSIC(dshiftl, "I,J,SHIFT") \
FORTRAN_GENERIC_INTRINSIC(dshiftr, "I,J,SHIFT") \
FORTRAN_GENERIC_INTRINSIC(eoshift, "ARRAY,SHIFT,?BOUNDARY,?DIM") \
FORTRAN_GENERIC_INTRINSIC(epsilon, "X") \
FORTRAN_GENERIC_INTRINSIC(erf, "X") \
FORTRAN_GENERIC_INTRINSIC(erfc, "X") \
FORTRAN_GENERIC_INTRINSIC(erfc_scaled, "X") \
FORTRAN_GENERIC_INTRINSIC(execute_command_line, "COMMAND,?WAIT,?EXITSTAT,?CMDSTAT,?CMDMSG") \
FORTRAN_GENERIC_INTRINSIC(exp, "X") \
FORTRAN_GENERIC_INTRINSIC(exponent, "X") \
FORTRAN_GENERIC_INTRINSIC(extends_type_of, "A,MOLD") \
FORTRAN_GENERIC_INTRINSIC_2(findloc, "ARRAY,VALUE,DIM,?MASK,?KIND,?BACK", "ARRAY,VALUE,?MASK,?KIND,?BACK") \
FORTRAN_GENERIC_INTRINSIC(floor, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC(fraction, "X") \
FORTRAN_GENERIC_INTRINSIC(gamma, "X") \
FORTRAN_GENERIC_INTRINSIC(get_command, "?COMMAND,?LENGTH,?STATUS") \
FORTRAN_GENERIC_INTRINSIC(get_command_argument, "NUMBER,?VALUE,?LENGTH,?STATUS") \
FORTRAN_GENERIC_INTRINSIC(get_environment_variable, "NUMBER,?VALUE,?LENGTH,?STATUS") \
FORTRAN_GENERIC_INTRINSIC(huge, "X") \
FORTRAN_GENERIC_INTRINSIC(hypot, "X,Y") \
FORTRAN_GENERIC_INTRINSIC(iachar, "C,?KIND") \
FORTRAN_GENERIC_INTRINSIC_2(iall, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(iand, "I,J") \
FORTRAN_GENERIC_INTRINSIC_2(iany, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(ibclr, "I,POS") \
FORTRAN_GENERIC_INTRINSIC(ibits, "I,POS,LEN") \
FORTRAN_GENERIC_INTRINSIC(ibset, "I,POS") \
FORTRAN_GENERIC_INTRINSIC(ichar, "C,?KIND") \
FORTRAN_GENERIC_INTRINSIC(ieor, "I,J") \
FORTRAN_GENERIC_INTRINSIC(image_index, "COARRAY,SUB") \
FORTRAN_GENERIC_INTRINSIC(index, "STRING,SUBSTRING,?BACK,?KIND") \
FORTRAN_GENERIC_INTRINSIC(int, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC(ior, "I,J") \
FORTRAN_GENERIC_INTRINSIC_2(iparity, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(ishft, "I,SHIFT") \
FORTRAN_GENERIC_INTRINSIC(ishftc, "I,SHIFT,?SIZE") \
FORTRAN_GENERIC_INTRINSIC(is_contiguous, "ARRAY") \
FORTRAN_GENERIC_INTRINSIC(is_iostat_end, "I") \
FORTRAN_GENERIC_INTRINSIC(is_iostat_eor, "I") \
FORTRAN_GENERIC_INTRINSIC(kind, "X") \
FORTRAN_GENERIC_INTRINSIC(lbound, "ARRAY,?DIM,?KIND") \
FORTRAN_GENERIC_INTRINSIC(lcobound, "COARRAY,?DIM,?KIND") \
FORTRAN_GENERIC_INTRINSIC(leadz, "I") \
FORTRAN_GENERIC_INTRINSIC(len, "STRING,?KIND") \
FORTRAN_GENERIC_INTRINSIC(len_trim, "STRING,?KIND") \
FORTRAN_GENERIC_INTRINSIC(lge, "STRING_A,STRING_B") \
FORTRAN_GENERIC_INTRINSIC(lgt, "STRING_A,STRING_B") \
FORTRAN_GENERIC_INTRINSIC(lle, "STRING_A,STRING_B") \
FORTRAN_GENERIC_INTRINSIC(llt, "STRING_A,STRING_B") \
FORTRAN_GENERIC_INTRINSIC(log, "X") \
FORTRAN_GENERIC_INTRINSIC(log_gamma, "X") \
FORTRAN_GENERIC_INTRINSIC(log10, "X") \
FORTRAN_GENERIC_INTRINSIC(logical, "L,?KIND") \
FORTRAN_GENERIC_INTRINSIC(maskl, "I,?KIND") \
FORTRAN_GENERIC_INTRINSIC(maskr, "I,?KIND") \
FORTRAN_GENERIC_INTRINSIC(matmul, "MATRIX_A,MATRIX_B") \
FORTRAN_GENERIC_INTRINSIC(max, NULL) \
FORTRAN_GENERIC_INTRINSIC(maxexponent, "X") \
FORTRAN_GENERIC_INTRINSIC_2(maxloc, "ARRAY,DIM,?MASK,?KIND,?BACK", "ARRAY,?MASK,?KIND,?BACK") \
FORTRAN_GENERIC_INTRINSIC_2(maxval, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(merge, "TSOURCE,FSOURCE,MASK") \
FORTRAN_GENERIC_INTRINSIC(merge_bits, "I,J,MASK") \
FORTRAN_GENERIC_INTRINSIC(min, NULL) \
FORTRAN_GENERIC_INTRINSIC(minexponent, "X") \
FORTRAN_GENERIC_INTRINSIC_2(minloc, "ARRAY,DIM,?MASK,?KIND,?BACK", "ARRAY,?MASK,?KIND,?BACK") \
FORTRAN_GENERIC_INTRINSIC_2(minval, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(mod, "A,P") \
FORTRAN_GENERIC_INTRINSIC(modulo, "A,P") \
FORTRAN_GENERIC_INTRINSIC(move_alloc, "FROM,TO") \
FORTRAN_GENERIC_INTRINSIC(mvbits, "FROM,FROMPOS,LEN,TO,TOPOS") \
FORTRAN_GENERIC_INTRINSIC(nearest, "X,S") \
FORTRAN_GENERIC_INTRINSIC(new_line, "A") \
FORTRAN_GENERIC_INTRINSIC(nint, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC(not, "I") \
FORTRAN_GENERIC_INTRINSIC(norm2, "X,?DIM") \
FORTRAN_GENERIC_INTRINSIC(null, "?MOLD") \
FORTRAN_GENERIC_INTRINSIC(num_images, "") \
FORTRAN_GENERIC_INTRINSIC(pack, "ARRAY,MASK,?VECTOR") \
FORTRAN_GENERIC_INTRINSIC(parity, "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(popcnt, "I") \
FORTRAN_GENERIC_INTRINSIC(poppar, "I") \
FORTRAN_GENERIC_INTRINSIC(precision, "X") \
FORTRAN_GENERIC_INTRINSIC(present, "A") \
FORTRAN_GENERIC_INTRINSIC_2(product, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(radix, "X") \
FORTRAN_GENERIC_INTRINSIC(random_number, "HARVEST") \
FORTRAN_GENERIC_INTRINSIC(random_seed, "SIZE,PUT,GET") \
FORTRAN_GENERIC_INTRINSIC(range, "X") \
FORTRAN_GENERIC_INTRINSIC(real, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC(repeat, "STRING,NCOPIES") \
FORTRAN_GENERIC_INTRINSIC(reshape, "SOURCE,SHAPE,?PAD,?ORDER") \
FORTRAN_GENERIC_INTRINSIC(rrspacing, "X") \
FORTRAN_GENERIC_INTRINSIC(same_type_as, "A,B") \
FORTRAN_GENERIC_INTRINSIC(scale, "X,I") \
FORTRAN_GENERIC_INTRINSIC(scan, "STRING,SET,?BACK,?KIND") \
FORTRAN_GENERIC_INTRINSIC(selected_char_kind, "NAME") \
FORTRAN_GENERIC_INTRINSIC(selected_int_kind, "R") \
FORTRAN_GENERIC_INTRINSIC(selected_real_kind, "?P,?R,?RADIX") \
FORTRAN_GENERIC_INTRINSIC(set_exponent, "X,I") \
FORTRAN_GENERIC_INTRINSIC(shape, "SOURCE,?KIND") \
FORTRAN_GENERIC_INTRINSIC(shifta, "I,SHIFT") \
FORTRAN_GENERIC_INTRINSIC(shiftl, "I,SHIFT") \
FORTRAN_GENERIC_INTRINSIC(shiftr, "I,SHIFT") \
FORTRAN_GENERIC_INTRINSIC(sign, "A,B") \
FORTRAN_GENERIC_INTRINSIC(sin, "X") \
FORTRAN_GENERIC_INTRINSIC(sinh, "X") \
FORTRAN_GENERIC_INTRINSIC(size, "ARRAY,?DIM,?KIND") \
FORTRAN_GENERIC_INTRINSIC(spacing, "X") \
FORTRAN_GENERIC_INTRINSIC(spread, "SOURCE,DIM,NCOPIES") \
FORTRAN_GENERIC_INTRINSIC(sqrt, "X") \
FORTRAN_GENERIC_INTRINSIC(storage_size, "A,?KIND") \
FORTRAN_GENERIC_INTRINSIC_2(sum, "ARRAY,DIM,?MASK", "ARRAY,?MASK") \
FORTRAN_GENERIC_INTRINSIC(system_clock, "?COUNT,?COUNT_RATE,?COUNT_MAX") \
FORTRAN_GENERIC_INTRINSIC(tan, "X") \
FORTRAN_GENERIC_INTRINSIC(tanh, "X") \
FORTRAN_GENERIC_INTRINSIC_2(this_image, "", "COARRAY,?DIM") \
FORTRAN_GENERIC_INTRINSIC(tiny, "X") \
FORTRAN_GENERIC_INTRINSIC(trailz, "I") \
FORTRAN_GENERIC_INTRINSIC(transfer, "SOURCE,MOLD,SIZE") \
FORTRAN_GENERIC_INTRINSIC(transpose, "MATRIX") \
FORTRAN_GENERIC_INTRINSIC(trim, "STRING") \
FORTRAN_GENERIC_INTRINSIC(ubound, "ARRAY,?DIM,?KIND") \
FORTRAN_GENERIC_INTRINSIC(ucobound, "COARRAY,?DIM,?KIND") \
FORTRAN_GENERIC_INTRINSIC(unpack, "VECTOR,MASK,FIELD") \
FORTRAN_GENERIC_INTRINSIC(verify, "STRING,SET,?BACK,?KIND") 


#define MAX_KEYWORDS_INTRINSICS 6

typedef struct intrinsic_variant_info_tag
{
    int num_keywords;
    const char* keyword_names[MAX_KEYWORDS_INTRINSICS];
    char is_optional[MAX_KEYWORDS_INTRINSICS];
} intrinsic_variant_info_t;

typedef struct intrinsic_info_tag
{
    const char* intrinsic_name;
    int num_variants;
    intrinsic_variant_info_t variants[2];
} intrinsic_info_t;

static intrinsic_variant_info_t get_variant(const char* keywords)
{
    int keyword_index = 0;
    intrinsic_variant_info_t result;
    memset(&result, 0, sizeof(result));
    if (keywords != NULL)
    {
        char *c = strdup(keywords);
        char *p = strtok(c, ",");
        while (p != NULL)
        {
            ERROR_CONDITION(keyword_index == MAX_KEYWORDS_INTRINSICS, 
                    "Too many keywords for intrinsic!\n", 0);

            char is_optional = (*p == '?');
            if (is_optional)
                p++;

            result.keyword_names[keyword_index] = uniquestr(strtolower(p));
            result.is_optional[keyword_index] = is_optional;

            p = strtok(NULL, ",");
            keyword_index++;
        }
        result.num_keywords = keyword_index;
        free(c);
    }
    else
    {
        // Special case, we allow anything
        result.num_keywords = -1;
    }

    return result;
}

#define FORTRAN_GENERIC_INTRINSIC(name, _) \
 static intrinsic_info_t intrinsic_info_##name; \
 static scope_entry_t* check_intrinsic_##name(decl_context_t, char, AST, AST); \
 static void register_generic_intrinsic_##name(decl_context_t decl_context) \
 { \
    scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, #name); \
    if (entry_list == NULL) \
    { \
       scope_entry_t* new_sym = new_symbol(decl_context, decl_context.current_scope, #name); \
       new_sym->kind = SK_FUNCTION; \
       new_sym->type_information = get_void_type(); \
       new_sym->do_not_print = 1; \
       new_sym->entity_specs.is_builtin = 1; \
       new_sym->entity_specs.builtin_check = check_intrinsic_##name; \
    } \
    else \
    { \
        entry_list_free(entry_list); \
    } \
 }
#define FORTRAN_GENERIC_INTRINSIC_2(name, _, __) FORTRAN_GENERIC_INTRINSIC(name, _)
FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
#undef FORTRAN_GENERIC_INTRINSIC_2

typedef
struct intrinsic_descr_tag
{
    const char* name;
    type_t* result;
    int num_types;
    type_t** parameters;
} intrinsic_descr_t;

static int compare_types(type_t* t1, type_t* t2)
{
    if (equivalent_types(t1, t2))
        return 0;
    else if (t1 < t2)
        return -1;
    else
        return 1;
}

static int intrinsic_descr_cmp(const void* i1, const void* i2)
{
    const intrinsic_descr_t* d1 = (const intrinsic_descr_t*)i1;
    const intrinsic_descr_t* d2 = (const intrinsic_descr_t*)i2;

    int c; 
    if ((c = strcasecmp(d1->name, d2->name)) != 0)
        return c;

    if ((c = compare_types(d1->result, d2->result)) != 0)
        return c;

    if (d1->num_types != d2->num_types)
    {
        if (d1->num_types < d2->num_types)
            return -1;
        else
            return 1;
    }

    int i;
    for (i = 0; i < d1->num_types; i++)
    {
        if ((c = compare_types(d1->parameters[i], d2->parameters[i])) != 0)
            return c;
    }

    return 0;
}

static rb_red_blk_tree* intrinsic_map = NULL;

static void null_dtor_func(const void *v UNUSED_PARAMETER) { }

void fortran_init_intrisics(decl_context_t decl_context)
{
#define FORTRAN_GENERIC_INTRINSIC(name, keywords0) \
    intrinsic_info_##name.intrinsic_name = uniquestr(#name); \
    intrinsic_info_##name.num_variants = 1; \
    intrinsic_info_##name.variants[0] = get_variant(keywords0); \
    register_generic_intrinsic_##name(decl_context); \

#define FORTRAN_GENERIC_INTRINSIC_2(name, keywords0, keywords1) \
    intrinsic_info_##name.intrinsic_name = uniquestr(#name); \
    intrinsic_info_##name.num_variants = 2; \
    intrinsic_info_##name.variants[0] = get_variant(keywords0); \
    intrinsic_info_##name.variants[1] = get_variant(keywords1); \
    register_generic_intrinsic_##name(decl_context); \

FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
#undef FORTRAN_GENERIC_INTRINSIC_2

    intrinsic_map = rb_tree_create(intrinsic_descr_cmp, null_dtor_func, null_dtor_func);
}

typedef
struct actual_argument_info_tag
{
    AST expr;
    type_t* type_info;
} actual_argument_info_t;

#define MAX_ARGUMENTS 128

static void generic_keyword_check(
        const intrinsic_info_t* intrinsic_info, 
        AST argument_list,
        int *variant_num,
        int *num_arguments,
        actual_argument_info_t* argument_info)
{
    // For each variant try to build a feasible argument list
    int i;
    for (i = 0; i < intrinsic_info->num_variants; i++)
    {
        const intrinsic_variant_info_t* current_variant = &intrinsic_info->variants[i];
        *num_arguments = 0;
        *variant_num = i;

        char ok = 1;
        if (argument_list != NULL)
        {
            AST it;
            char seen_keywords = 0;
            int position = 0;
            for_each_element(argument_list, it)
            {
                AST argument = ASTSon1(it);

                AST keyword = ASTSon0(argument);
                AST expr = ASTSon1(argument);

                if (keyword != NULL)
                {
                    char found = 0;
                    int j;
                    for (j = 0; j < current_variant->num_keywords && !found; j++)
                    {
                        if (strcasecmp(current_variant->keyword_names[j], ASTText(keyword)) == 0)
                        {
                            position = j;
                            found = 1;
                        }
                    }

                    if (!found)
                    {
                        // fprintf(stderr, "%s: warning: no keyword '%s' for intrinsic '%s'\n",
                        //         ast_location(keyword),
                        //         ASTText(keyword),
                        //         intrinsic_info->intrinsic_name);
                        ok = 0;
                        break;
                    }
                    seen_keywords = 1;
                }
                else
                {
                    ERROR_CONDITION(seen_keywords, "Invalid argument list", 0);
                    if (position > current_variant->num_keywords)
                    {
                        ok = 0;
                        break;
                    }
                }
                if (argument_info[position].expr == NULL)
                {
                    argument_info[position].expr = expr;
                    argument_info[position].type_info = expression_get_type(expr);
                }
                else
                {
                    // fprintf(stderr, "%s: warning: dummy argument '%d' of intrinsic '%s' already given a value\n",
                    //         ast_location(argument),
                    //         position,
                    //         intrinsic_info->intrinsic_name);
                    ok = 0;
                    break;
                }

                (*num_arguments)++;
                position++;
            }
        }

        // Now check every nonoptional dummy argument has a real argument
        int j;
        for (j = 0; j < current_variant->num_keywords && ok; j++)
        {
            if (argument_info[j].expr == NULL
                    && !current_variant->is_optional[j])
            {
                // fprintf(stderr, "%s: warning: no real argument given for dummy argument '%s' of intrinsic '%s'\n",
                //         ast_location(argument),
                //         current_variant->keyword_names[j],
                //         intrinsic_info->intrinsic_name);
                ok = 0;
                break;
            }
        }

        if (!ok)
            continue;

        // if (ok)
        // This is the variant we want
        return;
    }
    *variant_num = -1;
}

static scope_entry_t* get_intrinsic_symbol(const char* name, type_t* result, int num_args, type_t** types, decl_context_t decl_context)
{
    intrinsic_descr_t descr;
    descr.name = name;
    descr.result = result;
    descr.num_types = num_args;
    descr.parameters = types;

    rb_red_blk_node* n = rb_tree_query(intrinsic_map, &descr);
    if (n != NULL)
    {
        scope_entry_t* entry = (scope_entry_t*)rb_node_get_info(n);
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Returning existing intrinsic '%s' of type '%s'\n",
                    entry->symbol_name,
                    print_type_str(entry->type_information, decl_context));
        }
        return entry;
    }
    else
    {
        // Create a new descriptor
        intrinsic_descr_t *p = calloc(1, sizeof(*p));
        p->name = name;
        p->result = result;
        p->num_types = num_args;
        if (num_args > 0)
        {
            p->parameters = calloc(num_args, sizeof(*p->parameters));
            memcpy(p->parameters, types, num_args * sizeof(*p->parameters));
        }

        parameter_info_t param_info[num_args + 1];
        memset(param_info, 0, sizeof(param_info));
        int i;
        for (i = 0; i < num_args; i++)
        {
            param_info[i].type_info = types[i];
        }
        type_t* function_type = get_new_function_type(result, param_info, num_args);


        scope_entry_t* new_entry = new_symbol(decl_context, decl_context.current_scope, name);
        new_entry->kind = SK_FUNCTION;
        new_entry->do_not_print = 1;
        new_entry->type_information = function_type;

        rb_tree_add(intrinsic_map, p, new_entry);

        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Creating new intrinsic '%s' of type '%s'\n",
                    name,
                    print_type_str(function_type, decl_context));
        }

        return new_entry;
    }
}

static char optional_constant_integer_expr(AST expr)
{
    if (expr == NULL)
        return 1;

    if (is_integer_type(expression_get_type(expr))
            && expression_is_constant(expr))
        return 1;

    return 0;
}

static char optional_integer_expr(AST expr)
{
    if (expr == NULL)
        return 1;

    if (is_integer_type(expression_get_type(expr)))
        return 1;

    return 0;
}

static type_t* get_int_from_kind(AST expr)
{
    if (expr == NULL)
    {
        return get_signed_int_type();
    }
    else
    {
        return choose_int_type_from_kind(expr,
                const_value_cast_to_4(expression_get_constant(expr)));
    }
}

static type_t* get_floating_from_kind(AST expr)
{
    if (expr == NULL)
    {
        return get_float_type();
    }
    else
    {
        return choose_float_type_from_kind(expr, 
                const_value_cast_to_4(expression_get_constant(expr)));
    }
}

#define INTRINSIC_BOILERPLATE(name) \
        actual_argument_info_t argument_info[MAX_ARGUMENTS]; \
        memset(argument_info, 0, sizeof(argument_info)); \
        int variant_num = 0; \
        int num_arguments = 0; \
        generic_keyword_check(&intrinsic_info_##name, argument_list,  \
                &variant_num, &num_arguments, argument_info); \
        if (variant_num < 0) return NULL


static scope_entry_t* check_intrinsic_abs(decl_context_t decl_context,
        char is_actual_argument,
        AST ref UNUSED_PARAMETER, AST argument_list)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(abs);
        type_t* t0 = argument_info[0].type_info;

        if (is_integer_type(t0)
                || is_floating_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("abs", t0, 1, types, decl_context);
        }
        else if (is_complex_type(t0))
        {
            type_t* float_type = complex_type_get_base_type(t0);
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("abs", float_type, 1, types, decl_context);
        }
    }
    else
    {
        type_t* types[1] = { get_float_type() };
        return get_intrinsic_symbol("abs", get_float_type(), 1, types, decl_context);
    }

    return NULL;
}

static scope_entry_t* check_intrinsic_achar(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(achar);
        type_t* t0 = argument_info[0].type_info;

        if (is_integer_type(t0)
                && optional_constant_integer_expr(argument_info[1].expr))
        {
            AST len = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(ref), ASTLine(ref), "1");
            type_t* character_type = get_array_type_bounds(get_char_type(), len, len, decl_context);
            type_t* types[2] = { t0, get_signed_int_type() };
            return get_intrinsic_symbol("achar", character_type, 2, types, decl_context);
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_acos(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(acos);
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t0)
                || is_complex_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("acos", t0, 1, types, decl_context);
        }
    }
    else
    {
            type_t* types[1] = { get_float_type() };
            return get_intrinsic_symbol("acos", get_float_type(), 1, types, decl_context);
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_acosh(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(acosh);
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t0)
                || is_complex_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("acosh", t0, 1, types, decl_context);
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_adjustl(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(adjustl);
        type_t* t0 = argument_info[0].type_info;

        if (is_fortran_character_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("adjustl", t0, 1, types, decl_context);
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_adjustr(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(adjustr);
        type_t* t0 = argument_info[0].type_info;

        if (is_fortran_character_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("adjustr", t0, 1, types, decl_context);
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_aimag(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(aimag);
        type_t* t0 = argument_info[0].type_info;

        if (is_complex_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("aimag", complex_type_get_base_type(t0), 1, types, decl_context);
        }
    }
    else
    {
        type_t* types[1] = { get_complex_type(get_float_type()) };
        return get_intrinsic_symbol("aimag", get_float_type(), 1, types, decl_context);
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_aint(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(aint);
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t0)
                && optional_constant_integer_expr(argument_info[1].expr))
        {
            type_t* types[2] = { t0, get_signed_int_type() };
            return get_intrinsic_symbol("aint", get_int_from_kind(argument_info[1].expr), 2, types, decl_context); 
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_all(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(all);
        type_t* t0 = argument_info[0].type_info;

        if (is_array_type(t0)
                && is_bool_type(get_rank0_type(t0))
                && optional_integer_expr(argument_info[1].expr))
        {
            type_t* types[2] = { t0, get_signed_int_type() };
            return get_intrinsic_symbol("all", array_type_get_element_type(t0), 2, types, decl_context); 
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_allocated(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(allocated);
        type_t* t0 = argument_info[0].type_info;

        type_t* types[1] = { t0 };
        return get_intrinsic_symbol("allocated", get_bool_type(), 1, types, decl_context); 
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_anint(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(anint);
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t0)
                && optional_constant_integer_expr(argument_info[1].expr))
        {
            type_t* types[2] = { t0, get_signed_int_type() };
            return get_intrinsic_symbol("anint", get_int_from_kind(argument_info[1].expr), 2, types, decl_context); 
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_any(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(any);
        type_t* t0 = argument_info[0].type_info;

        if (is_array_type(t0)
                && is_bool_type(get_rank0_type(t0))
                && optional_integer_expr(argument_info[1].expr))
        {
            type_t* types[2] = { t0, get_signed_int_type() };
            return get_intrinsic_symbol("any", array_type_get_element_type(t0), 2, types, decl_context); 
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_asin(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(asin);
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t0)
                || is_complex_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("asin", t0, 1, types, decl_context);
        }
    }
    else
    {
            type_t* types[1] = { get_float_type() };
            return get_intrinsic_symbol("asin", get_float_type(), 1, types, decl_context);
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_asinh(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(asinh);
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t0)
                || is_complex_type(t0))
        {
            type_t* types[1] = { t0 };
            return get_intrinsic_symbol("asinh", t0, 1, types, decl_context);
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_associated(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(associated);
        type_t* t0 = argument_info[0].type_info;

        if (is_pointer_type(t0))
        {
            type_t* types[2] = { t0, t0 };
            return get_intrinsic_symbol("associated", get_bool_type(), 2, types, decl_context);
        }
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_atan(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(atan);

        if (num_arguments == 1)
        {
            type_t* t0 = argument_info[0].type_info;

            if (is_floating_type(t0))
            {
                type_t* types[1] = { t0 };
                return get_intrinsic_symbol("atan", t0, 1, types, decl_context);
            }
        }
        else if (num_arguments == 2)
        {
            type_t* t1 = argument_info[1].type_info;
            type_t* t0 = argument_info[0].type_info;

            if (is_floating_type(t1)
                    && equivalent_types(t0, t1))
            {
                type_t* types[2] = { t0, t1 };
                return get_intrinsic_symbol("atan", t1, 2, types, decl_context);
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else
    {
        type_t* t0 = get_float_type();
        type_t* types[1] = { t0 };
        return get_intrinsic_symbol("atan", t0, 1, types, decl_context);
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_atan2(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    if (!is_actual_argument)
    {
        INTRINSIC_BOILERPLATE(atan2);
        type_t* t1 = argument_info[1].type_info;
        type_t* t0 = argument_info[0].type_info;

        if (is_floating_type(t1)
                && equivalent_types(t0, t1))
        {
            type_t* types[2] = { t0, t1 };
            return get_intrinsic_symbol("atan", t1, 2, types, decl_context);
        }
    }
    else
    {
        type_t* t1 = get_float_type();
        type_t* t0 = get_float_type();

        type_t* types[2] = { t0, t1 };
        return get_intrinsic_symbol("atan", t1, 2, types, decl_context);
    }
    return NULL;
}

static scope_entry_t* check_intrinsic_atanh(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_atomic_define(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_atomic_ref(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bessel_j0(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bessel_j1(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bessel_jn(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bessel_y0(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bessel_y1(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bessel_yn(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bge(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bgt(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ble(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_blt(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_bit_size(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_btest(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ceiling(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_char(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_cmplx(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_command_argument_count(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_conjg(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_cos(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_cosh(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_count(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_cpu_time(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_cshift(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_date_and_time(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_dble(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_digits(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_dim(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_dot_product(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_dprod(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_dshiftl(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_dshiftr(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_eoshift(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_epsilon(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_erf(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_erfc(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_erfc_scaled(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_execute_command_line(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_exp(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_exponent(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_extends_type_of(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_findloc(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_floor(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_fraction(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_gamma(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_get_command(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_get_command_argument(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_get_environment_variable(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_huge(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_hypot(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_iachar(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_iall(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_iand(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_iany(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ibclr(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ibits(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ibset(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ichar(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ieor(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_image_index(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_index(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_int(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ior(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_iparity(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ishft(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ishftc(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_is_contiguous(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_is_iostat_end(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_is_iostat_eor(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_kind(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_lbound(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_lcobound(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_leadz(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_len(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_len_trim(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_lge(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_lgt(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_lle(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_llt(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_log(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_log10(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_log_gamma(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_logical(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_maskl(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_maskr(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_matmul(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_max(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_maxexponent(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_maxloc(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_maxval(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_merge(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_merge_bits(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_min(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_minexponent(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_minloc(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_minval(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_mod(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_modulo(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_move_alloc(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_mvbits(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_nearest(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_new_line(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_nint(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_not(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_norm2(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_null(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_num_images(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_parity(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_pack(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_popcnt(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_poppar(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_precision(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_present(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_product(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_radix(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_random_number(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_random_seed(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_range(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_real(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_repeat(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_reshape(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_rrspacing(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_same_type_as(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_scale(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_scan(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_selected_char_kind(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_selected_int_kind(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_selected_real_kind(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_set_exponent(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_shape(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_shifta(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_shiftl(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_shiftr(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_sign(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_sin(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_sinh(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_size(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_spacing(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_spread(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_sqrt(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_storage_size(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_sum(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_system_clock(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_tan(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_tanh(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_this_image(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_tiny(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_trailz(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_transfer(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_transpose(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_trim(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ubound(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_ucobound(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_unpack(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}

static scope_entry_t* check_intrinsic_verify(decl_context_t decl_context UNUSED_PARAMETER,
        char is_actual_argument UNUSED_PARAMETER,
        AST ref UNUSED_PARAMETER, AST argument_list UNUSED_PARAMETER)
{
    return NULL;
}
