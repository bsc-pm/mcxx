#include "fortran03-codegen.h"
#include "cxx-nodecl-visitor.h"
#include "cxx-driver-decls.h"
#include "cxx-utils.h"
#include <string.h>

typedef
struct nodecl_codegen_visitor_tag
{
    // Base visitor
    nodecl_external_visitor_t _base_visitor;

    // Codegen
    FILE *file;
    int indent_level;
    scope_entry_t* current_sym;
} nodecl_codegen_visitor_t;

typedef void (*codegen_visitor_fun_t)(nodecl_codegen_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

// This is safer than using the macro directly as it will warn us against wrong types
// while the macro does not
static inline nodecl_visitor_fun_t codegen_visitor_fun(codegen_visitor_fun_t p)
{
    return NODECL_VISITOR_FUN(p);
}
static inline void codegen_walk(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    NODECL_WALK(visitor, node);
}

// static void indent(nodecl_codegen_visitor_t* v)
// {
//     int i;
//     for (i = 0; i < v->indent_level; i++)
//     {
//         // FIXME - Make this spacing configurable
//         fprintf(v->file, "  ");
//     }
// }

// Codegen
static void not_implemented_yet(nodecl_external_visitor_t* visitor UNUSED_PARAMETER, nodecl_t node)
{
    internal_error("WARNING -> Uninmplemented node! '%s'\n", ast_print_node_type(ASTType(nodecl_get_ast(node))));
}

void fortran_codegen_translation_unit(FILE* f UNUSED_PARAMETER, AST a UNUSED_PARAMETER, scope_link_t* sl UNUSED_PARAMETER)
{
    nodecl_codegen_visitor_t codegen_visitor;
    memset(&codegen_visitor, 0, sizeof(codegen_visitor));

    if (sl == NULL)
    {
        sl = CURRENT_COMPILED_FILE->scope_link;
    }
    
    nodecl_init_walker((nodecl_external_visitor_t*)&codegen_visitor, not_implemented_yet);

    codegen_visitor.file = f;
    codegen_visitor.indent_level = 0;
    codegen_visitor.current_sym = NULL;

#if 0
    NODECL_VISITOR(&codegen_visitor)->visit_top_level = codegen_visitor_fun(codegen_top_level);
    NODECL_VISITOR(&codegen_visitor)->visit_function_code = codegen_visitor_fun(codegen_function_code);
    NODECL_VISITOR(&codegen_visitor)->visit_compound_statement = codegen_visitor_fun(codegen_compound_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_expression_statement = codegen_visitor_fun(codegen_expression_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_assignment = codegen_visitor_fun(codegen_assignment);
    NODECL_VISITOR(&codegen_visitor)->visit_add = codegen_visitor_fun(codegen_add);
    NODECL_VISITOR(&codegen_visitor)->visit_symbol = codegen_visitor_fun(codegen_symbol);
    NODECL_VISITOR(&codegen_visitor)->visit_integer_literal = codegen_visitor_fun(codegen_integer_literal);
    NODECL_VISITOR(&codegen_visitor)->visit_string_literal = codegen_visitor_fun(codegen_string_literal);
    NODECL_VISITOR(&codegen_visitor)->visit_floating_literal = codegen_visitor_fun(codegen_string_literal);
    NODECL_VISITOR(&codegen_visitor)->visit_object_init = codegen_visitor_fun(codegen_object_init);
    NODECL_VISITOR(&codegen_visitor)->visit_if_else_statement = codegen_visitor_fun(codegen_if_else_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_for_statement = codegen_visitor_fun(codegen_for_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_loop_control = codegen_visitor_fun(codegen_loop_control);
    NODECL_VISITOR(&codegen_visitor)->visit_while_statement = codegen_visitor_fun(codegen_while_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_do_statement = codegen_visitor_fun(codegen_do_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_empty_statement = codegen_visitor_fun(codegen_empty_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_default_statement = codegen_visitor_fun(codegen_default_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_switch_statement = codegen_visitor_fun(codegen_switch_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_labeled_statement = codegen_visitor_fun(codegen_labeled_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_break_statement = codegen_visitor_fun(codegen_break_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_continue_statement = codegen_visitor_fun(codegen_continue_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_case_statement = codegen_visitor_fun(codegen_case_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_return_statement = codegen_visitor_fun(codegen_return_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_goto_statement = codegen_visitor_fun(codegen_goto_statement);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_directive = codegen_visitor_fun(codegen_pragma_custom_directive);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_construct = codegen_visitor_fun(codegen_pragma_custom_construct);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_clause = codegen_visitor_fun(codegen_pragma_custom_clause);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_clause_arg = codegen_visitor_fun(codegen_pragma_clause_arg);
    NODECL_VISITOR(&codegen_visitor)->visit_pragma_custom_line = codegen_visitor_fun(codegen_pragma_custom_line);
    NODECL_VISITOR(&codegen_visitor)->visit_try_block = codegen_visitor_fun(codegen_try_block);
    NODECL_VISITOR(&codegen_visitor)->visit_catch_handler = codegen_visitor_fun(codegen_catch_handler);
    NODECL_VISITOR(&codegen_visitor)->visit_parenthesized_expression = codegen_visitor_fun(codegen_parenthesized_expression);
    NODECL_VISITOR(&codegen_visitor)->visit_new = codegen_visitor_fun(codegen_new);
    NODECL_VISITOR(&codegen_visitor)->visit_delete = codegen_visitor_fun(codegen_delete);
    NODECL_VISITOR(&codegen_visitor)->visit_delete_array = codegen_visitor_fun(codegen_delete_array);
    NODECL_VISITOR(&codegen_visitor)->visit_throw = codegen_visitor_fun(codegen_throw);
    NODECL_VISITOR(&codegen_visitor)->visit_function_call = codegen_visitor_fun(codegen_function_call);
    NODECL_VISITOR(&codegen_visitor)->visit_cast = codegen_visitor_fun(codegen_cast);
    NODECL_VISITOR(&codegen_visitor)->visit_sizeof = codegen_visitor_fun(codegen_sizeof);
    NODECL_VISITOR(&codegen_visitor)->visit_conditional_expression = codegen_visitor_fun(codegen_conditional_expression);
    NODECL_VISITOR(&codegen_visitor)->visit_builtin_decl = codegen_visitor_fun(codegen_builtin);
    NODECL_VISITOR(&codegen_visitor)->visit_builtin_expr = codegen_visitor_fun(codegen_builtin);
    NODECL_VISITOR(&codegen_visitor)->visit_any_list = codegen_visitor_fun(codegen_any_list);
    NODECL_VISITOR(&codegen_visitor)->visit_structured_literal = codegen_visitor_fun(codegen_structured_literal);
    NODECL_VISITOR(&codegen_visitor)->visit_field_designator = codegen_visitor_fun(codegen_field_designator);
    NODECL_VISITOR(&codegen_visitor)->visit_index_designator = codegen_visitor_fun(codegen_index_designator);
    NODECL_VISITOR(&codegen_visitor)->visit_array_subscript = codegen_visitor_fun(codegen_array_subscript);
    // All binary infix, unary prefix and unary postfix are here, look for the definition of OPERATOR_TABLE above
#define PREFIX_UNARY_EXPRESSION(_name, _) \
    NODECL_VISITOR(&codegen_visitor)->visit_##_name = codegen_visitor_fun(codegen_##_name);
#define POSTFIX_UNARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
#define BINARY_EXPRESSION(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
#define BINARY_EXPRESSION_ASSIG(_name, _) PREFIX_UNARY_EXPRESSION(_name, _)
    OPERATOR_TABLE
#undef PREFIX_UNARY_EXPRESSION
#undef POSTFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION
#endif

    codegen_walk(&codegen_visitor, _nodecl_wrap(a));
}

