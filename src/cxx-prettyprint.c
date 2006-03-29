#include "cxx-prettyprint.h"

typedef void (*prettyprint_handler_t)(FILE* f, AST a, int level);

typedef struct {
	char* handler_name;
	prettyprint_handler_t handler;
	char* parameter;
} prettyprint_entry_t;

#define HANDLER_PROTOTYPE(name) \
 static void name(FILE* f, AST a, int level)

#define NODE_HANDLER(type, handler, parameter) \
  [type] = {#handler, handler, parameter}

HANDLER_PROTOTYPE(ambiguity_handler);
HANDLER_PROTOTYPE(sequence_handler);
HANDLER_PROTOTYPE(spaced_sequence_handler);
HANDLER_PROTOTYPE(list_handler);
HANDLER_PROTOTYPE(simple_declaration_handler);
HANDLER_PROTOTYPE(simple_parameter_handler);
HANDLER_PROTOTYPE(indented_simple_parameter_handler);
HANDLER_PROTOTYPE(unary_container_handler);
HANDLER_PROTOTYPE(parenthesized_son_handler);
HANDLER_PROTOTYPE(declarator_id_typename_handler);
HANDLER_PROTOTYPE(nested_name_handler);
HANDLER_PROTOTYPE(simple_text_handler);
HANDLER_PROTOTYPE(template_id_handler);
HANDLER_PROTOTYPE(abstract_array_declarator_handler);
HANDLER_PROTOTYPE(abstract_declarator_function_handler);
HANDLER_PROTOTYPE(null_handler);
HANDLER_PROTOTYPE(parameter_decl_handler);
HANDLER_PROTOTYPE(init_declarator_handler);
HANDLER_PROTOTYPE(pointer_decl_handler);
HANDLER_PROTOTYPE(abstract_declarator_handler);
HANDLER_PROTOTYPE(type_id_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_son_handler);
HANDLER_PROTOTYPE(braced_initializer_handler);
HANDLER_PROTOTYPE(pointer_spec_handler);
HANDLER_PROTOTYPE(decl_specifier_sequence);
HANDLER_PROTOTYPE(binary_operator_handler);
HANDLER_PROTOTYPE(throw_expression_handler);
HANDLER_PROTOTYPE(conditional_expression_handler);
HANDLER_PROTOTYPE(cast_expression_handler);
HANDLER_PROTOTYPE(sizeof_typeid_handler);
HANDLER_PROTOTYPE(new_expression_handler);
HANDLER_PROTOTYPE(new_type_id_handler);
HANDLER_PROTOTYPE(new_initializer_handler);
HANDLER_PROTOTYPE(delete_expression_handler);
HANDLER_PROTOTYPE(array_subscript_handler);
HANDLER_PROTOTYPE(function_call_handler);
HANDLER_PROTOTYPE(function_call_handler);
HANDLER_PROTOTYPE(typename_handler);
HANDLER_PROTOTYPE(typename_template_handler);
HANDLER_PROTOTYPE(infix_parameter_handler);
HANDLER_PROTOTYPE(template_member_access);
HANDLER_PROTOTYPE(son_handler_then_suffix_parameter);
HANDLER_PROTOTYPE(templated_cast_handler);
HANDLER_PROTOTYPE(qualified_id_handler);
HANDLER_PROTOTYPE(qualified_template_handler);
HANDLER_PROTOTYPE(qualified_template_id_handler);
HANDLER_PROTOTYPE(qualified_operator_function_id_handler);
HANDLER_PROTOTYPE(conversion_type_id_handler);
HANDLER_PROTOTYPE(conversion_declarator_handler);
HANDLER_PROTOTYPE(constructor_initializer_handler);
HANDLER_PROTOTYPE(mem_initializer_handler);
HANDLER_PROTOTYPE(mem_initializer_id_handler);
HANDLER_PROTOTYPE(class_specifier_handler);
HANDLER_PROTOTYPE(class_head_handler);
HANDLER_PROTOTYPE(member_specification_handler);
HANDLER_PROTOTYPE(using_declaration_handler);
HANDLER_PROTOTYPE(template_declaration_handler);
HANDLER_PROTOTYPE(type_parameter_class_or_typename_handler);
HANDLER_PROTOTYPE(type_parameter_template_handler);
HANDLER_PROTOTYPE(function_definition_handler);
HANDLER_PROTOTYPE(simple_type_specifier_handler);
HANDLER_PROTOTYPE(compound_statement_handler);
HANDLER_PROTOTYPE(labeled_statement_handler);
HANDLER_PROTOTYPE(default_statement_handler);
HANDLER_PROTOTYPE(case_statement_handler);
HANDLER_PROTOTYPE(expression_statement_handler);
HANDLER_PROTOTYPE(condition_handler);
HANDLER_PROTOTYPE(while_statement_handler);
HANDLER_PROTOTYPE(do_while_statement_handler);
HANDLER_PROTOTYPE(for_statement_handler);
HANDLER_PROTOTYPE(return_statement_handler);
HANDLER_PROTOTYPE(goto_statement_handler);
HANDLER_PROTOTYPE(try_block_handler);
HANDLER_PROTOTYPE(exception_declaration_handler);
HANDLER_PROTOTYPE(member_declaration_handler);
HANDLER_PROTOTYPE(selection_statement_handler);
HANDLER_PROTOTYPE(catch_handler_handler);
HANDLER_PROTOTYPE(member_declarator_handler);
HANDLER_PROTOTYPE(constant_initializer_handler);
HANDLER_PROTOTYPE(base_clause_handler);
HANDLER_PROTOTYPE(base_specifier_handler);
HANDLER_PROTOTYPE(base_specifier_access_handler);
HANDLER_PROTOTYPE(base_specifier_virtual_handler);
HANDLER_PROTOTYPE(base_specifier_access_virtual_handler);
HANDLER_PROTOTYPE(elaborated_type_class_handler);
HANDLER_PROTOTYPE(elaborated_type_template_handler);
HANDLER_PROTOTYPE(elaborated_type_enum_handler);
HANDLER_PROTOTYPE(elaborated_typename_handler);
HANDLER_PROTOTYPE(if_else_statement_handler);
HANDLER_PROTOTYPE(exception_specification_handler);
HANDLER_PROTOTYPE(operator_function_id_handler);
HANDLER_PROTOTYPE(asm_definition_handler);
HANDLER_PROTOTYPE(bitfield_declarator_handler);
HANDLER_PROTOTYPE(enum_specifier_handler);
HANDLER_PROTOTYPE(enum_def_handler);
HANDLER_PROTOTYPE(explicit_instantiation_handler);
HANDLER_PROTOTYPE(explicit_specialization_decl_handler);
HANDLER_PROTOTYPE(explicit_specialization_handler);
HANDLER_PROTOTYPE(linkage_specification_handler);
HANDLER_PROTOTYPE(linkage_specification_decl_handler);
HANDLER_PROTOTYPE(namespace_alias_definition_handler);
HANDLER_PROTOTYPE(qualified_namespace_spec_handler);
HANDLER_PROTOTYPE(using_directive_handler);
HANDLER_PROTOTYPE(namespace_definition_handler);
HANDLER_PROTOTYPE(pseudo_destructor_name_handler);
HANDLER_PROTOTYPE(pseudo_destructor_template_handler);
HANDLER_PROTOTYPE(pseudo_destructor_qualified_handler);

prettyprint_entry_t handlers_list[] =
{
	NODE_HANDLER(AST_AMBIGUITY, ambiguity_handler, NULL),
	NODE_HANDLER(AST_NODE_LIST, sequence_handler, NULL),
	NODE_HANDLER(AST_SIMPLE_DECLARATION, simple_declaration_handler, NULL),
	NODE_HANDLER(AST_EMPTY_DECL, indented_simple_parameter_handler, ";\n"),
	NODE_HANDLER(AST_INIT_DECLARATOR, init_declarator_handler, NULL),
	NODE_HANDLER(AST_DECLARATOR, unary_container_handler, NULL),
	NODE_HANDLER(AST_POINTER_DECL, pointer_decl_handler, NULL),
	NODE_HANDLER(AST_PARENTHESIZED_DECLARATOR, parenthesized_son_handler, NULL),
	NODE_HANDLER(AST_DECLARATOR_ID_EXPR, unary_container_handler, NULL),
	NODE_HANDLER(AST_DECLARATOR_ID_TYPE_NAME, declarator_id_typename_handler, NULL),
	NODE_HANDLER(AST_GLOBAL_SCOPE, simple_parameter_handler, "::"),
	NODE_HANDLER(AST_NESTED_NAME_SPECIFIER, nested_name_handler, NULL),
	NODE_HANDLER(AST_NESTED_NAME_SPECIFIER_TEMPLATE, nested_name_handler, NULL),
	NODE_HANDLER(AST_SYMBOL, simple_text_handler, NULL),
	NODE_HANDLER(AST_TEMPLATE_ID, template_id_handler, NULL),
	NODE_HANDLER(AST_TYPE_ID, type_id_handler, NULL),
	NODE_HANDLER(AST_ABSTRACT_DECLARATOR, abstract_declarator_handler, NULL),
	NODE_HANDLER(AST_PARENTHESIZED_ABSTRACT_DECLARATOR, parenthesized_son_handler, NULL),
	NODE_HANDLER(AST_ABSTRACT_ARRAY, abstract_array_declarator_handler, NULL),
	NODE_HANDLER(AST_ABSTRACT_DECLARATOR_FUNC, abstract_declarator_function_handler, NULL),
	NODE_HANDLER(AST_VARIADIC_ARG, simple_parameter_handler, "..."),
	NODE_HANDLER(AST_EMPTY_PARAMETER_DECLARATION_CLAUSE, null_handler, NULL),
	NODE_HANDLER(AST_PARAMETER_DECL, parameter_decl_handler, NULL),
	NODE_HANDLER(AST_INITIALIZER, prefix_with_parameter_then_son_handler, " = "),
	NODE_HANDLER(AST_PARENTHESIZED_INITIALIZER, parenthesized_son_handler, NULL),
	NODE_HANDLER(AST_INITIALIZER_EXPR, unary_container_handler, NULL),
	NODE_HANDLER(AST_INITIALIZER_BRACES, braced_initializer_handler, NULL),
	NODE_HANDLER(AST_POINTER_DECL, pointer_decl_handler, NULL),
	NODE_HANDLER(AST_POINTER_SPEC, pointer_spec_handler, NULL),
	NODE_HANDLER(AST_REFERENCE_SPEC, simple_parameter_handler, "&"),
	NODE_HANDLER(AST_CONST_SPEC, simple_parameter_handler, "const"),
	NODE_HANDLER(AST_VOLATILE_SPEC, simple_parameter_handler, "volatile"),
	NODE_HANDLER(AST_DECLARATOR_FUNC, abstract_declarator_function_handler, NULL),
	NODE_HANDLER(AST_DECLARATOR_ARRAY, abstract_array_declarator_handler, NULL),
	NODE_HANDLER(AST_DECL_SPECIFIER_SEQ, decl_specifier_sequence, NULL),
	NODE_HANDLER(AST_FRIEND_SPEC, simple_parameter_handler, "friend"),
	NODE_HANDLER(AST_TYPEDEF_SPEC, simple_parameter_handler, "typedef"),
	NODE_HANDLER(AST_SIGNED_TYPE, simple_parameter_handler, "signed"),
	NODE_HANDLER(AST_UNSIGNED_TYPE, simple_parameter_handler, "unsigned"),
	NODE_HANDLER(AST_LONG_TYPE, simple_parameter_handler, "long"),
	NODE_HANDLER(AST_SHORT_TYPE, simple_parameter_handler, "short"),
	NODE_HANDLER(AST_VOID_TYPE, simple_parameter_handler, "void"),
	NODE_HANDLER(AST_CHAR_TYPE, simple_parameter_handler, "char"),
	NODE_HANDLER(AST_WCHAR_TYPE, simple_parameter_handler, "wchar_t"),
	NODE_HANDLER(AST_BOOL_TYPE, simple_parameter_handler, "bool"),
	NODE_HANDLER(AST_INT_TYPE, simple_parameter_handler, "int"),
	NODE_HANDLER(AST_FLOAT_TYPE, simple_parameter_handler, "float"),
	NODE_HANDLER(AST_DOUBLE_TYPE, simple_parameter_handler, "double"),
	NODE_HANDLER(AST_INLINE_SPEC, simple_parameter_handler, "inline"),
	NODE_HANDLER(AST_VIRTUAL_SPEC, simple_parameter_handler, "virtual"),
	NODE_HANDLER(AST_EXPLICIT_SPEC, simple_parameter_handler, "explicit"),
	NODE_HANDLER(AST_DECIMAL_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_OCTAL_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_HEXADECIMAL_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_FLOATING_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_BOOLEAN_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_CHARACTER_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_STRING_LITERAL, simple_text_handler, NULL),
	NODE_HANDLER(AST_CONSTANT_EXPRESSION, unary_container_handler, NULL),
	NODE_HANDLER(AST_COMMA_OP, binary_operator_handler, ","),
	NODE_HANDLER(AST_CONDITIONAL_EXPRESSION, conditional_expression_handler, NULL),
	NODE_HANDLER(AST_ASSIGNMENT, binary_operator_handler, "="),
	NODE_HANDLER(AST_MUL_ASSIGNMENT, binary_operator_handler, "*="),
	NODE_HANDLER(AST_DIV_ASSIGNMENT, binary_operator_handler, "/="),
	NODE_HANDLER(AST_ADD_ASSIGNMENT, binary_operator_handler, "+="),
	NODE_HANDLER(AST_SUB_ASSIGNMENT, binary_operator_handler, "-="),
	NODE_HANDLER(AST_SHL_ASSIGNMENT, binary_operator_handler, "<<="),
	NODE_HANDLER(AST_SHR_ASSIGNMENT, binary_operator_handler, ">>="),
	NODE_HANDLER(AST_AND_ASSIGNMENT, binary_operator_handler, "&="),
	NODE_HANDLER(AST_OR_ASSIGNMENT, binary_operator_handler, "|="),
	NODE_HANDLER(AST_XOR_ASSIGNMENT, binary_operator_handler, "^="),
	NODE_HANDLER(AST_MOD_ASSIGNMENT, binary_operator_handler, "+="),
	NODE_HANDLER(AST_THROW_EXPRESSION, throw_expression_handler, NULL),
	NODE_HANDLER(AST_LOGICAL_OR, binary_operator_handler, "||"),
	NODE_HANDLER(AST_LOGICAL_AND, binary_operator_handler, "&&"),
	NODE_HANDLER(AST_BITWISE_OR, binary_operator_handler, "|"),
	NODE_HANDLER(AST_BITWISE_AND, binary_operator_handler, "&"),
	NODE_HANDLER(AST_BITWISE_XOR, binary_operator_handler, "^"),
	NODE_HANDLER(AST_EQUAL_OP, binary_operator_handler, "=="),
	NODE_HANDLER(AST_DIFFERENT_OP, binary_operator_handler, "!="),
	NODE_HANDLER(AST_LOWER_THAN, binary_operator_handler, "<"),
	NODE_HANDLER(AST_GREATER_THAN, binary_operator_handler, ">"),
	NODE_HANDLER(AST_GREATER_OR_EQUAL_THAN, binary_operator_handler, ">="),
	NODE_HANDLER(AST_LOWER_OR_EQUAL_THAN, binary_operator_handler, "<="),
	NODE_HANDLER(AST_SHL_OP, binary_operator_handler, "<<"),
	NODE_HANDLER(AST_SHR_OP, binary_operator_handler, ">>"),
	NODE_HANDLER(AST_MINUS_OP, binary_operator_handler, "-"),
	NODE_HANDLER(AST_ADD_OP, binary_operator_handler, "+"),
	NODE_HANDLER(AST_DIV_OP, binary_operator_handler, "/"),
	NODE_HANDLER(AST_MOD_OP, binary_operator_handler, "%"),
	NODE_HANDLER(AST_MULT_OP, binary_operator_handler, "*"),
	NODE_HANDLER(AST_POINTER_TO_MEMBER, binary_operator_handler, ".*"),
	NODE_HANDLER(AST_POINTER_TO_POINTER_MEMBER, binary_operator_handler, "->*"),
	NODE_HANDLER(AST_CAST_EXPRESSION, cast_expression_handler, NULL),
	NODE_HANDLER(AST_PREDECREMENT, prefix_with_parameter_then_son_handler, "--"),
	NODE_HANDLER(AST_PREINCREMENT, prefix_with_parameter_then_son_handler, "++"),
	NODE_HANDLER(AST_DERREFERENCE, prefix_with_parameter_then_son_handler, "*"),
	NODE_HANDLER(AST_REFERENCE, prefix_with_parameter_then_son_handler, "&"),
	NODE_HANDLER(AST_PLUS_OP, prefix_with_parameter_then_son_handler, "+"),
	NODE_HANDLER(AST_NEG_OP, prefix_with_parameter_then_son_handler, "-"),
	NODE_HANDLER(AST_NOT_OP, prefix_with_parameter_then_son_handler, "!"),
	NODE_HANDLER(AST_COMPLEMENT_OP, prefix_with_parameter_then_son_handler, "~"),
	NODE_HANDLER(AST_SIZEOF, prefix_with_parameter_then_son_handler, "sizeof "),
	NODE_HANDLER(AST_SIZEOF_TYPEID, sizeof_typeid_handler, "sizeof"),
	NODE_HANDLER(AST_NEW_EXPRESSION, new_expression_handler, NULL),
	NODE_HANDLER(AST_NEW_TYPE_ID, new_type_id_handler, NULL),
	NODE_HANDLER(AST_NEW_DECLARATOR, new_type_id_handler, NULL),
	NODE_HANDLER(AST_DIRECT_NEW_DECLARATOR, abstract_array_declarator_handler, NULL),
	NODE_HANDLER(AST_NEW_INITIALIZER, new_initializer_handler, NULL),
	NODE_HANDLER(AST_DELETE_EXPR, delete_expression_handler, NULL),
	NODE_HANDLER(AST_DELETE_ARRAY_EXPR, delete_expression_handler, NULL),
	NODE_HANDLER(AST_ARRAY_SUBSCRIPT, array_subscript_handler, NULL),
	NODE_HANDLER(AST_FUNCTION_CALL, function_call_handler, NULL),
	NODE_HANDLER(AST_EXPLICIT_TYPE_CONVERSION, function_call_handler, NULL),
	NODE_HANDLER(AST_TYPENAME, typename_handler, NULL),
	NODE_HANDLER(AST_TYPENAME_TEMPLATE, typename_template_handler, NULL),
	NODE_HANDLER(AST_CLASS_MEMBER_ACCESS, infix_parameter_handler, "."),
	NODE_HANDLER(AST_POINTER_CLASS_MEMBER_ACCESS, infix_parameter_handler, "->"),
	NODE_HANDLER(AST_CLASS_TEMPLATE_MEMBER_ACCESS, template_member_access, "."),
	NODE_HANDLER(AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS, template_member_access, "->"),
	NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_CALL, infix_parameter_handler, "."),
	NODE_HANDLER(AST_POINTER_PSEUDO_DESTRUCTOR_CALL, infix_parameter_handler, "->"),
	NODE_HANDLER(AST_POSTINCREMENT, son_handler_then_suffix_parameter, "++"),
	NODE_HANDLER(AST_POSTDECREMENT, son_handler_then_suffix_parameter, "--"),
	NODE_HANDLER(AST_DYNAMIC_CAST, templated_cast_handler, "dynamic_cast"),
	NODE_HANDLER(AST_STATIC_CAST, templated_cast_handler, "static_cast"),
	NODE_HANDLER(AST_REINTERPRET_CAST, templated_cast_handler, "reinterpret_cast"),
	NODE_HANDLER(AST_CONST_CAST, templated_cast_handler, "const_cast"),
	NODE_HANDLER(AST_TYPEID_EXPR, sizeof_typeid_handler, "typeid"),
	NODE_HANDLER(AST_TYPEID, sizeof_typeid_handler, "typeid"),
	NODE_HANDLER(AST_THIS_VARIABLE, simple_parameter_handler, "this"),
	NODE_HANDLER(AST_PARENTHESIZED_EXPRESSION, parenthesized_son_handler, NULL),
	NODE_HANDLER(AST_QUALIFIED_ID, qualified_id_handler, NULL),
	NODE_HANDLER(AST_QUALIFIED_TEMPLATE, qualified_template_handler, NULL),
	NODE_HANDLER(AST_QUALIFIED_TEMPLATE_ID, qualified_template_id_handler, NULL),
	NODE_HANDLER(AST_QUALIFIED_OPERATOR_FUNCTION_ID, qualified_operator_function_id_handler, NULL),
	NODE_HANDLER(AST_DESTRUCTOR_ID, prefix_with_parameter_then_son_handler, "~"),
	NODE_HANDLER(AST_CONVERSION_FUNCTION_ID, prefix_with_parameter_then_son_handler, "operator "),
	NODE_HANDLER(AST_CONVERSION_TYPE_ID, conversion_type_id_handler, NULL),
	NODE_HANDLER(AST_CONVERSION_DECLARATOR, conversion_declarator_handler, NULL),
	NODE_HANDLER(AST_CTOR_INITIALIZER, constructor_initializer_handler, NULL),
	NODE_HANDLER(AST_MEM_INITIALIZER, mem_initializer_handler, NULL),
	NODE_HANDLER(AST_MEM_INITIALIZER_ID, mem_initializer_id_handler, NULL),
	NODE_HANDLER(AST_CLASS_SPECIFIER, class_specifier_handler, NULL),
	NODE_HANDLER(AST_CLASS_KEY_CLASS, simple_parameter_handler, "class"),
	NODE_HANDLER(AST_CLASS_KEY_STRUCT, simple_parameter_handler, "struct"),
	NODE_HANDLER(AST_CLASS_KEY_UNION, simple_parameter_handler, "union"),
	NODE_HANDLER(AST_CLASS_HEAD, class_head_handler, NULL),
	NODE_HANDLER(AST_MEMBER_SPEC, member_specification_handler, NULL),
	NODE_HANDLER(AST_MEMBER_DECLARATION_QUALIFIED, qualified_id_handler, NULL),
	NODE_HANDLER(AST_MEMBER_DECLARATION_TEMPLATE, qualified_template_handler, NULL),
	NODE_HANDLER(AST_MEMBER_DECLARATION, member_declaration_handler, NULL),
	NODE_HANDLER(AST_MEMBER_DECLARATOR, member_declarator_handler, NULL),
	NODE_HANDLER(AST_CONSTANT_INITIALIZER, constant_initializer_handler, NULL),
	NODE_HANDLER(AST_USING_DECL, using_declaration_handler, NULL),
	NODE_HANDLER(AST_USING_DECL_TYPENAME, using_declaration_handler, NULL),
	NODE_HANDLER(AST_TEMPLATE_DECLARATION, template_declaration_handler, NULL),
	NODE_HANDLER(AST_EXPORT_TEMPLATE_DECLARATION, template_declaration_handler, NULL),
	NODE_HANDLER(AST_TYPE_PARAMETER_CLASS, type_parameter_class_or_typename_handler, "class"),
	NODE_HANDLER(AST_TYPE_PARAMETER_TYPENAME, type_parameter_class_or_typename_handler, "typename"),
	NODE_HANDLER(AST_TYPE_PARAMETER_TEMPLATE, type_parameter_template_handler, NULL),
	NODE_HANDLER(AST_SIMPLE_TYPE_SPECIFIER, simple_type_specifier_handler, NULL),
	NODE_HANDLER(AST_FUNCTION_DEFINITION, function_definition_handler, NULL),
	NODE_HANDLER(AST_COMPOUND_STATEMENT, compound_statement_handler, NULL),
	NODE_HANDLER(AST_FUNCTION_BODY, unary_container_handler, NULL),
	NODE_HANDLER(AST_EMPTY_STATEMENT, indented_simple_parameter_handler, ";\n"),
	NODE_HANDLER(AST_LABELED_STATEMENT, labeled_statement_handler, NULL),
	NODE_HANDLER(AST_DEFAULT_STATEMENT, default_statement_handler, NULL),
	NODE_HANDLER(AST_CASE_STATEMENT, case_statement_handler, NULL),
	NODE_HANDLER(AST_EXPRESSION_STATEMENT, expression_statement_handler, NULL),
	NODE_HANDLER(AST_CONDITION, condition_handler, NULL),
	NODE_HANDLER(AST_WHILE_STATEMENT, while_statement_handler, NULL),
	NODE_HANDLER(AST_DO_STATEMENT, do_while_statement_handler, NULL),
	NODE_HANDLER(AST_FOR_STATEMENT, for_statement_handler, NULL),
	NODE_HANDLER(AST_BREAK_STATEMENT, indented_simple_parameter_handler, "break;\n"),
	NODE_HANDLER(AST_CONTINUE_STATEMENT, indented_simple_parameter_handler, "continue;\n"),
	NODE_HANDLER(AST_RETURN_STATEMENT, return_statement_handler, NULL),
	NODE_HANDLER(AST_GOTO_STATEMENT, goto_statement_handler, NULL),
	NODE_HANDLER(AST_DECLARATION_STATEMENT, unary_container_handler, NULL),
	NODE_HANDLER(AST_TRY_BLOCK, try_block_handler, NULL),
	NODE_HANDLER(AST_ANY_EXCEPTION, simple_parameter_handler, "..."),
	NODE_HANDLER(AST_EXCEPTION_DECLARATION, exception_declaration_handler, NULL),
	NODE_HANDLER(AST_SWITCH_STATEMENT, selection_statement_handler, NULL),
	NODE_HANDLER(AST_CATCH_HANDLER, catch_handler_handler, NULL),
	NODE_HANDLER(AST_PRIVATE_SPEC, simple_parameter_handler, "private"),
	NODE_HANDLER(AST_PROTECTED_SPEC, simple_parameter_handler, "protected"),
	NODE_HANDLER(AST_PUBLIC_SPEC, simple_parameter_handler, "public"),
	NODE_HANDLER(AST_BASE_CLAUSE, base_clause_handler, NULL),
	NODE_HANDLER(AST_BASE_SPECIFIER, base_specifier_handler, NULL),
	NODE_HANDLER(AST_BASE_SPECIFIER_ACCESS, base_specifier_access_handler, NULL),
	NODE_HANDLER(AST_BASE_SPECIFIER_VIRTUAL, base_specifier_virtual_handler, NULL),
	NODE_HANDLER(AST_BASE_SPECIFIER_ACCESS_VIRTUAL, base_specifier_access_virtual_handler, NULL),
	NODE_HANDLER(AST_ELABORATED_TYPE_CLASS, elaborated_type_class_handler, NULL),
	NODE_HANDLER(AST_ELABORATED_TYPE_TEMPLATE, elaborated_type_template_handler, NULL),
	NODE_HANDLER(AST_ELABORATED_TYPE_ENUM, elaborated_type_enum_handler, NULL),
	NODE_HANDLER(AST_ELABORATED_TYPENAME, elaborated_typename_handler, NULL),
	NODE_HANDLER(AST_STATIC_SPEC, simple_parameter_handler, "static"),
	NODE_HANDLER(AST_IF_ELSE_STATEMENT, if_else_statement_handler, NULL),
	NODE_HANDLER(AST_EXCEPTION_SPECIFICATION, exception_specification_handler, NULL),
	NODE_HANDLER(AST_OPERATOR_FUNCTION_ID, operator_function_id_handler, NULL),
	NODE_HANDLER(AST_OPERATOR_FUNCTION_ID_TEMPLATE, operator_function_id_handler, NULL),
	NODE_HANDLER(AST_NEW_OPERATOR, simple_parameter_handler, "new"),
	NODE_HANDLER(AST_DELETE_OPERATOR, simple_parameter_handler, "delete"),
	NODE_HANDLER(AST_NEW_ARRAY_OPERATOR, simple_parameter_handler, "new[]"),
	NODE_HANDLER(AST_DELETE_ARRAY_OPERATOR, simple_parameter_handler, "delete[]"),
	NODE_HANDLER(AST_ADD_OPERATOR, simple_parameter_handler, "+"),
	NODE_HANDLER(AST_MINUS_OPERATOR, simple_parameter_handler, "-"),
	NODE_HANDLER(AST_MULT_OPERATOR, simple_parameter_handler, "*"),
	NODE_HANDLER(AST_DIV_OPERATOR, simple_parameter_handler, "/"),
	NODE_HANDLER(AST_MOD_OPERATOR, simple_parameter_handler, "%"),
	NODE_HANDLER(AST_BITWISE_XOR_OPERATOR, simple_parameter_handler, "^"),
	NODE_HANDLER(AST_BITWISE_AND_OPERATOR, simple_parameter_handler, "&"),
	NODE_HANDLER(AST_BITWISE_OR_OPERATOR, simple_parameter_handler, "|"),
	NODE_HANDLER(AST_BITWISE_NEG_OPERATOR, simple_parameter_handler, "~"),
	NODE_HANDLER(AST_ASSIGNMENT_OPERATOR, simple_parameter_handler, "="),
	NODE_HANDLER(AST_LOWER_OPERATOR, simple_parameter_handler, "<"),
	NODE_HANDLER(AST_GREATER_OPERATOR, simple_parameter_handler, ">"),
	NODE_HANDLER(AST_ADD_ASSIGN_OPERATOR, simple_parameter_handler, "+="),
	NODE_HANDLER(AST_SUB_ASSIGN_OPERATOR, simple_parameter_handler, "-="),
	NODE_HANDLER(AST_MUL_ASSIGN_OPERATOR, simple_parameter_handler, "*="),
	NODE_HANDLER(AST_DIV_ASSIGN_OPERATOR, simple_parameter_handler, "/="),
	NODE_HANDLER(AST_MOD_ASSIGN_OPERATOR, simple_parameter_handler, "%="),
	NODE_HANDLER(AST_XOR_ASSIGN_OPERATOR, simple_parameter_handler, "^="),
	NODE_HANDLER(AST_OR_ASSIGN_OPERATOR, simple_parameter_handler,  "|="),
	NODE_HANDLER(AST_AND_ASSIGN_OPERATOR, simple_parameter_handler,  "|="),
	NODE_HANDLER(AST_LEFT_OPERATOR, simple_parameter_handler, "<<"),
	NODE_HANDLER(AST_RIGHT_OPERATOR, simple_parameter_handler, ">>"),
	NODE_HANDLER(AST_LEFT_ASSIGN_OPERATOR, simple_parameter_handler, "<<="),
	NODE_HANDLER(AST_RIGHT_ASSIGN_OPERATOR, simple_parameter_handler, ">>="),
	NODE_HANDLER(AST_EQUAL_OPERATOR, simple_parameter_handler, "=="),
	NODE_HANDLER(AST_DIFFERENT_OPERATOR, simple_parameter_handler, "!="),
	NODE_HANDLER(AST_LESS_OR_EQUAL_OPERATOR, simple_parameter_handler, "<="),
	NODE_HANDLER(AST_GREATER_OR_EQUAL_OPERATOR, simple_parameter_handler, ">="),
	NODE_HANDLER(AST_LOGICAL_AND_OPERATOR, simple_parameter_handler, "&&"),
	NODE_HANDLER(AST_LOGICAL_OR_OPERATOR, simple_parameter_handler, "||"),
	NODE_HANDLER(AST_LOGICAL_NOT_OPERATOR, simple_parameter_handler, "!"),
	NODE_HANDLER(AST_INCREMENT_OPERATOR, simple_parameter_handler, "++"),
	NODE_HANDLER(AST_DECREMENT_OPERATOR, simple_parameter_handler, "--"),
	NODE_HANDLER(AST_COMMA_OPERATOR, simple_parameter_handler, ","),
	NODE_HANDLER(AST_POINTER_OPERATOR, simple_parameter_handler, "->"),
	NODE_HANDLER(AST_POINTER_DERREF_OPERATOR, simple_parameter_handler, "->*"),
	NODE_HANDLER(AST_FUNCTION_CALL_OPERATOR, simple_parameter_handler, "()"),
	NODE_HANDLER(AST_SUBSCRIPT_OPERATOR, simple_parameter_handler, "[]"),
	NODE_HANDLER(AST_ASM_DEFINITION, asm_definition_handler, NULL),
	NODE_HANDLER(AST_AUTO_SPEC, simple_parameter_handler, "auto"),
	NODE_HANDLER(AST_REGISTER_SPEC, simple_parameter_handler, "register"),
	NODE_HANDLER(AST_EXTERN_SPEC, simple_parameter_handler, "extern"),
	NODE_HANDLER(AST_MUTABLE_SPEC, simple_parameter_handler, "mutable"),
	NODE_HANDLER(AST_THREAD_SPEC, simple_parameter_handler, "mutable"),
	NODE_HANDLER(AST_BITFIELD_DECLARATOR, bitfield_declarator_handler, NULL),
	NODE_HANDLER(AST_ENUM_SPECIFIER, enum_specifier_handler, NULL),
	NODE_HANDLER(AST_ENUM_DEF, enum_def_handler, NULL),
	NODE_HANDLER(AST_EXPLICIT_INSTANTIATION, explicit_instantiation_handler, NULL),
	NODE_HANDLER(AST_EXPLICIT_SPECIALIZATION_DECL, explicit_specialization_decl_handler, NULL),
	NODE_HANDLER(AST_EXPLICIT_SPECIALIZATION, explicit_specialization_handler, NULL),
	NODE_HANDLER(AST_LINKAGE_SPEC, linkage_specification_handler, NULL),
	NODE_HANDLER(AST_LINKAGE_SPEC_DECL, linkage_specification_decl_handler, NULL),
	NODE_HANDLER(AST_NAMESPACE_ALIAS, namespace_alias_definition_handler, NULL),
	NODE_HANDLER(AST_QUALIFIED_NAMESPACE_SPEC, qualified_namespace_spec_handler, NULL),
	NODE_HANDLER(AST_USING_DIRECTIVE, using_directive_handler, NULL),
	NODE_HANDLER(AST_NAMESPACE_DEFINITION, namespace_definition_handler, NULL),
	NODE_HANDLER(AST_NEW_PLACEMENT, parenthesized_son_handler, NULL),
	NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_NAME, pseudo_destructor_name_handler, NULL),
	NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_QUALIF, pseudo_destructor_qualified_handler, NULL),
	NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_TEMPLATE, pseudo_destructor_template_handler, NULL),
};

static void prettyprint_level(FILE* f, AST a, int level);

#define HELPER_PARAMETER \
	(handlers_list[ASTType(a)].parameter)

#define HELPER_PARAMETER_STRING \
	((handlers_list[ASTType(a)].parameter != NULL) ? (handlers_list[ASTType(a)].parameter) : "")

void prettyprint(FILE* f, AST a)
{
	prettyprint_level(f, a, 0);
}


static void indent_at_level(FILE* f, int level)
{
	int i;
	// This 4 should be configurable
	for (i = 0; i < 4*level; i++)
	{
		fprintf(f, " ");
	}
}

static void prettyprint_level(FILE* f, AST a, int level)
{
	if (a == NULL)
		return;

	prettyprint_handler_t hnd = handlers_list[ASTType(a)].handler;

	if (hnd == NULL)
	{
		fprintf(stderr, "Node '%s' has NULL handler\n", ast_node_names[ASTType(a)]);
		return;
	}
	else
	{
		// fprintf(stderr, "Calling handler of '%s'\n", ast_node_names[ASTType(a)]);
	}

	(*hnd)(f, a, level);
}

static void ambiguity_handler(FILE* f, AST a, int level)
{
	fprintf(f, "/* Ambiguous node with %d options */ ", a->num_ambig);
	prettyprint_level(f, a->ambig[0], level);
}

static void spaced_sequence_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		spaced_sequence_handler(f, ASTSon0(a), level);
		fprintf(f, " ");
	}

	prettyprint_level(f, ASTSon1(a), level);
}

static void sequence_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		sequence_handler(f, ASTSon0(a), level);
		// fprintf(f, " ");
	}

	prettyprint_level(f, ASTSon1(a), level);
}

static void list_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		list_handler(f, ASTSon0(a), level);
		fprintf(f, ", ");
	}

	prettyprint_level(f, ASTSon1(a), level);
}

static void simple_declaration_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
		if (ASTSon1(a) != NULL)
		{
			fprintf(f, " ");
		}
	}

	if (ASTSon1(a) != NULL)
	{
		list_handler(f, ASTSon1(a), level);
	}
	
	fprintf(f, ";\n");
	fprintf(f, "\n");
}

static void simple_parameter_handler(FILE* f, AST a, int level)
{
	fprintf(f, "%s", HELPER_PARAMETER);
}

static void indented_simple_parameter_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	simple_parameter_handler(f, a, level);
}

static void init_declarator_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void unary_container_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
}

static void parenthesized_son_handler(FILE* f, AST a, int level)
{
	fprintf(f, "(");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ")");
}

static void declarator_id_typename_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
}

static void nested_name_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	fprintf(f, "::");

	if (ASTSon1(a) != NULL)
	{
		if (ASTType(a) == AST_NESTED_NAME_SPECIFIER_TEMPLATE)
		{
			fprintf(f, "template ");
		}

		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void simple_text_handler(FILE* f, AST a, int level)
{
	fprintf(f, "%s", ASTText(a));
}

static void template_id_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	fprintf(f, "<");
	if (ASTSon1(a) != NULL)
	{
		list_handler(f, ASTSon1(a), level);
	}
	fprintf(f, ">");
}

static void type_id_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void abstract_declarator_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void abstract_declarator_function_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	fprintf(f, "(");
	list_handler(f, ASTSon1(a), level);
	fprintf(f, ")");

	if (ASTSon2(a) != NULL)
	{
		fprintf(f, " ");
		spaced_sequence_handler(f, ASTSon2(a), level);
	}

	if (ASTSon3(a) != NULL)
	{
		fprintf(f, " ");
		prettyprint_level(f, ASTSon3(a), level);
	}
}

static void abstract_array_declarator_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	fprintf(f, "[");
	prettyprint_level(f, ASTSon1(a), level);
	fprintf(f, "]");
}

static void null_handler(FILE* f, AST a, int level) { }

static void parameter_decl_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
	
	if (ASTSon2(a) != NULL)
	{
		fprintf(f, " = ");
		prettyprint_level(f, ASTSon2(a), level);
	}
}

static void prefix_with_parameter_then_son_handler(FILE* f, AST a, int level)
{
	fprintf(f, "%s", HELPER_PARAMETER_STRING);
	prettyprint_level(f, ASTSon0(a), level);
}

static void braced_initializer_handler(FILE* f, AST a, int level)
{
	fprintf(f, "{");
	if (ASTSon0(a) != NULL)
	{
		list_handler(f, ASTSon0(a), level);
	}
	fprintf(f, "}");
}

static void pointer_decl_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	prettyprint_level(f, ASTSon1(a), level);
}

static void pointer_spec_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
		fprintf(f, " ");
	}
	
	fprintf(f, "*");
	
	if (ASTSon2(a) != NULL)
	{
		spaced_sequence_handler(f, ASTSon2(a), level);
	}
}

static void decl_specifier_sequence(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		spaced_sequence_handler(f, ASTSon0(a), level);

		if (ASTSon1(a) != NULL || ASTSon2(a) != NULL)
		{
			fprintf(f, " ");
		}
	}


	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);

		if (ASTSon2(a) != NULL)
		{
			fprintf(f, " ");
		}
	}

	if (ASTSon2(a) != NULL)
	{
		spaced_sequence_handler(f, ASTSon2(a), level);
	}
}

static void binary_operator_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " %s ", HELPER_PARAMETER_STRING);
	prettyprint_level(f, ASTSon1(a), level);
}

static void throw_expression_handler(FILE* f, AST a, int level)
{
	fprintf(f, "throw");

	if (ASTSon0(a) != NULL)
	{
		fprintf(f, " ");
		prettyprint_level(f, ASTSon0(a), level);
	}
}

static void conditional_expression_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " ? ");
	prettyprint_level(f, ASTSon1(a), level);
	fprintf(f, " : ");
	prettyprint_level(f, ASTSon2(a), level);
}

static void cast_expression_handler(FILE* f, AST a, int level)
{
	fprintf(f, "(");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ") ");
	prettyprint_level(f, ASTSon1(a), level);
}

static void sizeof_typeid_handler(FILE* f, AST a, int level)
{
	fprintf(f, "%s(", HELPER_PARAMETER_STRING);
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ")");
}

static void new_expression_handler(FILE* f, AST a, int level)
{
	fprintf(f, "new ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
		fprintf(f, " ");
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
		fprintf(f, " ");
	}

	prettyprint_level(f, ASTSon2(a), level);

	if (ASTSon3(a) != NULL)
	{
		fprintf(f, " ");
		prettyprint_level(f, ASTSon3(a), level);
	}
}

static void new_type_id_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	
	if (ASTSon1(a) != NULL)
	{
		fprintf(f, " ");
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void new_initializer_handler(FILE* f, AST a, int level)
{
	fprintf(f, "(");
	list_handler(f, ASTSon0(a), level);
	fprintf(f, ")");
}

static void delete_expression_handler(FILE* f, AST a, int level)
{
	if(ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}
	
	fprintf(f, "delete");
	if (ASTType(a) == AST_DELETE_ARRAY_EXPR)
	{
		fprintf(f, "[]");
	}
	fprintf(f, " ");

	prettyprint_level(f, ASTSon1(a), level);
}

static void array_subscript_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "[");
	list_handler(f, ASTSon1(a), level);
	fprintf(f, "]");
}

static void function_call_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "(");
	if (ASTSon1(a) != NULL)
	{
		list_handler(f, ASTSon1(a), level);
	}
	fprintf(f, ")");
}

static void typename_handler(FILE* f, AST a, int level)
{
	fprintf(f, "typename ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);

	fprintf(f, "(");
	if (ASTSon3(a) != NULL)
	{
		list_handler(f, ASTSon3(a), level);
	}
	fprintf(f, ")");
}

static void typename_template_handler(FILE* f, AST a, int level)
{
	fprintf(f, "typename ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	prettyprint_level(f, ASTSon1(a), level);

	fprintf(f, "template ");
	prettyprint_level(f, ASTSon2(a), level);

	fprintf(f, "(");
	if (ASTSon3(a) != NULL)
	{
		list_handler(f, ASTSon3(a), level);
	}
	fprintf(f, ")");
}

static void infix_parameter_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "%s", HELPER_PARAMETER_STRING);
	prettyprint_level(f, ASTSon1(a), level);
}

static void template_member_access(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "%stemplate ", HELPER_PARAMETER_STRING);
	prettyprint_level(f, ASTSon1(a), level);
}

static void son_handler_then_suffix_parameter(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "%s", HELPER_PARAMETER_STRING);
}

static void templated_cast_handler(FILE* f, AST a, int level)
{
	fprintf(f, "%s<", HELPER_PARAMETER_STRING);
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ">");
	fprintf(f, "(");
	prettyprint_level(f, ASTSon1(a), level);
	fprintf(f, ")");
}

static void qualified_id_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	prettyprint_level(f, ASTSon1(a), level);
	prettyprint_level(f, ASTSon2(a), level);
}

static void qualified_template_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	prettyprint_level(f, ASTSon1(a), level);
	fprintf(f, "template ");
	prettyprint_level(f, ASTSon2(a), level);
}

static void qualified_operator_function_id_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	prettyprint_level(f, ASTSon1(a), level);
}

static void qualified_template_id_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	prettyprint_level(f, ASTSon1(a), level);
}

static void conversion_type_id_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	
	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void conversion_declarator_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	
	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void constructor_initializer_handler(FILE* f, AST a, int level)
{
	fprintf(f, ": ");
	list_handler(f, ASTSon0(a), level);
}

static void mem_initializer_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "(");
	list_handler(f, ASTSon1(a), level);
	fprintf(f, ")");
}

static void mem_initializer_id_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
}

static void class_specifier_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "\n");
	indent_at_level(f, level);
	fprintf(f, "{\n");
	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level + 2);
	}
	fprintf(f, "}");
}

static void class_head_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	fprintf(f, " ");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	if (ASTSon3(a) != NULL)
	{
		prettyprint_level(f, ASTSon3(a), level);
	}
}

static void member_specification_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		indent_at_level(f, level > 0 ? level - 1 : 0);
		prettyprint_level(f, ASTSon0(a), level);
		fprintf(f, ":\n");
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}
}

static void using_declaration_handler(FILE* f, AST a, int level)
{
	fprintf(f, "using ");

	if (ASTType(a) == AST_USING_DECL_TYPENAME)
	{
		fprintf(f, "typename ");
	}
	
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);

	fprintf(f, ";\n");
}

static void template_declaration_handler(FILE* f, AST a, int level)
{
	if (ASTType(a) == AST_EXPORT_TEMPLATE_DECLARATION)
	{
		fprintf(f, "export ");
	}
	fprintf(f, "template<");
	list_handler(f, ASTSon0(a), level);
	fprintf(f, "> ");
	prettyprint_level(f, ASTSon1(a), level);
}

static void type_parameter_class_or_typename_handler(FILE* f, AST a, int level)
{
	fprintf(f, "%s ", HELPER_PARAMETER_STRING);
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		fprintf(f, "= ");
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void type_parameter_template_handler(FILE* f, AST a, int level)
{
	fprintf(f, "template<");
	list_handler(f, ASTSon0(a), level);
	fprintf(f, "> class");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
	if (ASTSon2(a) != NULL)
	{
		fprintf(f, " = ");
		prettyprint_level(f, ASTSon2(a), level);
	}
}

static void simple_type_specifier_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}
	else if (ASTSon3(a) != NULL)
	{
		fprintf(f, "template ");
		prettyprint_level(f, ASTSon3(a), level);
	}
}

static void member_declaration_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
	fprintf(f, ";\n");
}

static void member_declarator_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		fprintf(f, " ");
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void constant_initializer_handler(FILE* f, AST a, int level)
{
	fprintf(f, "= ");
	prettyprint_level(f, ASTSon0(a), level);
}

static void function_definition_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	prettyprint_level(f, ASTSon1(a), level);

	if (ASTSon2(a) != NULL)
	{
		fprintf(f, "\n");
		indent_at_level(f, level+1);
		prettyprint_level(f, ASTSon2(a), level+1);
		fprintf(f, " ");
	}

	prettyprint_level(f, ASTSon3(a), level);
	fprintf(f, "\n");
}

static void compound_statement_handler(FILE* f, AST a, int level)
{
	fprintf(f, "\n");
	indent_at_level(f, level);
	fprintf(f, "{\n");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level + 1);
	}
	indent_at_level(f, level);
	fprintf(f, "}\n");
}

static void labeled_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " : ");
	// Newline will be done here
	prettyprint_level(f, ASTSon1(a), level);
}

static void case_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "case ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " : ");
	// Newline will be done here
	prettyprint_level(f, ASTSon1(a), level);
}

static void default_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "default : ");
	// Newline will be done here
	prettyprint_level(f, ASTSon0(a), level);
}

static void expression_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ";\n");
}

static void selection_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "switch (");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ")");
	prettyprint_level(f, ASTSon1(a), level);
}

static void condition_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}
	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
		fprintf(f, " = ");
	}

	prettyprint_level(f, ASTSon2(a), level);
}

static void while_statement_handler(FILE* f, AST a, int level)
{
	fprintf(f, "while (");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ")");
	prettyprint_level(f, ASTSon1(a), level);
}

static void do_while_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "do ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, "while (");
	prettyprint_level(f, ASTSon1(a), level);
	fprintf(f, ");\n");
}

static void for_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "for (");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
	
	fprintf(f, ";");

	if (ASTSon3(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	fprintf(f, ")");

	prettyprint_level(f, ASTSon3(a), level);
}

static void return_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "return");

	if (ASTSon0(a) != NULL)
	{
		fprintf(f, " ");
		prettyprint_level(f, ASTSon0(a), level);
	}

	fprintf(f, ";\n");
}

static void goto_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);

	fprintf(f, "goto ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ";\n");
}

static void try_block_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);

	fprintf(f, "try ");
	prettyprint_level(f, ASTSon0(a), level);
	prettyprint_level(f, ASTSon1(a), level);
}

static void catch_handler_handler(FILE* f, AST a, int level)
{
	fprintf(f, "catch ( ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ") ");
	prettyprint_level(f, ASTSon1(a), level);
}

static void exception_declaration_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void base_clause_handler(FILE* f, AST a, int level)
{
	fprintf(f, " : ");
	list_handler(f, ASTSon0(a), level);
}

static void base_specifier_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
}

static void base_specifier_access_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " ");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	prettyprint_level(f, ASTSon3(a), level);
}

static void base_specifier_virtual_handler(FILE* f, AST a, int level)
{
	fprintf(f, "virtual ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
		fprintf(f, " ");
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	prettyprint_level(f, ASTSon3(a), level);
}

static void base_specifier_access_virtual_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " ");

	fprintf(f, "virtual ");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_class_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " ");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_template_handler(FILE* f, AST a, int level)
{
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " ");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	fprintf(f, "template ");
	prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_enum_handler(FILE* f, AST a, int level)
{
	fprintf(f, "enum ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
}

static void elaborated_typename_handler(FILE* f, AST a, int level)
{
	fprintf(f, "typename ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}
	else if (ASTSon3(a) != NULL)
	{
		fprintf(f, "template ");
		prettyprint_level(f, ASTSon3(a), level);
	}
}

static void if_else_statement_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "if (");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ")");
	prettyprint_level(f, ASTSon1(a), level);
	if (ASTSon2(a) != NULL)
	{
		indent_at_level(f, level);
		fprintf(f, "else");
		prettyprint_level(f, ASTSon2(a), level);
	}
}

static void exception_specification_handler(FILE* f, AST a, int level)
{
	fprintf(f, "throw (");
	if (ASTSon0(a) != NULL)
	{
		list_handler(f, ASTSon0(a), level);
	}
	fprintf(f, ")");
}

static void operator_function_id_handler(FILE* f, AST a, int level)
{
	fprintf(f, "operator ");
	prettyprint_level(f, ASTSon0(a), level);
	
	if (ASTType(a) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
	{
		fprintf(f, "<");
		prettyprint_level(f, ASTSon1(a), level);
		fprintf(f, ">");
	}
}

static void asm_definition_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "asm (");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ");\n");
}

static void bitfield_declarator_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
		fprintf(f, " ");
	}

	fprintf(f, ": ");

	prettyprint_level(f, ASTSon1(a), level);
}

static void enum_specifier_handler(FILE* f, AST a, int level)
{
	fprintf(f, "enum ");
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}
	fprintf(f, "\n");
	indent_at_level(f, level);
	fprintf(f, "{");

	if (ASTSon1(a) != NULL)
	{
		list_handler(f, ASTSon1(a), level+1);
	}
	fprintf(f, "\n");
	indent_at_level(f, level);
	fprintf(f, "}");
}

static void enum_def_handler(FILE* f, AST a, int level)
{
	fprintf(f, "\n");
	indent_at_level(f, level);
	prettyprint_level(f, ASTSon0(a), level);

	if (ASTSon1(a) != NULL)
	{
		fprintf(f, " = ");
		prettyprint_level(f, ASTSon1(a), level);
	}
}

static void explicit_instantiation_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "template ");
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
		if (ASTSon1(a) != NULL)
		{
			fprintf(f, " ");
		}
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	fprintf(f, ";\n");
}

static void explicit_specialization_decl_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "template<> ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
		if (ASTSon1(a) != NULL)
		{
			fprintf(f, " ");
		}
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	fprintf(f, ";\n");
}

static void explicit_specialization_handler(FILE* f, AST a, int level)
{
	fprintf(f, "template<> ");

	prettyprint_level(f, ASTSon0(a), level);
}

static void linkage_specification_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "extern ");
	
	prettyprint_level(f, ASTSon0(a), level);
	
	fprintf(f, "\n");
	indent_at_level(f, level);
	fprintf(f, "{\n");

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level+1);
	}
	indent_at_level(f, level);
	fprintf(f, "}\n");
}

static void linkage_specification_decl_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "extern ");
	prettyprint_level(f, ASTSon0(a), level);

	fprintf(f, " ");
	prettyprint_level(f, ASTSon1(a), level);

	fprintf(f, "\n");
}

static void namespace_alias_definition_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "namespace ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " = ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, ";\n");
}

static void qualified_namespace_spec_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
}

static void using_directive_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "using namespace ");

	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
	fprintf(f, ";\n");
}

static void namespace_definition_handler(FILE* f, AST a, int level)
{
	indent_at_level(f, level);
	fprintf(f, "namespace ");
	prettyprint_level(f, ASTSon0(a), level);
	fprintf(f, " {\n");

	prettyprint_level(f, ASTSon1(a), level+1);
	
	indent_at_level(f, level);
	fprintf(f, "}");
}

static void pseudo_destructor_name_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}
	
	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}
	
	fprintf(f, "~");
	prettyprint_level(f, ASTSon2(a), level);
}

static void pseudo_destructor_qualified_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}

	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	prettyprint_level(f, ASTSon2(a), level);
	fprintf(f, "::~");
	prettyprint_level(f, ASTSon3(a), level);
}

static void pseudo_destructor_template_handler(FILE* f, AST a, int level)
{
	if (ASTSon0(a) != NULL)
	{
		prettyprint_level(f, ASTSon0(a), level);
	}
	if (ASTSon1(a) != NULL)
	{
		prettyprint_level(f, ASTSon1(a), level);
	}

	fprintf(f, " template");

	if (ASTSon2(a) != NULL)
	{
		prettyprint_level(f, ASTSon2(a), level);
	}

	fprintf(f, "::~");

	if (ASTSon3(a) != NULL)
	{
		prettyprint_level(f, ASTSon3(a), level);
	}
}
