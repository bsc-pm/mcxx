%{
/*
   Parser of ISO/IEC 14882:2003 - C++

   It parses a superset of the language.

   Must be compiled with rofi-bison-2.3
   Ask for it at <rferrer@ac.upc.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "cxx-lexer.h"
#include "cxx-ast.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
// Sometimes we need lots of memory
#define YYMAXDEPTH (10000000)

void yyerror(AST* parsed_tree, const char* c);

// #define yylex mcxxlex
// #define yytext mcxxtext
extern int yylex(void);

%}

%glr-parser

%union {
	token_atrib_t token_atrib;
	AST ast;
	node_t node_type;
};


// This is a specific feature of rofi-bison 2.1
%default-merge <ambiguityHandler>

%parse-param {AST* parsed_tree}

%{
static AST ambiguityHandler (YYSTYPE x0, YYSTYPE x1);
%}

// C++ tokens
%token<token_atrib> ADD_ASSIGN
%token<token_atrib> ANDAND
%token<token_atrib> AND_ASSIGN
%token<token_atrib> ASM
%token<token_atrib> AUTO
%token<token_atrib> BOOL
%token<token_atrib> BOOLEAN_LITERAL
%token<token_atrib> BREAK
%token<token_atrib> CASE
%token<token_atrib> CATCH
%token<token_atrib> CHAR
%token<token_atrib> CHARACTER_LITERAL
%token<token_atrib> CLASS
%token<token_atrib> CONST
%token<token_atrib> CONST_CAST
%token<token_atrib> CONTINUE
%token<token_atrib> DECIMAL_LITERAL
%token<token_atrib> DEFAULT
%token<token_atrib> DELETE
%token<token_atrib> DIV_ASSIGN
%token<token_atrib> DO
%token<token_atrib> DOS_DOS_PUNTS
%token<token_atrib> DOUBLE
%token<token_atrib> DYNAMIC_CAST
%token<token_atrib> ELSE
%token<token_atrib> ENUM
%token<token_atrib> EQUAL
%token<token_atrib> EXPLICIT
%token<token_atrib> EXPORT
%token<token_atrib> EXTERN
%token<token_atrib> FLOAT
%token<token_atrib> FLOATING_LITERAL
%token<token_atrib> HEXADECIMAL_FLOAT
%token<token_atrib> FOR
%token<token_atrib> FRIEND
%token<token_atrib> GOTO
%token<token_atrib> GREATER_OR_EQUAL
%token<token_atrib> HEXADECIMAL_LITERAL
%token<token_atrib> IDENTIFIER
%token<token_atrib> IF
%token<token_atrib> INLINE
%token<token_atrib> INT
%token<token_atrib> LEFT
%token<token_atrib> LEFT_ASSIGN
%token<token_atrib> LESS_OR_EQUAL
%token<token_atrib> LONG
%token<token_atrib> MINUSMINUS
%token<token_atrib> MOD_ASSIGN
%token<token_atrib> MUL_ASSIGN
%token<token_atrib> MUTABLE
%token<token_atrib> NAMESPACE
%token<token_atrib> TOKEN_NEW
%token<token_atrib> NOT_EQUAL
%token<token_atrib> OCTAL_LITERAL
%token<token_atrib> OPERATOR
%token<token_atrib> OR_ASSIGN
%token<token_atrib> OROR
%token<token_atrib> PLUSPLUS
%token<token_atrib> PRIVATE
%token<token_atrib> PROTECTED
%token<token_atrib> PTR_OP
%token<token_atrib> PTR_OP_MUL
%token<token_atrib> PUBLIC
%token<token_atrib> REGISTER
%token<token_atrib> REINTERPRET_CAST
%token<token_atrib> RETURN
%token<token_atrib> RIGHT
%token<token_atrib> RIGHT_ASSIGN
%token<token_atrib> SHORT
%token<token_atrib> SIGNED
%token<token_atrib> SIZEOF
%token<token_atrib> STATIC
%token<token_atrib> STATIC_CAST
%token<token_atrib> STRING_LITERAL
%token<token_atrib> STRUCT
%token<token_atrib> SUB_ASSIGN
%token<token_atrib> SWITCH
%token<token_atrib> TEMPLATE
%token<token_atrib> THIS
%token<token_atrib> THROW
%token<token_atrib> TRES_PUNTS
%token<token_atrib> TRY
%token<token_atrib> TYPEDEF
%token<token_atrib> TYPEID
%token<token_atrib> TYPENAME
%token<token_atrib> UNION
%token<token_atrib> UNSIGNED
%token<token_atrib> USING
%token<token_atrib> VIRTUAL
%token<token_atrib> VOID
%token<token_atrib> VOLATILE
%token<token_atrib> WCHAR_T
%token<token_atrib> WHILE
%token<token_atrib> XOR_ASSIGN
%token<token_atrib> UNKNOWN_PRAGMA

// OpenMP 2.5 tokens
%token<token_atrib> OMP_ATOMIC
%token<token_atrib> OMP_BARRIER
%token<token_atrib> OMP_COPYIN
%token<token_atrib> OMP_COPYPRIVATE
%token<token_atrib> OMP_CRITICAL
%token<token_atrib> OMP_DEFAULT
%token<token_atrib> OMP_DYNAMIC
%token<token_atrib> OMP_FIRSTPRIVATE
%token<token_atrib> OMP_FLUSH
%token<token_atrib> OMP_FOR
%token<token_atrib> OMP_GUIDED
%token<token_atrib> OMP_IF
%token<token_atrib> OMP_LASTPRIVATE
%token<token_atrib> OMP_MASTER
%token<token_atrib> OMP_NEWLINE
%token<token_atrib> OMP_NONE
%token<token_atrib> OMP_NOWAIT
%token<token_atrib> OMP_NUM_THREADS
%token<token_atrib> OMP_ORDERED
%token<token_atrib> OMP_PARALLEL
%token<token_atrib> OMP_PARALLEL_FOR
%token<token_atrib> OMP_PARALLEL_SECTIONS
%token<token_atrib> OMP_PRAGMA
%token<token_atrib> OMP_PRIVATE
%token<token_atrib> OMP_REDUCTION
%token<token_atrib> OMP_RUNTIME
%token<token_atrib> OMP_SCHEDULE
%token<token_atrib> OMP_SECTION
%token<token_atrib> OMP_SECTIONS
%token<token_atrib> OMP_SHARED
%token<token_atrib> OMP_SINGLE
%token<token_atrib> OMP_STATIC
%token<token_atrib> OMP_THREADPRIVATE
%token<token_atrib> OMP_CUSTOM_CLAUSE
%token<token_atrib> OMP_CUSTOM_DIRECTIVE
%token<token_atrib> OMP_CONSTRUCT_TOKEN
%token<token_atrib> OMP_DIRECTIVE_TOKEN

// Lexical symbols
%token<token_atrib> '!'
%token<token_atrib> '%'
%token<token_atrib> '&'
%token<token_atrib> '('
%token<token_atrib> ')'
%token<token_atrib> '*'
%token<token_atrib> '+'
%token<token_atrib> ','
%token<token_atrib> '-'
%token<token_atrib> '.'
%token<token_atrib> '/'
%token<token_atrib> ':'
%token<token_atrib> ';'
%token<token_atrib> '<'
%token<token_atrib> '='
%token<token_atrib> '>'
%token<token_atrib> '?'
%token<token_atrib> '['
%token<token_atrib> ']'
%token<token_atrib> '^'
%token<token_atrib> '{'
%token<token_atrib> '|'
%token<token_atrib> '}'
%token<token_atrib> '~'

// GNU Extensions
%token<token_atrib> BUILTIN_VA_ARG
%token<token_atrib> ALIGNOF
%token<token_atrib> EXTENSION
%token<token_atrib> REAL
%token<token_atrib> IMAG
%token<token_atrib> LABEL
%token<token_atrib> COMPLEX
%token<token_atrib> TYPEOF
%token<token_atrib> RESTRICT
%token<token_atrib> ATTRIBUTE
%token<token_atrib> THREAD
%token<token_atrib> MAX_OPERATOR
%token<token_atrib> MIN_OPERATOR
%token<token_atrib> MAX_OPERATOR_ASSIGN
%token<token_atrib> MIN_OPERATOR_ASSIGN

// Subparsing
%token<token_atrib> SUBPARSE_EXPRESSION
%token<token_atrib> SUBPARSE_STATEMENT
%token<token_atrib> SUBPARSE_MEMBER

// Nonterminals
%type<ast> abstract_declarator
%type<ast> access_specifier
%type<ast> additive_expression
%type<ast> and_expression
%type<ast> asm_definition
%type<ast> asm_operand
%type<ast> asm_operand_list
%type<ast> asm_operand_list_nonempty
%type<ast> asm_specification
%type<ast> assignment_expression
%type<ast> attribute
%type<ast> attribute_list
%type<ast> attributes
%type<ast> attribute_value
%type<ast> base_clause
%type<ast> base_specifier
%type<ast> base_specifier_list
%type<ast> block_declaration
%type<ast> builtin_types
%type<ast> cast_expression
%type<ast> class_head
%type<ast> class_key
%type<ast> class_or_namespace_name
%type<ast> class_specifier
%type<ast> compound_statement
%type<ast> condition
%type<ast> conditional_expression
%type<ast> constant_expression
%type<ast> constant_initializer
%type<ast> conversion_declarator
%type<ast> conversion_function_id
%type<ast> conversion_type_id
%type<ast> ctor_initializer
%type<ast> cv_qualifier
%type<ast> cv_qualifier_seq
%type<ast> declaration
%type<ast> declaration_sequence
%type<ast> declaration_statement
%type<ast> declarator
%type<ast> functional_declarator
%type<ast> declarator_id
%type<ast> functional_declarator_id
// %type<ast> decl_specifier
%type<ast> decl_specifier_seq
%type<ast> delete_expression
%type<ast> direct_abstract_declarator
%type<ast> direct_declarator
%type<ast> functional_direct_declarator
%type<ast> direct_new_declarator
%type<ast> elaborated_type_specifier
%type<ast> enumeration_definition
%type<ast> enumeration_list
%type<ast> enumeration_list_proper
%type<ast> enum_specifier
%type<ast> equality_expression
%type<ast> exception_declaration
%type<ast> exception_specification
%type<ast> exclusive_or_expression
%type<ast> explicit_instantiation
%type<ast> explicit_specialization
%type<ast> expression
%type<ast> expression_list
%type<ast> expression_statement
%type<ast> for_init_statement
%type<ast> function_body
%type<ast> function_definition
%type<ast> function_specifier
%type<ast> handler
%type<ast> handler_seq
%type<ast> id_expression
%type<ast> if_else_eligible_statements
%type<ast> if_else_statement
%type<ast> if_statement
%type<ast> inclusive_or_expression
%type<ast> init_declarator
%type<ast> init_declarator_list
%type<ast> initializer
%type<ast> initializer_clause
%type<ast> initializer_list
%type<ast> iteration_statement
%type<ast> jump_statement
%type<ast> label_declaration
%type<ast> label_declarator_seq
%type<ast> labeled_statement
%type<ast> linkage_specification
%type<ast> literal
%type<ast> logical_and_expression
%type<ast> logical_or_expression
%type<ast> member_declaration
%type<ast> member_declarator
%type<ast> member_declarator_list
%type<ast> member_specification
%type<ast> mem_initializer
%type<ast> mem_initializer_id
%type<ast> mem_initializer_list
%type<ast> multiplicative_expression
%type<ast> named_namespace_definition
%type<ast> namespace_alias_definition
%type<ast> namespace_definition
%type<ast> nested_name_specifier
%type<ast> new_declarator
%type<ast> new_expression
%type<ast> new_initializer
%type<ast> new_placement
%type<ast> new_type_id
%type<ast> no_if_statement
%type<ast> operator
%type<ast> operator_function_id
%type<ast> parameter_declaration
%type<ast> parameter_declaration_clause
%type<ast> parameter_declaration_clause_nonempty
%type<ast> parameter_declaration_list
%type<ast> pm_expression
%type<ast> postfix_expression
%type<ast> primary_expression
%type<ast> pseudo_destructor_name
%type<ast> ptr_operator
%type<ast> qualified_id
%type<ast> qualified_namespace_specifier
%type<ast> relational_expression
%type<ast> selection_statement
%type<ast> shift_expression
%type<ast> simple_declaration
%type<ast> simple_type_specifier
%type<ast> statement
%type<ast> statement_seq
%type<ast> storage_class_specifier
%type<ast> string_literal
%type<ast> template_and_expression
%type<ast> template_argument
%type<ast> template_argument_list
%type<ast> template_assignment_expression
%type<ast> template_conditional_expression
%type<ast> template_declaration
%type<ast> templated_declaration
%type<ast> template_equality_expression
%type<ast> template_exclusive_or_expression
%type<ast> template_expression
%type<ast> template_id
%type<ast> template_inclusive_or_expression
%type<ast> template_logical_and_expression
%type<ast> template_logical_or_expression
%type<ast> template_parameter
%type<ast> template_parameter_list
%type<ast> template_relational_expression
%type<ast> template_shift_expression
%type<ast> throw_expression
%type<ast> translation_unit
%type<ast> try_block
%type<ast> type_id
%type<ast> type_id_list
%type<ast> type_name
%type<ast> type_parameter
%type<ast> type_specifier
%type<ast> type_specifier_seq
%type<ast> unary_expression
%type<ast> unnamed_namespace_definition
%type<ast> unqualified_id
%type<ast> using_declaration
%type<ast> using_directive
%type<ast> nontype_specifier_seq
%type<ast> nontype_specifier
%type<ast> nontype_specifier_seq2
%type<ast> nontype_specifier2
%type<ast> volatile_optional
%type<ast> unknown_pragma
%type<ast> designation
%type<ast> designator_list
%type<ast> designator

%type<node_type> unary_operator
%type<node_type> assignment_operator

// OpenMP 2.5
%type<ast> openmp_construct
%type<ast> openmp_directive
%type<ast> parallel_construct
%type<ast> for_construct
%type<ast> sections_construct
%type<ast> single_construct
%type<ast> parallel_for_construct
%type<ast> parallel_sections_construct
%type<ast> master_construct
%type<ast> critical_construct
%type<ast> atomic_construct
%type<ast> ordered_construct
%type<ast> barrier_directive
%type<ast> flush_directive
%type<ast> structured_block

%type<ast> parallel_directive
%type<ast> parallel_clause_seq_opt
%type<ast> parallel_clause_seq
%type<ast> parallel_clause
%type<ast> unique_parallel_clause

%type<ast> for_directive
%type<ast> for_clause_opt_seq
%type<ast> for_clause_seq
%type<ast> for_clause
%type<ast> unique_for_clause

%type<ast> sections_directive
%type<ast> sections_clause_opt_seq
%type<ast> sections_clause_seq
%type<ast> sections_clause

%type<ast> section_scope
%type<ast> section_sequence
%type<ast> section_directive

%type<ast> single_directive
%type<ast> single_clause_opt_seq
%type<ast> single_clause_seq
%type<ast> single_clause

%type<ast> parallel_for_directive
%type<ast> parallel_for_clause_opt_seq
%type<ast> parallel_for_clause_seq
%type<ast> parallel_for_clause

%type<ast> parallel_sections_directive
%type<ast> parallel_sections_clause_opt_seq
%type<ast> parallel_sections_clause_seq
%type<ast> parallel_sections_clause

%type<ast> atomic_directive

%type<ast> master_directive

%type<ast> critical_directive
%type<ast> region_phrase_opt
%type<ast> region_phrase

%type<ast> flush_vars_opt
%type<ast> flush_vars

%type<ast> ordered_directive

%type<ast> threadprivate_directive

%type<ast> schedule_kind

%type<ast> data_clause

%type<ast> nowait_clause

%type<ast> variable_list

%type<ast> reduction_operator

%type<ast> custom_construct
%type<ast> custom_construct_directive
%type<ast> custom_directive
%type<ast> custom_clause_opt_seq
%type<ast> custom_clause_seq
%type<ast> custom_parameter_clause
%type<ast> custom_clause

%type<ast> subparsing


%start translation_unit

%%

// *********************************************************
// A.3 - Basic concepts
// *********************************************************

translation_unit : declaration_sequence
{
	*parsed_tree = ASTMake1(AST_TRANSLATION_UNIT, $1, 0, NULL);
}
// This is used for subparsing
| subparsing
{
	*parsed_tree = $1;
}
| /* empty */
{
	*parsed_tree = ASTMake1(AST_TRANSLATION_UNIT, NULL, 0, NULL);
}
;

subparsing : SUBPARSE_EXPRESSION expression
{
	$$ = $2;
}
| SUBPARSE_STATEMENT statement
{
	$$ = $2;
}
| SUBPARSE_MEMBER member_specification
{
	$$ = $2;
}
;

// *********************************************************
// A.6. - Declarations
// *********************************************************

declaration_sequence : declaration
{
	$$ = ASTListLeaf($1);
}
| declaration_sequence declaration
{
	$$ = ASTList($1, $2);
}
;

declaration : block_declaration 
{
	$$ = $1;
}
| function_definition
{
	$$ = $1;
}
| template_declaration
{
	$$ = $1;
}
| explicit_instantiation
{
	$$ = $1;
}
| explicit_specialization
{
	$$ = $1;
}
| linkage_specification
{
	$$ = $1;
}
| namespace_definition
{
	$$ = $1;
}
// OpenMP 2.5
| threadprivate_directive
{
	$$ = $1;
}
| custom_directive
{
	$$ = $1;
}
// GNU Extension
// | EXTENSION declaration
// {
// 	// This extension is designed to shut up gcc's -pedantic
// 	$$ = ASTMake1(AST_GCC_EXTENSION, $2, $1.token_line, NULL);
// }
;

linkage_specification : EXTERN string_literal '{' declaration_sequence '}'
{
	$$ = ASTMake2(AST_LINKAGE_SPEC, $2, $4, $1.token_line, NULL);
}
| EXTERN string_literal '{' '}'
{
	$$ = ASTMake2(AST_LINKAGE_SPEC, $2, NULL, $1.token_line, NULL);
}
| EXTERN string_literal declaration
{
	$$ = ASTMake2(AST_LINKAGE_SPEC_DECL, $2, $3, $1.token_line, NULL);
}
;

namespace_definition : named_namespace_definition
{
	$$ = $1;
}
| unnamed_namespace_definition
{
	$$ = $1;
}
;

// extension_namespace_definition genera exactament el mateix
// que aquesta regla, per aixo més val eliminar-la
named_namespace_definition : NAMESPACE IDENTIFIER '{' declaration_sequence '}'
{
	AST identif = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_NAMESPACE_DEFINITION, identif, $4, $1.token_line, NULL);
}
| NAMESPACE IDENTIFIER '{' '}'
{
	AST identif = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_NAMESPACE_DEFINITION, identif, NULL, $1.token_line, NULL);
}
;

unnamed_namespace_definition : NAMESPACE '{' declaration_sequence '}'
{
	$$ = ASTMake2(AST_NAMESPACE_DEFINITION, NULL, $3, $1.token_line, NULL);
}
| NAMESPACE '{' '}'
{
	$$ = ASTMake2(AST_NAMESPACE_DEFINITION, NULL, NULL, $1.token_line, NULL);
}
;

block_declaration : simple_declaration
{
	$$ = $1;
}
| asm_definition
{
	$$ = $1;
}
| namespace_alias_definition
{
	$$ = $1;
}
| using_declaration
{
	$$ = $1;
}
| using_directive
{
	$$ = $1;
}
| label_declaration 
{
	$$ = $1;
}
| EXTENSION block_declaration
{
	$$ = ASTMake1(AST_GCC_EXTENSION, $2, $1.token_line, $1.token_text);
}
| unknown_pragma
{
	$$ = $1;
}
;

/* GNU Extension */
label_declaration : LABEL label_declarator_seq ';'
{
	$$ = ASTMake1(AST_GCC_LABEL_DECL, $2, $1.token_line, NULL);
}
;

label_declarator_seq : IDENTIFIER 
{
	$$ = ASTListLeaf(ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text));
}
| label_declarator_seq ',' IDENTIFIER
{
	AST label = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);
	$$ = ASTList($1, label);
}
;
/* End of GNU extension */

/* GNU Extension */
attributes : attributes attribute
{
	$$ = ASTList($1, $2);
}
| attribute
{
	$$ = ASTListLeaf($1);
}
;

attribute : ATTRIBUTE '(' '(' attribute_list ')' ')'
{
	$$ = ASTMake1(AST_GCC_ATTRIBUTE, $4, $1.token_line, $1.token_text);
}
| ATTRIBUTE '(''(' ')'')'
{
	$$ = ASTMake1(AST_GCC_ATTRIBUTE, NULL, $1.token_line, $1.token_text);
}
;

attribute_list : attribute_value
{
	$$ = ASTListLeaf($1);
}
| attribute_list ',' attribute_value
{
	$$ = ASTList($1, $3);
}
;

attribute_value : IDENTIFIER
{
	AST identif = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake3(AST_GCC_ATTRIBUTE_EXPR, identif, NULL, NULL, $1.token_line, NULL);
}
| CONST
{
	AST identif = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake3(AST_GCC_ATTRIBUTE_EXPR, identif, NULL, NULL, $1.token_line, NULL);
}
// | IDENTIFIER '(' IDENTIFIER ')'
// {
// 	AST identif1 = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
// 	AST identif2 = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);
// 	
// 	$$ = ASTMake3(AST_GCC_ATTRIBUTE_EXPR, identif1, identif2, NULL, $1.token_line, NULL);
// }
// | IDENTIFIER '(' IDENTIFIER ',' expression_list ')'
// {
// 	AST identif1 = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
// 	AST identif2 = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);
// 	
// 	$$ = ASTMake3(AST_GCC_ATTRIBUTE_EXPR, identif1, identif2, $5, $1.token_line, NULL);
// }
| IDENTIFIER '(' expression_list ')'
{
	AST identif1 = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
	
	$$ = ASTMake3(AST_GCC_ATTRIBUTE_EXPR, identif1, NULL, $3, $1.token_line, NULL);
}
;
/* End of GNU extension */

asm_definition : ASM '(' string_literal ')' ';'
{
	$$ = ASTMake1(AST_ASM_DEFINITION, $3, $1.token_line, $1.token_text);
}
// GNU Extensions
| ASM volatile_optional '(' string_literal ')' ';'
{
	AST asm_parms = ASTMake4(AST_GCC_ASM_DEF_PARMS, 
			$4, NULL, NULL, NULL, ASTLine($4), NULL);
	$$ = ASTMake2(AST_GCC_ASM_DEFINITION, $2, asm_parms, $1.token_line, $1.token_text);
}
| ASM volatile_optional '(' string_literal ':' asm_operand_list ')' ';'
{
	AST asm_parms = ASTMake4(AST_GCC_ASM_DEF_PARMS, 
			$4, $6, NULL, NULL, ASTLine($4), NULL);
	$$ = ASTMake2(AST_GCC_ASM_DEFINITION, $2, asm_parms, $1.token_line, $1.token_text);
}
| ASM volatile_optional '(' string_literal ':' asm_operand_list ':' asm_operand_list ')' ';'
{
	AST asm_parms = ASTMake4(AST_GCC_ASM_DEF_PARMS, 
			$4, $6, $8, NULL, ASTLine($4), NULL);
	$$ = ASTMake2(AST_GCC_ASM_DEFINITION, $2, asm_parms, $1.token_line, $1.token_text);
}
| ASM volatile_optional '(' string_literal ':' asm_operand_list ':' asm_operand_list ':' asm_operand_list ')' ';'
{
	AST asm_parms = ASTMake4(AST_GCC_ASM_DEF_PARMS, 
			$4, $6, $8, $10, ASTLine($4), NULL);
	$$ = ASTMake2(AST_GCC_ASM_DEFINITION, $2, asm_parms, $1.token_line, $1.token_text);
}
;

volatile_optional : /* empty */
{
	$$ = NULL;
}
| VOLATILE
{
	$$ = ASTLeaf(AST_VOLATILE_SPEC, $1.token_line, $1.token_text);
}
;

asm_operand_list : asm_operand_list_nonempty
{
	$$ = $1;
}
| /* empty */
{
	$$ = NULL;
};


/* GNU Extensions */
asm_operand_list_nonempty : asm_operand
{
	$$ = ASTListLeaf($1);
}
| asm_operand_list_nonempty ',' asm_operand
{
	$$ = ASTList($1, $3);
}
;

asm_operand : string_literal '(' expression ')' 
{
	$$ = ASTMake3(AST_GCC_ASM_OPERAND, NULL, $1, $3, ASTLine($1), NULL);
}
| '[' string_literal ']' string_literal '(' expression ')'
{
	$$ = ASTMake3(AST_GCC_ASM_OPERAND, $2, $4, $6, $1.token_line, NULL);
}
| string_literal
{
	$$ = $1;
}
;
/* End of GNU extensions */

namespace_alias_definition : NAMESPACE IDENTIFIER '=' qualified_namespace_specifier ';'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_NAMESPACE_ALIAS, identifier, $4, $1.token_line, NULL);
}
;

qualified_namespace_specifier : IDENTIFIER 
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake3(AST_QUALIFIED_NAMESPACE_SPEC, NULL, NULL, identifier, $1.token_line, NULL);
}
| nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake3(AST_QUALIFIED_NAMESPACE_SPEC, NULL, $1, identifier, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake3(AST_QUALIFIED_NAMESPACE_SPEC, global_op, NULL, identifier, $1.token_line, NULL);
}
| DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_QUALIFIED_NAMESPACE_SPEC, global_op, $2, identifier, $1.token_line, NULL);
}
;

using_declaration : USING nested_name_specifier unqualified_id ';'
{
	$$ = ASTMake3(AST_USING_DECL, NULL, $2, $3, $1.token_line, NULL);
}
| USING DOS_DOS_PUNTS nested_name_specifier unqualified_id ';'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake3(AST_USING_DECL, global_op, $3, $4, $1.token_line, NULL);
}
| USING TYPENAME nested_name_specifier unqualified_id ';'
{
	$$ = ASTMake3(AST_USING_DECL_TYPENAME, NULL, $3, $4, $1.token_line, NULL);
}
| USING TYPENAME DOS_DOS_PUNTS nested_name_specifier unqualified_id ';'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake3(AST_USING_DECL_TYPENAME, global_op, $4, $5, $1.token_line, NULL);
}
| USING DOS_DOS_PUNTS unqualified_id ';'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);
	
	$$ = ASTMake3(AST_USING_DECL, global_op, NULL, $3, $1.token_line, NULL);
}
;


using_directive : USING NAMESPACE IDENTIFIER ';'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_USING_DIRECTIVE, NULL, NULL, identifier, $1.token_line, NULL);
}
| USING NAMESPACE DOS_DOS_PUNTS IDENTIFIER ';'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake3(AST_USING_DIRECTIVE, global_op, NULL, identifier, $1.token_line, NULL);
}
| USING NAMESPACE nested_name_specifier IDENTIFIER ';'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake3(AST_USING_DIRECTIVE, NULL, $3, identifier, $1.token_line, NULL);
}
| USING NAMESPACE DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $5.token_line, $5.token_text);

	$$ = ASTMake3(AST_USING_DIRECTIVE, global_op, $4, identifier, $1.token_line, NULL);
}
;

simple_declaration : decl_specifier_seq init_declarator_list ';' 
{
	$$ = ASTMake2(AST_SIMPLE_DECLARATION, $1, $2, ASTLine($1), NULL);
}
| init_declarator_list ';' 
{
	$$ = ASTMake2(AST_SIMPLE_DECLARATION, NULL, $1, ASTLine($1), NULL);
}
| decl_specifier_seq ';' 
{
	$$ = ASTMake2(AST_SIMPLE_DECLARATION, $1, NULL, ASTLine($1), NULL);
}
| ';'
{
	// This is an error but also a common extension
	$$ = ASTLeaf(AST_EMPTY_DECL, $1.token_line, $1.token_text);
}
;

// decl_specifier_seq : decl_specifier 
// {
// 	$$ = ASTListLeaf($1);
// }
// | decl_specifier_seq decl_specifier 
// {
// 	$$ = ASTList($1, $2);
// }
// ;

decl_specifier_seq : nontype_specifier_seq type_specifier nontype_specifier_seq
{
	$$ = ASTMake3(AST_DECL_SPECIFIER_SEQ, $1, $2, $3, ASTLine($1), NULL);
}
| nontype_specifier_seq type_specifier
{
	$$ = ASTMake3(AST_DECL_SPECIFIER_SEQ, $1, $2, NULL, ASTLine($1), NULL);
}
| type_specifier nontype_specifier_seq
{
	$$ = ASTMake3(AST_DECL_SPECIFIER_SEQ, NULL, $1, $2, ASTLine($1), NULL);
}
| type_specifier
{
	$$ = ASTMake3(AST_DECL_SPECIFIER_SEQ, NULL, $1, NULL, ASTLine($1), NULL);
}
| nontype_specifier_seq
{
	$$ = ASTMake3(AST_DECL_SPECIFIER_SEQ, $1, NULL, NULL, ASTLine($1), NULL);
}
;

nontype_specifier_seq : nontype_specifier
{
	$$ = ASTListLeaf($1);
}
| nontype_specifier_seq nontype_specifier
{
	$$ = ASTList($1, $2);
}
;

nontype_specifier : storage_class_specifier
{
	$$ = $1;
}
// No el volem aqui
// | type_specifier
// {
// 	$$ = $1;
// }
| function_specifier
{
	$$ = $1;
}
| FRIEND
{
	$$ = ASTLeaf(AST_FRIEND_SPEC, $1.token_line, $1.token_text);
}
| TYPEDEF
{
	$$ = ASTLeaf(AST_TYPEDEF_SPEC, $1.token_line, $1.token_text);
}
// El posem aqui per comoditat
| cv_qualifier
{
	$$ = $1;
}
// Els posem aqui repetits
| SIGNED
{
	$$ = ASTLeaf(AST_SIGNED_TYPE, $1.token_line, $1.token_text);
}
| UNSIGNED
{
	$$ = ASTLeaf(AST_UNSIGNED_TYPE, $1.token_line, $1.token_text);
}
| LONG
{
	$$ = ASTLeaf(AST_LONG_TYPE, $1.token_line, $1.token_text);
}
| SHORT
{
	$$ = ASTLeaf(AST_SHORT_TYPE, $1.token_line, $1.token_text);
}
// GNU Extension
| COMPLEX
{
	$$ = ASTLeaf(AST_GCC_COMPLEX_TYPE, $1.token_line, $1.token_text);
}
| attribute
{
	$$ = $1;
}
;

storage_class_specifier : AUTO 
{
	$$ = ASTLeaf(AST_AUTO_SPEC, $1.token_line, $1.token_text);
}
| REGISTER
{
	$$ = ASTLeaf(AST_REGISTER_SPEC, $1.token_line, $1.token_text);
}
| STATIC
{
	$$ = ASTLeaf(AST_STATIC_SPEC, $1.token_line, $1.token_text);
}
| EXTERN
{
	$$ = ASTLeaf(AST_EXTERN_SPEC, $1.token_line, $1.token_text);
}
| MUTABLE
{
	$$ = ASTLeaf(AST_MUTABLE_SPEC, $1.token_line, $1.token_text);
}
// GNU Extension
| THREAD
{
	$$ = ASTLeaf(AST_THREAD_SPEC, $1.token_line, $1.token_text);
}
;

function_specifier : INLINE
{
	$$ = ASTLeaf(AST_INLINE_SPEC, $1.token_line, $1.token_text);
}
| VIRTUAL
{
	$$ = ASTLeaf(AST_VIRTUAL_SPEC, $1.token_line, $1.token_text);
}
| EXPLICIT
{
	$$ = ASTLeaf(AST_EXPLICIT_SPEC, $1.token_line, $1.token_text);
}
;

type_specifier : simple_type_specifier
{
	$$ = $1;
}
| class_specifier
{
	$$ = $1;
}
| enum_specifier
{
	$$ = $1;
}
| elaborated_type_specifier
{
	$$ = $1;
}
// GNU Extensions
| COMPLEX
{
	$$ = ASTLeaf(AST_GCC_COMPLEX_TYPE, $1.token_line, $1.token_text);
}
;

/*
type_specifier_seq has been rewritten in a similar fashion to "decl_specifier_sequence"

type_specifier_seq : nontype_specifier_seq2 type_specifier nontype_specifier_seq2

nontype_specifier_seq2 is a strict subset of nontype_specifier_seq that allows only
const, volatile, long, short, unsigned, signed and __complex__
*/

type_specifier_seq : nontype_specifier_seq2 type_specifier nontype_specifier_seq2
{
	$$ = ASTMake3(AST_TYPE_SPECIFIER_SEQ, $1, $2, $3, ASTLine($1), NULL);
}
| nontype_specifier_seq2 type_specifier
{
	$$ = ASTMake3(AST_TYPE_SPECIFIER_SEQ, $1, $2, NULL, ASTLine($1), NULL);
}
| type_specifier nontype_specifier_seq2
{
	$$ = ASTMake3(AST_TYPE_SPECIFIER_SEQ, NULL, $1, $2, ASTLine($1), NULL);
}
| type_specifier
{
	$$ = ASTMake3(AST_TYPE_SPECIFIER_SEQ, NULL, $1, NULL, ASTLine($1), NULL);
}
;

nontype_specifier_seq2 : nontype_specifier2
{
	$$ = ASTListLeaf($1);
}
| nontype_specifier_seq2 nontype_specifier2
{
	$$ = ASTList($1, $2);
}
;

nontype_specifier2 : cv_qualifier
{
	$$ = $1;
}
// Els posem aqui repetits
| SIGNED
{
	$$ = ASTLeaf(AST_SIGNED_TYPE, $1.token_line, $1.token_text);
}
| UNSIGNED
{
	$$ = ASTLeaf(AST_UNSIGNED_TYPE, $1.token_line, $1.token_text);
}
| LONG
{
	$$ = ASTLeaf(AST_LONG_TYPE, $1.token_line, $1.token_text);
}
| SHORT
{
	$$ = ASTLeaf(AST_SHORT_TYPE, $1.token_line, $1.token_text);
}
// GNU Extension
| COMPLEX
{
	$$ = ASTLeaf(AST_GCC_COMPLEX_TYPE, $1.token_line, $1.token_text);
}
;

simple_type_specifier : type_name
{
	$$ = ASTMake4(AST_SIMPLE_TYPE_SPECIFIER, NULL, NULL, $1, NULL, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_SIMPLE_TYPE_SPECIFIER, global_op, NULL, $2, NULL, $1.token_line, NULL);
}
| nested_name_specifier type_name
{
	$$ = ASTMake4(AST_SIMPLE_TYPE_SPECIFIER, NULL, $1, $2, NULL, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_SIMPLE_TYPE_SPECIFIER, global_op, $2, $3, NULL, $1.token_line, NULL);
}
| nested_name_specifier TEMPLATE template_id
{
	$$ = ASTMake4(AST_SIMPLE_TYPE_SPECIFIER, NULL, $1, NULL, $3, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_SIMPLE_TYPE_SPECIFIER, global_op, $2, NULL, $4, $1.token_line, NULL);
}
| builtin_types
{
	$$ = $1;
}
// GNU Extension
| TYPEOF unary_expression
{
	$$ = ASTMake1(AST_GCC_TYPEOF_EXPR, $2, $1.token_line, $1.token_text);
}
| TYPEOF '(' type_id ')'
{
	$$ = ASTMake1(AST_GCC_TYPEOF, $3, $1.token_line, $1.token_text);
}
;

// Regla simplificada
type_name : IDENTIFIER
{
	$$ = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
}
| template_id
{
	$$ = $1;
}
;

builtin_types : CHAR
{
	$$ = ASTLeaf(AST_CHAR_TYPE, $1.token_line, $1.token_text);
}
| WCHAR_T
{
	$$ = ASTLeaf(AST_WCHAR_TYPE, $1.token_line, $1.token_text);
}
| BOOL
{
	$$ = ASTLeaf(AST_BOOL_TYPE, $1.token_line, $1.token_text);
}
| SHORT
{
	$$ = ASTLeaf(AST_SHORT_TYPE, $1.token_line, $1.token_text);
}
| INT
{
	$$ = ASTLeaf(AST_INT_TYPE, $1.token_line, $1.token_text);
}
| LONG
{
	$$ = ASTLeaf(AST_LONG_TYPE, $1.token_line, $1.token_text);
}
| SIGNED
{
	$$ = ASTLeaf(AST_SIGNED_TYPE, $1.token_line, $1.token_text);
}
| UNSIGNED
{
	$$ = ASTLeaf(AST_UNSIGNED_TYPE, $1.token_line, $1.token_text);
}
| FLOAT
{
	$$ = ASTLeaf(AST_FLOAT_TYPE, $1.token_line, $1.token_text);
}
| DOUBLE
{
	$$ = ASTLeaf(AST_DOUBLE_TYPE, $1.token_line, $1.token_text);
}
| VOID
{
	$$ = ASTLeaf(AST_VOID_TYPE, $1.token_line, $1.token_text);
}
;

elaborated_type_specifier : class_key IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, NULL, NULL, identifier, ASTLine($1), NULL);
}
| class_key DOS_DOS_PUNTS IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, global_op, NULL, identifier, ASTLine($1), NULL);
}
| class_key nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, NULL, $2, identifier, ASTLine($1), NULL);
}
| class_key DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, global_op, $3, identifier, ASTLine($1), NULL);
}
| class_key TEMPLATE template_id
{
	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $1, NULL, NULL, $3, ASTLine($1), NULL);
}
| class_key DOS_DOS_PUNTS TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $1, global_op, NULL, $4, ASTLine($1), NULL);
}
| class_key nested_name_specifier TEMPLATE template_id
{
	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $1, NULL, $2, $4, ASTLine($1), NULL);
}
| class_key DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $1, global_op, $3, $5, ASTLine($1), NULL);
}
| class_key template_id
{
	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, NULL, NULL, $2, ASTLine($1), NULL);
}
| class_key DOS_DOS_PUNTS template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, global_op, NULL, $3, ASTLine($1), NULL);
}
| class_key nested_name_specifier template_id
{
	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, NULL, $2, $3, ASTLine($1), NULL);
}
| class_key DOS_DOS_PUNTS nested_name_specifier template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, global_op, $3, $4, ASTLine($1), NULL);
}
| ENUM IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake3(AST_ELABORATED_TYPE_ENUM, NULL, NULL, identifier, $1.token_line, NULL);
}
| ENUM DOS_DOS_PUNTS IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_ELABORATED_TYPE_ENUM, global_op, NULL, identifier, $1.token_line, NULL);
}
| ENUM nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_ELABORATED_TYPE_ENUM, NULL, $2, identifier, $1.token_line, NULL);
}
| ENUM DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake3(AST_ELABORATED_TYPE_ENUM, global_op, $3, identifier, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_ELABORATED_TYPENAME, NULL, $2, identifier, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake3(AST_ELABORATED_TYPENAME, global_op, $3, identifier, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier template_id
{
	$$ = ASTMake3(AST_ELABORATED_TYPENAME_TEMPLATE, NULL, $2, $3, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake3(AST_ELABORATED_TYPENAME_TEMPLATE, global_op, $3, $4, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier TEMPLATE template_id
{
	$$ = ASTMake3(AST_ELABORATED_TYPENAME_TEMPLATE, NULL, $2, $4, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake3(AST_ELABORATED_TYPENAME_TEMPLATE, global_op, $3, $5, $1.token_line, NULL);
}
// GNU Extensions
| class_key attributes DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $5.token_line, $5.token_text);
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_CLASS, $2, 
			ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, global_op, $4, identifier, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_CLASS, $2, 
			ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, NULL, $3, identifier, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes DOS_DOS_PUNTS IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_CLASS, $2, 
			ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, global_op, NULL, identifier, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_CLASS, $2, 
			ASTMake4(AST_ELABORATED_TYPE_CLASS, $1, NULL, NULL, identifier, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes template_id
{
	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, NULL, NULL, $3, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes DOS_DOS_PUNTS template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, global_op, NULL, $4, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier template_id
{
	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, NULL, $3, $4, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes DOS_DOS_PUNTS nested_name_specifier template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, global_op, $4, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes TEMPLATE template_id
{
	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, NULL, NULL, $4, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes DOS_DOS_PUNTS TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, global_op, NULL, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier TEMPLATE template_id
{
	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, NULL, $3, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake2(AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, $2,
			ASTMake4(AST_ELABORATED_TYPE_TEMPLATE_CLASS, $1, global_op, $4, $6, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| ENUM attributes IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_GCC_ELABORATED_TYPE_ENUM, NULL, NULL, identifier, $2, $1.token_line, NULL);
}
| ENUM attributes DOS_DOS_PUNTS IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake4(AST_GCC_ELABORATED_TYPE_ENUM, global_op, NULL, identifier, $2, $1.token_line, NULL);
}
| ENUM attributes nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake4(AST_GCC_ELABORATED_TYPE_ENUM, NULL, $3, identifier, $2, $1.token_line, NULL);
}
| ENUM attributes DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $5.token_line, $5.token_text);

	$$ = ASTMake4(AST_GCC_ELABORATED_TYPE_ENUM, global_op, $4, identifier, $2, $1.token_line, NULL);
}
;



// *********************************************************
// A.7 - Declarators
// *********************************************************
init_declarator_list : init_declarator
{
	$$ = ASTListLeaf($1);
}
| init_declarator_list ',' init_declarator
{
	$$ = ASTList($1, $3);
}
;

init_declarator : declarator 
{
	$$ = ASTMake2(AST_INIT_DECLARATOR, $1, NULL, ASTLine($1), NULL);
}
| declarator initializer
{
	$$ = ASTMake2(AST_INIT_DECLARATOR, $1, $2, ASTLine($1), NULL);
}
// GNU Extensions
| declarator asm_specification 
{
	$$ = ASTMake4(AST_GCC_INIT_DECLARATOR, $1, NULL, $2, NULL, ASTLine($1), NULL);
}
| declarator attributes
{
	$$ = ASTMake4(AST_GCC_INIT_DECLARATOR, $1, NULL, NULL, $2, ASTLine($1), NULL);
}
| declarator asm_specification attributes
{
	$$ = ASTMake4(AST_GCC_INIT_DECLARATOR, $1, NULL, $2, $3, ASTLine($1), NULL);
}
| declarator asm_specification initializer
{
	$$ = ASTMake4(AST_GCC_INIT_DECLARATOR, $1, $3, $2, NULL, ASTLine($1), NULL);
}
| declarator attributes initializer
{
	$$ = ASTMake4(AST_GCC_INIT_DECLARATOR, $1, $3, NULL, $2, ASTLine($1), NULL);
}
| declarator asm_specification attributes initializer
{
	$$ = ASTMake4(AST_GCC_INIT_DECLARATOR, $1, $4, $2, $3, ASTLine($1), NULL);
}
;

/* GNU Extension */
asm_specification : ASM '(' string_literal ')'
{
	$$ = ASTMake1(AST_GCC_ASM_SPEC, $3, $1.token_line, NULL);
}
;
/* End of GNU Extension */

declarator : direct_declarator
{
	$$ = ASTMake1(AST_DECLARATOR, $1, ASTLine($1), NULL);
}
| ptr_operator declarator
{
	$$ = ASTMake2(AST_POINTER_DECL, $1, $2, ASTLine($1), NULL);
}
;

ptr_operator : '*'
{
	$$ = ASTMake3(AST_POINTER_SPEC, NULL, NULL, NULL, $1.token_line, NULL);
}
| '*' cv_qualifier_seq
{
	$$ = ASTMake3(AST_POINTER_SPEC, NULL, NULL, $2, $1.token_line, NULL);
}
| nested_name_specifier '*'
{
	$$ = ASTMake3(AST_POINTER_SPEC, NULL, $1, NULL, $2.token_line, NULL);
}
| nested_name_specifier '*' cv_qualifier_seq
{
	$$ = ASTMake3(AST_POINTER_SPEC, NULL, $1, $3, $2.token_line, NULL);
}
| DOS_DOS_PUNTS nested_name_specifier '*'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_POINTER_SPEC, global_op, $2, NULL, $3.token_line, NULL);
}
| DOS_DOS_PUNTS nested_name_specifier '*' cv_qualifier_seq
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_POINTER_SPEC, global_op, $2, $4, $3.token_line, NULL);
}
| '&'
{
	$$ = ASTLeaf(AST_REFERENCE_SPEC, $1.token_line, NULL);
}
// GNU Extensions
| '&' cv_qualifier_seq
{
	$$ = ASTMake1(AST_GCC_REFERENCE_SPEC, $2, $1.token_line, NULL);
}
;

/*
   A functional declarator is a syntactic enforced declarator that will have
   a functional nature
 */
functional_declarator : functional_direct_declarator
{
	$$ = ASTMake1(AST_DECLARATOR, $1, ASTLine($1), NULL);
}
| ptr_operator functional_declarator
{
	$$ = ASTMake2(AST_POINTER_DECL, $1, $2, ASTLine($1), NULL);
}
;

functional_direct_declarator : functional_declarator_id
{
	$$ = $1;
}
| functional_direct_declarator '(' parameter_declaration_clause ')'
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, NULL, NULL, ASTLine($1), NULL);
}
| functional_direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, $5, NULL, ASTLine($1), NULL);
}
| functional_direct_declarator '(' parameter_declaration_clause ')' exception_specification
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, NULL, $5, ASTLine($1), NULL);
}
| functional_direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, $5, $6, ASTLine($1), NULL);
}
| functional_direct_declarator '[' constant_expression ']'
{
	$$ = ASTMake2(AST_DECLARATOR_ARRAY, $1, $3, ASTLine($1), NULL);
}
| functional_direct_declarator '[' ']'
{
	$$ = ASTMake2(AST_DECLARATOR_ARRAY, $1, NULL, ASTLine($1), NULL);
}
| '(' functional_declarator ')'
{
	$$ = ASTMake1(AST_PARENTHESIZED_DECLARATOR, $2, $1.token_line, NULL);
}
;

functional_declarator_id : declarator_id '(' parameter_declaration_clause ')'
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, NULL, NULL, ASTLine($1), NULL);
}
| declarator_id '(' parameter_declaration_clause ')' cv_qualifier_seq
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, $5, NULL, ASTLine($1), NULL);
}
| declarator_id '(' parameter_declaration_clause ')' exception_specification
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, NULL, $5, ASTLine($1), NULL);
}
| declarator_id '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, $5, $6, ASTLine($1), NULL);
}
;

cv_qualifier_seq : cv_qualifier
{
	$$ = ASTListLeaf($1);
}
| cv_qualifier_seq cv_qualifier
{
	$$ = ASTList($1, $2);
}
;

cv_qualifier : CONST
{
	$$ = ASTLeaf(AST_CONST_SPEC, $1.token_line, $1.token_text);
}
| VOLATILE
{
	$$ = ASTLeaf(AST_VOLATILE_SPEC, $1.token_line, $1.token_text);
}
// GNU Extension
| RESTRICT
{
	$$ = ASTLeaf(AST_GCC_RESTRICT_SPEC, $1.token_line, $1.token_text);
}
;

direct_declarator : declarator_id
{
	$$ = $1;
}
| direct_declarator '(' parameter_declaration_clause ')'
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, NULL, NULL, ASTLine($1), NULL);
}
| direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, $5, NULL, ASTLine($1), NULL);
}
| direct_declarator '(' parameter_declaration_clause ')' exception_specification
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, NULL, $5, ASTLine($1), NULL);
}
| direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
	$$ = ASTMake4(AST_DECLARATOR_FUNC, $1, $3, $5, $6, ASTLine($1), NULL);
}
| direct_declarator '[' constant_expression ']'
{
	$$ = ASTMake2(AST_DECLARATOR_ARRAY, $1, $3, ASTLine($1), NULL);
}
| direct_declarator '[' ']'
{
	$$ = ASTMake2(AST_DECLARATOR_ARRAY, $1, NULL, ASTLine($1), NULL);
}
| '(' declarator ')'
{
	$$ = ASTMake1(AST_PARENTHESIZED_DECLARATOR, $2, $1.token_line, NULL);
}
;

declarator_id : id_expression
{
	$$ = ASTMake1(AST_DECLARATOR_ID_EXPR, $1, ASTLine($1), NULL);
}
;


enum_specifier : ENUM IDENTIFIER '{' enumeration_list '}'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_ENUM_SPECIFIER, identifier, $4, $1.token_line, NULL);
}
| ENUM '{' enumeration_list '}'
{
	$$ = ASTMake2(AST_ENUM_SPECIFIER, NULL, $3, $1.token_line, NULL);
}
| ENUM IDENTIFIER '{' '}'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_ENUM_SPECIFIER, identifier, NULL, $1.token_line, NULL);
}
| ENUM '{' '}'
{
	$$ = ASTMake2(AST_ENUM_SPECIFIER, NULL, NULL, $1.token_line, NULL);
}
;

enumeration_list : enumeration_list_proper
{
	$$ = $1;
}
// This is a running comma that many people forgets here. It is of non
// standard nature
| enumeration_list_proper ','
{
	$$ = $1;
};

enumeration_list_proper : enumeration_list_proper ',' enumeration_definition
{
	$$ = ASTList($1, $3);
}
| enumeration_definition
{
	$$ = ASTListLeaf($1);
}
;

enumeration_definition : IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake2(AST_ENUM_DEF, identifier, NULL, $1.token_line, NULL);
}
| IDENTIFIER '=' constant_expression
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake2(AST_ENUM_DEF, identifier, $3, $1.token_line, NULL);
}
;

type_id : type_specifier_seq
{
	$$ = ASTMake2(AST_TYPE_ID, $1, NULL, ASTLine($1), NULL);
}
| type_specifier_seq abstract_declarator
{
	$$ = ASTMake2(AST_TYPE_ID, $1, $2, ASTLine($1), NULL);
}
;

abstract_declarator : ptr_operator
{
	$$ = ASTMake2(AST_ABSTRACT_DECLARATOR, $1, NULL, ASTLine($1), NULL);
}
| ptr_operator abstract_declarator
{
	$$ = ASTMake2(AST_ABSTRACT_DECLARATOR, $1, $2, ASTLine($1), NULL);
}
| direct_abstract_declarator
{
	$$ = $1;
}
;

direct_abstract_declarator : '(' abstract_declarator ')'
{
	$$ = ASTMake1(AST_PARENTHESIZED_ABSTRACT_DECLARATOR, $2, $1.token_line, NULL);
}
| '(' parameter_declaration_clause ')'
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, NULL, $2, NULL, NULL, $1.token_line, NULL);
}
| '(' parameter_declaration_clause ')' cv_qualifier_seq
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, NULL, $2, $4, NULL, $1.token_line, NULL);
}
| '(' parameter_declaration_clause ')' exception_specification
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, NULL, $2, NULL, $4, $1.token_line, NULL);
}
| '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, NULL, $2, $4, $5, $1.token_line, NULL);
}
| direct_abstract_declarator '(' parameter_declaration_clause ')'
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, $1, $3, NULL, NULL, ASTLine($1), NULL);
}
| direct_abstract_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, $1, $3, $5, NULL, ASTLine($1), NULL);
}
| direct_abstract_declarator '(' parameter_declaration_clause ')' exception_specification
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, $1, $3, NULL, $5, ASTLine($1), NULL);
}
| direct_abstract_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
	$$ = ASTMake4(AST_ABSTRACT_DECLARATOR_FUNC, $1, $3, $5, $6, ASTLine($1), NULL);
}
| '[' constant_expression ']'
{
	$$ = ASTMake2(AST_ABSTRACT_ARRAY, NULL, $2, $1.token_line, NULL);
}
| '[' ']'
{
	$$ = ASTMake2(AST_ABSTRACT_ARRAY, NULL, NULL, $1.token_line, NULL);
}
| direct_abstract_declarator '[' constant_expression ']'
{
	$$ = ASTMake2(AST_ABSTRACT_ARRAY, $1, $3, ASTLine($1), NULL);
}
| direct_abstract_declarator '[' ']'
{
	$$ = ASTMake2(AST_ABSTRACT_ARRAY, $1, NULL, ASTLine($1), NULL);
}
;

parameter_declaration_clause : parameter_declaration_clause_nonempty
{
	$$ = $1;
}
| parameter_declaration_clause_nonempty ',' TRES_PUNTS
{
	$$ = ASTList($1, ASTLeaf(AST_VARIADIC_ARG, $3.token_line, $3.token_text));
} 
| parameter_declaration_clause_nonempty TRES_PUNTS
{
	$$ = ASTList($1, ASTLeaf(AST_VARIADIC_ARG, $2.token_line, $2.token_text));
} 
// Com funciona aixo ?
| TRES_PUNTS
{
	$$ = ASTListLeaf(ASTLeaf(AST_VARIADIC_ARG, $1.token_line, $1.token_text));
}
// No m'agrada aixo, però farem una excepció
| /* empty */
{
	$$ = ASTLeaf(AST_EMPTY_PARAMETER_DECLARATION_CLAUSE, 0, NULL);
}
;

parameter_declaration_clause_nonempty : parameter_declaration_list
{
	$$ = $1;
}
;

parameter_declaration_list : parameter_declaration
{
	$$ = ASTListLeaf($1);
}
| parameter_declaration_list ',' parameter_declaration
{
	$$ = ASTList($1, $3);
}
;

parameter_declaration : decl_specifier_seq declarator 
{
	$$ = ASTMake3(AST_PARAMETER_DECL, $1, $2, NULL, ASTLine($1), NULL);
}
| decl_specifier_seq declarator '=' assignment_expression 
{
	$$ = ASTMake3(AST_PARAMETER_DECL, $1, $2, $4, ASTLine($1), NULL);
}
| decl_specifier_seq 
{
	$$ = ASTMake3(AST_PARAMETER_DECL, $1, NULL, NULL, ASTLine($1), NULL);
}
| decl_specifier_seq abstract_declarator 
{
	$$ = ASTMake3(AST_PARAMETER_DECL, $1, $2, NULL, ASTLine($1), NULL);
}
| decl_specifier_seq  '=' assignment_expression 
{
	$$ = ASTMake3(AST_PARAMETER_DECL, $1, NULL, $3, ASTLine($1), NULL);
}
| decl_specifier_seq abstract_declarator '=' assignment_expression
{
	$$ = ASTMake3(AST_PARAMETER_DECL, $1, $2, $4, ASTLine($1), NULL);
}
// GCC Extension
| decl_specifier_seq declarator attributes
{
	$$ = ASTMake4(AST_GCC_PARAMETER_DECL, $1, $2, NULL, $3, ASTLine($1), NULL);
}
| decl_specifier_seq declarator attributes '=' assignment_expression 
{
	$$ = ASTMake4(AST_GCC_PARAMETER_DECL, $1, $2, $5, $3, ASTLine($1), NULL);
}
| decl_specifier_seq abstract_declarator attributes
{
	$$ = ASTMake4(AST_GCC_PARAMETER_DECL, $1, $2, NULL, $3, ASTLine($1), NULL);
}
| decl_specifier_seq abstract_declarator attributes '=' assignment_expression
{
	$$ = ASTMake4(AST_GCC_PARAMETER_DECL, $1, $2, $5, $3, ASTLine($1), NULL);
}
;

initializer : '=' initializer_clause
{
	$$ = ASTMake1(AST_INITIALIZER, $2, $1.token_line, NULL);
}
| '(' expression_list ')'
{
	$$ = ASTMake1(AST_PARENTHESIZED_INITIALIZER, $2, $1.token_line, NULL);
}
;

initializer_clause : assignment_expression
{
	$$ = ASTMake1(AST_INITIALIZER_EXPR, $1, ASTLine($1), NULL);
}
| '{' initializer_list '}'
{
	$$ = ASTMake1(AST_INITIALIZER_BRACES, $2, $1.token_line, NULL);
}
| '{' initializer_list ',' '}'
{
	$$ = ASTMake1(AST_INITIALIZER_BRACES, $2, $1.token_line, NULL);
}
| '{' '}'
{
	$$ = ASTMake1(AST_INITIALIZER_BRACES, NULL, $1.token_line, NULL);
}
;

initializer_list : initializer_clause
{
	$$ = ASTListLeaf($1);
}
| initializer_list ',' initializer_clause
{
	$$ = ASTList($1, $3);
}
| designation initializer_clause
{
    AST designated_initializer = ASTMake2(AST_DESIGNATED_INITIALIZER, $1, $2, ASTLine($1), NULL);

    $$ = ASTListLeaf(designated_initializer);
}
| initializer_list ',' designation initializer_clause
{
    AST designated_initializer = ASTMake2(AST_DESIGNATED_INITIALIZER, $3, $4, ASTLine($3), NULL);

    $$ = ASTList($1, designated_initializer);
}
// GNU Extensions
| IDENTIFIER ':' initializer_clause
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	AST gcc_initializer_clause = ASTMake2(AST_GCC_INITIALIZER_CLAUSE, identifier, $3, $1.token_line, NULL);

	$$ = ASTListLeaf(gcc_initializer_clause);
}
| initializer_list ',' IDENTIFIER ':' initializer_clause
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	AST gcc_initializer_clause = ASTMake2(AST_GCC_INITIALIZER_CLAUSE, identifier, $5, ASTLine($1), NULL);

	$$ = ASTList($1, gcc_initializer_clause);
}
;

designation : designator_list '='
{
    $$ = ASTMake1(AST_DESIGNATION, $1, ASTLine($1), NULL);
}
;

designator_list : designator
{
    $$ = ASTListLeaf($1);
}
| designator_list designator
{
    $$ = ASTList($1, $2);
}
;

designator : '[' constant_expression ']'
{
    $$ = ASTMake1(AST_INDEX_DESIGNATOR, $2, $1.token_line, NULL);
}
| '.' IDENTIFIER
{
    AST symbol = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

    $$ = ASTMake1(AST_FIELD_DESIGNATOR, symbol, $1.token_line, NULL);
}
;

function_definition : functional_declarator function_body
{
	$$ = ASTMake4(AST_FUNCTION_DEFINITION, NULL, $1, NULL, $2, ASTLine($1), NULL);
}
| functional_declarator ctor_initializer function_body 
{
	$$ = ASTMake4(AST_FUNCTION_DEFINITION, NULL, $1, $2, $3, ASTLine($1), NULL);
}
| decl_specifier_seq functional_declarator function_body 
{
	$$ = ASTMake4(AST_FUNCTION_DEFINITION, $1, $2, NULL, $3, ASTLine($1), NULL);
}
| decl_specifier_seq functional_declarator ctor_initializer function_body 
{
	$$ = ASTMake4(AST_FUNCTION_DEFINITION, $1, $2, $3, $4, ASTLine($1), NULL);
}
| EXTENSION function_definition
{
	$$ = ASTMake1(AST_GCC_EXTENSION, $2, $1.token_line, $1.token_text);
}
;

function_body : compound_statement
{
	$$ = ASTMake1(AST_FUNCTION_BODY, $1, ASTLine($1), NULL);
}
;

// *********************************************************
// A.8 - Classes
// *********************************************************

class_specifier : class_head '{' member_specification '}'
{
	$$ = ASTMake2(AST_CLASS_SPECIFIER, $1, $3, ASTLine($1), NULL);
}
| class_head '{' '}'
{
	AST member_declaration = ASTLeaf(AST_EMPTY_DECL, $2.token_line, NULL);
	AST member_specification = 
		ASTMake3(AST_MEMBER_SPEC, NULL, member_declaration, 
				NULL, ASTLine(member_declaration), NULL);

	$$ = ASTMake2(AST_CLASS_SPECIFIER, $1, member_specification, 
			ASTLine($1), NULL);
}
;

class_head : class_key 
{
	$$ = ASTMake4(AST_CLASS_HEAD, $1, NULL, NULL, NULL, ASTLine($1), NULL);
}
| class_key IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake4(AST_CLASS_HEAD, $1, NULL, identifier, NULL, ASTLine($1), NULL);
}
| class_key base_clause
{
	$$ = ASTMake4(AST_CLASS_HEAD, $1, NULL, NULL, $2, ASTLine($1), NULL);
}
| class_key IDENTIFIER base_clause
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake4(AST_CLASS_HEAD, $1, NULL, identifier, $3, ASTLine($1), NULL);
}
| class_key nested_name_specifier IDENTIFIER 
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_CLASS_HEAD, $1, $2, identifier, NULL, ASTLine($1), NULL);
}
| class_key nested_name_specifier IDENTIFIER base_clause
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_CLASS_HEAD, $1, $2, identifier, $4, ASTLine($1), NULL);
}
| class_key template_id
{
	$$ = ASTMake4(AST_CLASS_HEAD, $1, NULL, $2, NULL, ASTLine($1), NULL);
}
| class_key template_id base_clause
{
	$$ = ASTMake4(AST_CLASS_HEAD, $1, NULL, $2, $3, ASTLine($1), NULL);
}
| class_key nested_name_specifier template_id
{
	$$ = ASTMake4(AST_CLASS_HEAD, $1, $2, $3, NULL, ASTLine($1), NULL);
}
| class_key nested_name_specifier template_id base_clause
{
	$$ = ASTMake4(AST_CLASS_HEAD, $1, $2, $3, $4, ASTLine($1), NULL);
}
// GNU Extensions
| class_key attributes
{
	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, NULL, NULL, NULL, ASTLine($1), NULL), 
			ASTLine($1), NULL);
}
| class_key attributes IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, NULL, identifier, NULL, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes base_clause
{
	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2,
			ASTMake4(AST_CLASS_HEAD, $1, NULL, NULL, $3, ASTLine($1), NULL), 
			ASTLine($1), NULL);
}
| class_key attributes IDENTIFIER base_clause
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, NULL, identifier, $4, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, $3, identifier, NULL, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier IDENTIFIER base_clause
{
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2,
			ASTMake4(AST_CLASS_HEAD, $1, $3, identifier, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes template_id 
{
	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, NULL, $3, NULL, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier template_id 
{
	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, $3, $4, NULL, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes template_id base_clause
{
	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, NULL, $3, $4, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| class_key attributes nested_name_specifier template_id base_clause
{
	$$ = ASTMake2(AST_GCC_CLASS_HEAD, $2, 
			ASTMake4(AST_CLASS_HEAD, $1, $3, $4, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
;

class_key : CLASS
{
	$$ = ASTLeaf(AST_CLASS_KEY_CLASS, $1.token_line, $1.token_text);
}
| STRUCT
{
	$$ = ASTLeaf(AST_CLASS_KEY_STRUCT, $1.token_line, $1.token_text);
}
| UNION
{
	$$ = ASTLeaf(AST_CLASS_KEY_UNION, $1.token_line, $1.token_text);
}
;

member_specification : member_declaration
{
	$$ = ASTMake3(AST_MEMBER_SPEC, NULL, $1, NULL, ASTLine($1), NULL);
}
| access_specifier ':'
{
	$$ = ASTMake3(AST_MEMBER_SPEC, $1, NULL, NULL, ASTLine($1), NULL);
}
| member_declaration member_specification
{
	$$ = ASTMake3(AST_MEMBER_SPEC, NULL, $1, $2, ASTLine($1), NULL);
}
| access_specifier ':' member_specification
{
	$$ = ASTMake3(AST_MEMBER_SPEC, $1, NULL, $3, ASTLine($1), NULL);
}
;

member_declaration : decl_specifier_seq member_declarator_list ';'  
{
	$$ = ASTMake2(AST_MEMBER_DECLARATION, $1, $2, ASTLine($1), NULL);
}
| decl_specifier_seq ';' 
{
	$$ = ASTMake2(AST_MEMBER_DECLARATION, $1, NULL, ASTLine($1), NULL);
}
| member_declarator_list ';' 
{
	$$ = ASTMake2(AST_MEMBER_DECLARATION, NULL, $1, ASTLine($1), NULL);
}
// This is in the standard, but this causes an ambiguity with an empty declaration
// | function_definition ';'
// {
// 	$$ = $1;
// }
| function_definition 
{
	$$ = $1;
}
| nested_name_specifier unqualified_id ';'
{
	$$ = ASTMake3(AST_MEMBER_DECLARATION_QUALIFIED, NULL, $1, $2, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier unqualified_id ';'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_MEMBER_DECLARATION_QUALIFIED, global_op, $2, $3, $1.token_line, NULL);
}
| nested_name_specifier TEMPLATE unqualified_id ';'
{
	$$ = ASTMake3(AST_MEMBER_DECLARATION_TEMPLATE, NULL, $1, $3, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE unqualified_id ';'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	
	$$ = ASTMake3(AST_MEMBER_DECLARATION_TEMPLATE, global_op, $2, $4, $1.token_line, NULL);
}
| using_declaration
{
	$$ = $1;
}
| template_declaration
{
	$$ = $1;
}
| unknown_pragma
{
	$$ = $1;
}
// This is a common tolerated error
| ';' 
{
	$$ = ASTLeaf(AST_EMPTY_DECL, $1.token_line, NULL);
}
// GNU Extension
| EXTENSION member_declaration
{
	$$ = ASTMake1(AST_GCC_EXTENSION, $2, $1.token_line, $1.token_text);
}
;

member_declarator_list : member_declarator
{
	$$ = ASTListLeaf($1);
}
| member_declarator_list ',' member_declarator
{
	$$ = ASTList($1, $3);
}
;

member_declarator : declarator 
{
	$$ = ASTMake2(AST_MEMBER_DECLARATOR, $1, NULL, ASTLine($1), NULL);
}
| declarator constant_initializer
{
	$$ = ASTMake2(AST_MEMBER_DECLARATOR, $1, $2, ASTLine($1), NULL);
}
// - El susbsumirem amb constant_initializer -
// | declarator pure_specifier
// {
// }
| ':' constant_expression
{
	$$ = ASTMake2(AST_BITFIELD_DECLARATOR, NULL, $2, $1.token_line, NULL);
}
| IDENTIFIER ':' constant_expression
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
	AST declarator_id_expr = ASTMake1(AST_DECLARATOR_ID_EXPR, identifier, ASTLine(identifier), NULL);

	$$ = ASTMake2(AST_BITFIELD_DECLARATOR, declarator_id_expr, $3, $1.token_line, NULL);
}
// GNU Extensions
| declarator attributes 
{
	$$ = ASTMake3(AST_GCC_MEMBER_DECLARATOR, $1, NULL, $2, ASTLine($1), NULL);
}
// - El susbsumirem amb constant_initializer -
// | declarator attributes pure_specifier
// }
| declarator attributes constant_initializer
{
	$$ = ASTMake3(AST_GCC_MEMBER_DECLARATOR, $1, $3, $2, ASTLine($1), NULL);
}
| IDENTIFIER attributes ':' constant_expression
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake3(AST_GCC_BITFIELD_DECLARATOR, identifier, $4, $2, $1.token_line, NULL);
}
| attributes ':' constant_expression
{
	$$ = ASTMake3(AST_GCC_BITFIELD_DECLARATOR, NULL, $3, $1, ASTLine($1), NULL);
}
;

constant_initializer : '=' constant_expression
{
	$$ = ASTMake1(AST_CONSTANT_INITIALIZER, $2, $1.token_line, NULL);
}
;

// -- Invalid Rule -- informational only
// pure_specifier : '=' '0'
// {
// }
// ;

// *********************************************************
// A.9 - Derived classes
// *********************************************************

base_clause : ':' base_specifier_list
{
	$$ = ASTMake1(AST_BASE_CLAUSE, $2, $1.token_line, NULL);
}
;

base_specifier_list : base_specifier
{
	$$ = ASTListLeaf($1);
}
| base_specifier_list ',' base_specifier
{
	$$ = ASTList($1, $3);
}
;

base_specifier : type_name
// without access_specifier
{
	$$ = ASTMake3(AST_BASE_SPECIFIER, NULL, NULL, $1, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_BASE_SPECIFIER, global_op, NULL, $2, $1.token_line, NULL);
}
| DOS_DOS_PUNTS nested_name_specifier type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_BASE_SPECIFIER, global_op, $2, $3, $1.token_line, NULL);
}
| nested_name_specifier type_name
{
	$$ = ASTMake3(AST_BASE_SPECIFIER, NULL, $1, $2, ASTLine($1), NULL);
}
// without virtual but with access_specifier
| access_specifier type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS, $1, NULL, NULL, $2, ASTLine($1), NULL);
}
| access_specifier DOS_DOS_PUNTS type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS, $1, global_op, NULL, $3, ASTLine($1), NULL);
}
| access_specifier nested_name_specifier type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS, $1, NULL, $2, $3, ASTLine($1), NULL);
}
| access_specifier DOS_DOS_PUNTS nested_name_specifier type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS, $1, global_op, $3, $4, ASTLine($1), NULL);
}
// with virtual and optionally access_specifier
| VIRTUAL type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, NULL, NULL, NULL, $2, $1.token_line, NULL);
}
| VIRTUAL access_specifier type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, $2, NULL, NULL, $3, $1.token_line, NULL);
}
| VIRTUAL DOS_DOS_PUNTS type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, NULL, global_op, NULL, $3, $1.token_line, NULL);
}
| VIRTUAL access_specifier DOS_DOS_PUNTS type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, $2, global_op, NULL, $4, $1.token_line, NULL);
}
| VIRTUAL nested_name_specifier type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, NULL, NULL, $2, $3, $1.token_line, NULL);
}
| VIRTUAL access_specifier nested_name_specifier type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, $2, NULL, $3, $4, $1.token_line, NULL);
}
| VIRTUAL DOS_DOS_PUNTS nested_name_specifier type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, NULL, global_op, $3, $4, $1.token_line, NULL);
}
| VIRTUAL access_specifier DOS_DOS_PUNTS nested_name_specifier type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_VIRTUAL, $2, global_op, $4, $5, $1.token_line, NULL);
}
// with access_specifier and then virtual
| access_specifier VIRTUAL type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS_VIRTUAL, $1, NULL, NULL, $3, ASTLine($1), NULL);
}
| access_specifier VIRTUAL DOS_DOS_PUNTS type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS_VIRTUAL, $1, global_op, NULL, $4, ASTLine($1), NULL);
}
| access_specifier VIRTUAL nested_name_specifier type_name
{
	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS_VIRTUAL, $1, NULL, $3, $4, ASTLine($1), NULL);
}
| access_specifier VIRTUAL DOS_DOS_PUNTS nested_name_specifier type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $3.token_line, NULL);

	$$ = ASTMake4(AST_BASE_SPECIFIER_ACCESS_VIRTUAL, $1, global_op, $4, $5, ASTLine($1), NULL);
}
;

access_specifier : PRIVATE
{
	$$ = ASTLeaf(AST_PRIVATE_SPEC, $1.token_line, $1.token_text);
}
| PROTECTED
{
	$$ = ASTLeaf(AST_PROTECTED_SPEC, $1.token_line, $1.token_text);
}
| PUBLIC
{
	$$ = ASTLeaf(AST_PUBLIC_SPEC, $1.token_line, $1.token_text);
}
;

// *********************************************************
// A.5. - Statements
// *********************************************************


statement : no_if_statement
{
	$$ = $1;
}
| if_statement
{
	$$ = $1;
}
| if_else_statement
{
	$$ = $1;
}
;

no_if_statement : labeled_statement
{
	$$ = $1;
}
| expression_statement 
{
	$$ = $1;
}
| compound_statement
{
	$$ = $1;
}
| selection_statement
{
	$$ = $1;
}
| iteration_statement
{
	$$ = $1;
}
| jump_statement
{
	$$ = $1;
}
| declaration_statement  
{
	$$ = $1;
}
| try_block
{
	$$ = $1;
}
/* OpenMP 2.5 support */
| openmp_construct
{
	$$ = $1;
}
;

labeled_statement : IDENTIFIER ':' statement
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
	
	$$ = ASTMake2(AST_LABELED_STATEMENT, identifier, $3, $1.token_line, NULL);
}
| CASE constant_expression ':' statement
{
	$$ = ASTMake2(AST_CASE_STATEMENT, $2, $4, $1.token_line, NULL);
}
| DEFAULT ':' statement
{
	$$ = ASTMake1(AST_DEFAULT_STATEMENT, $3, $1.token_line, NULL);
}
// GNU Extension
| CASE constant_expression TRES_PUNTS constant_expression ':' statement
{
	$$ = ASTMake3(AST_GCC_CASE_STATEMENT, $2, $4, $6, $1.token_line, NULL);
}
;

expression_statement : expression ';'
{
	$$ = ASTMake1(AST_EXPRESSION_STATEMENT, $1, ASTLine($1), NULL);
}
// | ';'
// {
// 	// Empty statement ...
// 	$$ = ASTLeaf(AST_EMPTY_STATEMENT, $1.token_line, NULL);
// }
;

declaration_statement : block_declaration
{
	$$ = ASTMake1(AST_DECLARATION_STATEMENT, $1, ASTLine($1), NULL);
}
;

compound_statement : '{' statement_seq '}'
{
	$$ = ASTMake1(AST_COMPOUND_STATEMENT, $2, $1.token_line, NULL);
}
| '{' '}'
{
	$$ = ASTMake1(AST_COMPOUND_STATEMENT, NULL, $1.token_line, NULL);
}
;

statement_seq : statement
{
	$$ = ASTListLeaf($1);
}
| statement_seq statement
{
	$$ = ASTList($1, $2);
}
// Support for OpenMP 2.5
| openmp_directive
{
	$$ = ASTListLeaf($1);
}
| statement_seq openmp_directive
{
	$$ = ASTList($1, $2);
}
;

// If ambiguity
if_statement : IF '(' condition ')' statement
{
	$$ = ASTMake3(AST_IF_ELSE_STATEMENT, $3, $5, NULL, $1.token_line, NULL);
}
;

// Only if's with else within them
if_else_statement : IF '(' condition ')' if_else_eligible_statements ELSE statement
{
	$$ = ASTMake3(AST_IF_ELSE_STATEMENT, $3, $5, $7, $1.token_line, NULL);
}
;

// Aqui nomes generarem if's q tenen else a dins
if_else_eligible_statements : no_if_statement
{
	$$ = $1;
}
| if_else_statement
{
	$$ = $1;
}
;

selection_statement : SWITCH '(' condition ')' statement
{
	$$ = ASTMake2(AST_SWITCH_STATEMENT, $3, $5, $1.token_line, NULL);
}
;

condition : expression
{
	$$ = ASTMake3(AST_CONDITION, NULL, NULL, $1, ASTLine($1), NULL);
}
| type_specifier_seq declarator '=' assignment_expression
{
	$$ = ASTMake3(AST_CONDITION, $1, $2, $4, ASTLine($1), NULL);
}
// GNU Extension
| type_specifier_seq declarator asm_specification attributes '=' assignment_expression
{
	$$ = ASTMake2(AST_GCC_CONDITION, $4,
			ASTMake4(AST_GCC_CONDITION_DECL, $1, $2, $3, $6, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| type_specifier_seq declarator attributes '=' assignment_expression
{
	$$ = ASTMake2(AST_GCC_CONDITION, $3,
			ASTMake4(AST_GCC_CONDITION_DECL, $1, $2, NULL, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
| type_specifier_seq declarator asm_specification '=' assignment_expression
{
	$$ = ASTMake2(AST_GCC_CONDITION, NULL,
			ASTMake4(AST_GCC_CONDITION_DECL, $1, $2, $3, $5, ASTLine($1), NULL),
			ASTLine($1), NULL);
}
;

iteration_statement : WHILE '(' condition ')' statement
{
	$$ = ASTMake2(AST_WHILE_STATEMENT, $3, $5, $1.token_line, NULL);
}
| DO statement WHILE '(' expression ')' ';'
{
	$$ = ASTMake2(AST_DO_STATEMENT, $2, $5, $1.token_line, NULL);
}
| FOR '(' for_init_statement ';' ')' statement
{
	$$ = ASTMake4(AST_FOR_STATEMENT, $3, NULL, NULL, $6, $1.token_line, NULL);
}
| FOR '(' for_init_statement condition ';' ')' statement 
{
	$$ = ASTMake4(AST_FOR_STATEMENT, $3, $4, NULL, $7, $1.token_line, NULL);
}
| FOR '(' for_init_statement ';' expression ')' statement
{
	$$ = ASTMake4(AST_FOR_STATEMENT, $3, NULL, $5, $7, $1.token_line, NULL);
}
| FOR '(' for_init_statement condition ';' expression ')' statement
{
	$$ = ASTMake4(AST_FOR_STATEMENT, $3, $4, $6, $8, $1.token_line, NULL);
}
;

for_init_statement : expression_statement
{
	$$ = $1;
}
| simple_declaration
{
	$$ = $1;
}
;

jump_statement : BREAK ';'
{
	$$ = ASTLeaf(AST_BREAK_STATEMENT, $1.token_line, NULL);
}
| CONTINUE ';'
{
	$$ = ASTLeaf(AST_CONTINUE_STATEMENT, $1.token_line, NULL);
}
| RETURN ';'
{
	$$ = ASTMake1(AST_RETURN_STATEMENT, NULL, $1.token_line, NULL);
}
| RETURN expression ';'
{
	$$ = ASTMake1(AST_RETURN_STATEMENT, $2, $1.token_line, NULL);
}
| GOTO IDENTIFIER ';'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);
	
	$$ = ASTMake1(AST_GOTO_STATEMENT, identifier, $1.token_line, NULL);
}
// GNU Extension
| GOTO '*' expression ';'
{
	$$ = ASTMake1(AST_GCC_GOTO_STATEMENT, $3, $1.token_line, NULL);
}
;

// *********************************************************
// A.10 - Special member functions
// *********************************************************

conversion_function_id : OPERATOR conversion_type_id
{
	$$ = ASTMake1(AST_CONVERSION_FUNCTION_ID, $2, $1.token_line, NULL);
}
;

conversion_type_id : type_specifier_seq 
{
	$$ = ASTMake2(AST_CONVERSION_TYPE_ID, $1, NULL, ASTLine($1), NULL);
}
| type_specifier_seq conversion_declarator
{
	$$ = ASTMake2(AST_CONVERSION_TYPE_ID, $1, $2, ASTLine($1), NULL);
}
;

conversion_declarator : ptr_operator
{
	$$ = ASTMake2(AST_CONVERSION_DECLARATOR, $1, NULL, ASTLine($1), NULL);
}
| ptr_operator conversion_declarator
{
	$$ = ASTMake2(AST_CONVERSION_DECLARATOR, $1, $2, ASTLine($1), NULL);
}
;

ctor_initializer : ':' mem_initializer_list
{
	$$ = ASTMake1(AST_CTOR_INITIALIZER, $2, $1.token_line, NULL);
}
;

mem_initializer_list : mem_initializer
{
	$$ = ASTListLeaf($1);
}
| mem_initializer_list ',' mem_initializer
{
	$$ = ASTList($1, $3);
}
;

mem_initializer : mem_initializer_id '(' ')'
{
	$$ = ASTMake2(AST_MEM_INITIALIZER, $1, NULL, ASTLine($1), NULL);
}
| mem_initializer_id '(' expression_list ')'
{
	$$ = ASTMake2(AST_MEM_INITIALIZER, $1, $3, ASTLine($1), NULL);
}
// GNU Extensions
| '(' expression_list ')'
{
	$$ = ASTMake1(AST_GCC_MEM_INITIALIZER, $2, $1.token_line, NULL);
}
| '(' ')'
{
	$$ = ASTMake1(AST_GCC_MEM_INITIALIZER, NULL, $1.token_line, NULL);
}
;

mem_initializer_id : IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, NULL, NULL, identifier, $1.token_line, NULL);
}
| nested_name_specifier IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, NULL, $1, identifier, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, global_op, $2, identifier, $1.token_line, NULL);
}
| DOS_DOS_PUNTS IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, global_op, NULL, identifier, $1.token_line, NULL);
}
| template_id
{
	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, NULL, NULL, $1, ASTLine($1), NULL);
}
| nested_name_specifier template_id
{
	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, NULL, $1, $2, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, global_op, $2, $3, $1.token_line, NULL);
}
| DOS_DOS_PUNTS template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_MEM_INITIALIZER_ID, global_op, NULL, $2, $1.token_line, NULL);
}
;

// *********************************************************
// A.4 - Expressions
// *********************************************************

primary_expression : literal
{
	$$ = $1;
}
| THIS
{
	$$ = ASTLeaf(AST_THIS_VARIABLE, $1.token_line, NULL);
}
| '(' expression ')' 
{
	$$ = ASTMake1(AST_PARENTHESIZED_EXPRESSION, $2, $1.token_line, NULL);
}
| id_expression
{
	$$ = $1;
}
// GNU Extensions
| '(' compound_statement ')'
{
	$$ = ASTMake1(AST_GCC_PARENTHESIZED_EXPRESSION, $2, $1.token_line, NULL);
}
| BUILTIN_VA_ARG '(' assignment_expression ',' type_id ')'
{
	$$ = ASTMake2(AST_GCC_BUILTIN_VA_ARG, $3, $5, $1.token_line, NULL);
}
;

id_expression : qualified_id
{
	$$ = $1;
}
| unqualified_id
{
	$$ = $1;
}
;

qualified_id : nested_name_specifier unqualified_id
{
	$$ = ASTMake3(AST_QUALIFIED_ID, NULL, $1, $2, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier unqualified_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_QUALIFIED_ID, global_op, $2, $3, $1.token_line, NULL);
}
| DOS_DOS_PUNTS IDENTIFIER
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake3(AST_QUALIFIED_ID, global_op, NULL, identifier, $1.token_line, NULL);
}
| DOS_DOS_PUNTS template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_QUALIFIED_ID, global_op, NULL, $2, $1.token_line, NULL);
}
| nested_name_specifier TEMPLATE template_id
{
	$$ = ASTMake3(AST_QUALIFIED_TEMPLATE, NULL, $1, $3, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_QUALIFIED_TEMPLATE, global_op, $2, $4, $1.token_line, NULL);
}
| DOS_DOS_PUNTS operator_function_id 
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);
	
	$$ = ASTMake2(AST_QUALIFIED_OPERATOR_FUNCTION_ID, global_op, $2, $1.token_line, NULL);
}
;

nested_name_specifier : class_or_namespace_name DOS_DOS_PUNTS 
{
	$$ = ASTMake2(AST_NESTED_NAME_SPECIFIER, $1, NULL, ASTLine($1), NULL);
}
| class_or_namespace_name DOS_DOS_PUNTS nested_name_specifier
{
	$$ = ASTMake2(AST_NESTED_NAME_SPECIFIER, $1, $3, ASTLine($1), NULL);
}
| class_or_namespace_name DOS_DOS_PUNTS TEMPLATE nested_name_specifier
{
	$$ = ASTMake2(AST_NESTED_NAME_SPECIFIER_TEMPLATE, $1, $4, ASTLine($1), NULL);
}
;

class_or_namespace_name : template_id
{
	$$ = $1;
}
| IDENTIFIER
{
	$$ = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
}
;

unqualified_id : IDENTIFIER
{
	$$ = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);
}
| operator_function_id
{
	$$ = $1;
}
| conversion_function_id
{
	$$ = $1;
}
| '~' IDENTIFIER
{
	// Construct an artificial name for it
	char* c = calloc(sizeof(char), strlen($2.token_text) + 2);
	strcat(c, "~");
	strcat(c, $2.token_text);

	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, c);

	$$ = ASTMake1(AST_DESTRUCTOR_ID, identifier, $1.token_line, NULL);
}
| '~' template_id
{
	AST symbol = ASTSon0($2);

	char* c = calloc(sizeof(char), strlen(ASTText(symbol)) + 2);
	strcat(c, "~");
	strcat(c, ASTText(symbol));

	$$ = ASTMake1(AST_DESTRUCTOR_TEMPLATE_ID, $2, $1.token_line, c);
}
| template_id
{
	$$ = $1;
}
;

postfix_expression : primary_expression
{
	$$ = $1;
}
| postfix_expression '[' expression ']'
{
	$$ = ASTMake2(AST_ARRAY_SUBSCRIPT, $1, $3, ASTLine($1), NULL);
}
| postfix_expression '(' ')' 
{
	$$ = ASTMake2(AST_FUNCTION_CALL, $1, NULL, ASTLine($1), NULL);
}
| postfix_expression '(' expression_list ')' 
{
	$$ = ASTMake2(AST_FUNCTION_CALL, $1, $3, ASTLine($1), NULL);
}
| simple_type_specifier '(' ')' 
{
	$$ = ASTMake2(AST_EXPLICIT_TYPE_CONVERSION, $1, NULL, ASTLine($1), NULL);
}
| simple_type_specifier '(' expression_list ')' 
{
	$$ = ASTMake2(AST_EXPLICIT_TYPE_CONVERSION, $1, $3, ASTLine($1), NULL);
}
| TYPENAME nested_name_specifier IDENTIFIER '(' ')'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_TYPENAME_EXPLICIT_TYPE_CONVERSION, NULL, $2, identifier, NULL, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier IDENTIFIER '(' ')'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake4(AST_TYPENAME_EXPLICIT_TYPE_CONVERSION, global_op, $3, identifier, NULL, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier IDENTIFIER '(' expression_list ')'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_TYPENAME_EXPLICIT_TYPE_CONVERSION, NULL, $2, identifier, $5, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier IDENTIFIER '(' expression_list ')'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);
	AST identifier = ASTLeaf(AST_SYMBOL, $4.token_line, $4.token_text);

	$$ = ASTMake4(AST_TYPENAME_EXPLICIT_TYPE_CONVERSION, global_op, $3, identifier, $6, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier template_id '(' ')'
{
	$$ = ASTMake4(AST_TYPENAME_TEMPLATE, NULL, $2, $3, NULL, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier template_id '(' expression_list ')'
{
	$$ = ASTMake4(AST_TYPENAME_TEMPLATE, NULL, $2, $3, $5, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier TEMPLATE template_id '(' ')'
{
	$$ = ASTMake4(AST_TYPENAME_TEMPLATE_TEMPLATE, NULL, $2, $4, NULL, $1.token_line, NULL);
}
| TYPENAME nested_name_specifier TEMPLATE template_id '(' expression_list ')'
{
	$$ = ASTMake4(AST_TYPENAME_TEMPLATE_TEMPLATE, NULL, $2, $4, $6, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier template_id '(' ')'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_TYPENAME_TEMPLATE, global_op, $3, $4, NULL, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier template_id '(' expression_list ')'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_TYPENAME_TEMPLATE, global_op, $3, $4, $6, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id '(' ')'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_TYPENAME_TEMPLATE_TEMPLATE, global_op, $3, $5, NULL, $1.token_line, NULL);
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id '(' expression_list ')'
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $2.token_line, NULL);

	$$ = ASTMake4(AST_TYPENAME_TEMPLATE_TEMPLATE, global_op, $3, $5, $7, $1.token_line, NULL);
}
| postfix_expression '.' id_expression
{
	$$ = ASTMake2(AST_CLASS_MEMBER_ACCESS, $1, $3, ASTLine($1), NULL);
}
| postfix_expression '.' TEMPLATE id_expression
{
	$$ = ASTMake2(AST_CLASS_TEMPLATE_MEMBER_ACCESS, $1, $4, ASTLine($1), NULL);
}
| postfix_expression PTR_OP id_expression
{
	$$ = ASTMake2(AST_POINTER_CLASS_MEMBER_ACCESS, $1, $3, ASTLine($1), NULL);
}
| postfix_expression PTR_OP TEMPLATE id_expression
{
	$$ = ASTMake2(AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS, $1, $4, ASTLine($1), NULL);
}
| postfix_expression '.' pseudo_destructor_name
{
	$$ = ASTMake2(AST_PSEUDO_DESTRUCTOR_CALL, $1, $3, ASTLine($1), NULL);
}
| postfix_expression PTR_OP pseudo_destructor_name
{
	$$ = ASTMake2(AST_POINTER_PSEUDO_DESTRUCTOR_CALL, $1, $3, ASTLine($1), NULL);
}
| postfix_expression PLUSPLUS
{
	$$ = ASTMake1(AST_POSTINCREMENT, $1, ASTLine($1), NULL);
}
| postfix_expression MINUSMINUS
{
	$$ = ASTMake1(AST_POSTDECREMENT, $1, ASTLine($1), NULL);
}
| DYNAMIC_CAST '<' type_id '>' '(' expression ')'
{
	$$ = ASTMake2(AST_DYNAMIC_CAST, $3, $6, $1.token_line, NULL);
}
| STATIC_CAST '<' type_id '>' '(' expression ')' 
{
	$$ = ASTMake2(AST_STATIC_CAST, $3, $6, $1.token_line, NULL);
}
| REINTERPRET_CAST '<' type_id '>' '(' expression ')'
{
	$$ = ASTMake2(AST_REINTERPRET_CAST, $3, $6, $1.token_line, NULL);
}
| CONST_CAST '<' type_id '>' '(' expression ')'
{
	$$ = ASTMake2(AST_CONST_CAST, $3, $6, $1.token_line, NULL);
}
| TYPEID '(' expression ')' 
{
	$$ = ASTMake1(AST_TYPEID_EXPR, $3, $1.token_line, NULL);
}
| TYPEID '(' type_id ')' 
{
	$$ = ASTMake1(AST_TYPEID_TYPE, $3, $1.token_line, NULL);
}
// GNU Extensions
| '(' type_id ')' '{' initializer_list '}'
{
	$$ = ASTMake2(AST_GCC_POSTFIX_EXPRESSION, $2, $5, $1.token_line, NULL);
}
| '(' type_id ')' '{' initializer_list ',' '}'
{
	$$ = ASTMake2(AST_GCC_POSTFIX_EXPRESSION, $2, $5, $1.token_line, NULL);
}
;

expression_list : assignment_expression 
{
	$$ = ASTListLeaf($1);
}
| expression_list ',' assignment_expression
{
	$$ = ASTList($1, $3);
}
;

pseudo_destructor_name : type_name DOS_DOS_PUNTS '~' type_name
{
	$$ = ASTMake4(AST_PSEUDO_DESTRUCTOR_QUALIF, NULL, NULL, $1, $4, ASTLine($1), NULL);
}
| nested_name_specifier type_name DOS_DOS_PUNTS '~' type_name
{
	$$ = ASTMake4(AST_PSEUDO_DESTRUCTOR_QUALIF, NULL, $1, $2, $5, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS type_name DOS_DOS_PUNTS '~' type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_PSEUDO_DESTRUCTOR_QUALIF, global_op, NULL, $2, $5, $1.token_line, NULL);
}
| DOS_DOS_PUNTS nested_name_specifier type_name DOS_DOS_PUNTS '~' type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_PSEUDO_DESTRUCTOR_QUALIF, global_op, $2, $3, $6, $1.token_line, NULL);
}
| nested_name_specifier TEMPLATE template_id DOS_DOS_PUNTS '~' type_name
{
	$$ = ASTMake4(AST_PSEUDO_DESTRUCTOR_TEMPLATE, NULL, $1, $3, $6, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id DOS_DOS_PUNTS '~' type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_PSEUDO_DESTRUCTOR_TEMPLATE, global_op, $2, $4, $7, $1.token_line, NULL);
}
| '~' type_name
{
	$$ = ASTMake3(AST_PSEUDO_DESTRUCTOR_NAME, NULL, NULL, $2, $1.token_line, NULL);
}
| nested_name_specifier '~' type_name
{
	$$ = ASTMake3(AST_PSEUDO_DESTRUCTOR_NAME, NULL, $1, $3, ASTLine($1), NULL);
}
| DOS_DOS_PUNTS nested_name_specifier '~' type_name
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake3(AST_PSEUDO_DESTRUCTOR_NAME, global_op, $2, $4, $1.token_line, NULL);
}
;

unary_expression : postfix_expression
{
	$$ = $1;
}
| PLUSPLUS cast_expression
{
	$$ = ASTMake1(AST_PREINCREMENT, $2, $1.token_line, NULL);
}
| MINUSMINUS cast_expression
{
	$$ = ASTMake1(AST_PREDECREMENT, $2, $1.token_line, NULL);
}
| unary_operator cast_expression
{
	$$ = ASTMake1($1, $2, ASTLine($2), NULL);
}
| SIZEOF unary_expression  
{
	$$ = ASTMake1(AST_SIZEOF, $2, $1.token_line, NULL);
}
| SIZEOF '(' type_id ')' 
{
	$$ = ASTMake1(AST_SIZEOF_TYPEID, $3, $1.token_line, NULL);
}
| new_expression
{
	$$ = $1;
}
| delete_expression
{
	$$ = $1;
}
// GNU Extensions
| EXTENSION cast_expression
{
	$$ = ASTMake1(AST_GCC_EXTENSION_EXPR, $2, $1.token_line, $1.token_text);
}
| ALIGNOF unary_expression
{
	$$ = ASTMake1(AST_GCC_ALIGNOF, $2, $1.token_line, $1.token_text);
}
| ALIGNOF '(' type_id ')'
{
	$$ = ASTMake1(AST_GCC_ALIGNOF_TYPE, $3, $1.token_line, $1.token_text);
}
| REAL cast_expression
{
	$$ = ASTMake1(AST_GCC_REAL_PART, $2, $1.token_line, NULL);
}
| IMAG cast_expression
{
	$$ = ASTMake1(AST_GCC_IMAG_PART, $2, $1.token_line, NULL);
}
| ANDAND IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake1(AST_GCC_LABEL_ADDR, identifier, $1.token_line, NULL);
}
;

unary_operator : '*'
{
	$$ = AST_DERREFERENCE;
}
| '&' 
{
	$$ = AST_REFERENCE;
}
| '+'
{
	$$ = AST_PLUS_OP;
}
| '-'
{
	$$ = AST_NEG_OP;
}
| '!'
{
	$$ = AST_NOT_OP;
}
| '~'
{
	$$ = AST_COMPLEMENT_OP;
}
;

new_expression : TOKEN_NEW new_type_id 
{
	$$ = ASTMake4(AST_NEW_EXPRESSION, NULL, NULL, $2, NULL, $1.token_line, NULL);
}
| TOKEN_NEW new_type_id new_initializer
{
	$$ = ASTMake4(AST_NEW_EXPRESSION, NULL, NULL, $2, $3, $1.token_line, NULL);
}
| TOKEN_NEW new_placement new_type_id 
{
	$$ = ASTMake4(AST_NEW_EXPRESSION, NULL, $2, $3, NULL, $1.token_line, NULL);
}
| TOKEN_NEW new_placement new_type_id new_initializer
{
	$$ = ASTMake4(AST_NEW_EXPRESSION, NULL, $2, $3, $4, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW new_type_id 
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_EXPRESSION, global_op, NULL, NULL, $3, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW new_type_id new_initializer
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_EXPRESSION, global_op, NULL, $3, $4, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW new_placement new_type_id 
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_EXPRESSION, global_op, $3, $4, NULL, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW new_placement new_type_id new_initializer
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_EXPRESSION, global_op, $3, $4, $5, $1.token_line, NULL);
}
| TOKEN_NEW '(' type_id ')' 
{
	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, NULL, NULL, $3, NULL, $1.token_line, NULL);
}
| TOKEN_NEW '(' type_id ')' new_initializer
{
	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, NULL, NULL, $3, $5, $1.token_line, NULL);
}
| TOKEN_NEW new_placement '(' type_id ')' 
{
	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, NULL, $2, $4, NULL, $1.token_line, NULL);
}
| TOKEN_NEW new_placement '(' type_id ')' new_initializer
{
	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, NULL, $2, $4, $6, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW '(' type_id ')' 
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, global_op, NULL, $4, NULL, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW '(' type_id ')' new_initializer
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, global_op, NULL, $4, $6, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW new_placement '(' type_id ')' 
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, global_op, $3, $5, NULL, $1.token_line, NULL);
}
| DOS_DOS_PUNTS TOKEN_NEW new_placement '(' type_id ')' new_initializer
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake4(AST_NEW_TYPE_ID_EXPR, global_op, $3, $5, $7, $1.token_line, NULL);
}
;

new_placement : '(' expression_list ')'
{
	$$ = ASTMake1(AST_NEW_PLACEMENT, $2, $1.token_line, NULL);
}
;

new_type_id : type_specifier_seq
{
	$$ = ASTMake2(AST_NEW_TYPE_ID, $1, NULL, ASTLine($1), NULL);
}
| type_specifier_seq new_declarator
{
	$$ = ASTMake2(AST_NEW_TYPE_ID, $1, $2, ASTLine($1), NULL);
}
;

new_declarator : ptr_operator 
{
	$$ = ASTMake2(AST_NEW_DECLARATOR, $1, NULL, ASTLine($1), NULL);
}
| ptr_operator new_declarator
{
	$$ = ASTMake2(AST_NEW_DECLARATOR, $1, $2, ASTLine($1), NULL);
}
| direct_new_declarator
{
	$$ = $1;
}
;

direct_new_declarator : '[' expression ']'
{
	$$ = ASTMake2(AST_DIRECT_NEW_DECLARATOR, NULL, $2, $1.token_line, NULL);
}
| direct_new_declarator '[' constant_expression ']'
{
	$$ = ASTMake2(AST_DIRECT_NEW_DECLARATOR, $1, $3, ASTLine($1), NULL);
}
;

new_initializer : '(' ')'
{
	$$ = ASTMake1(AST_NEW_INITIALIZER, NULL, $1.token_line, NULL);
}
| '(' expression_list ')'
{
	$$ = ASTMake1(AST_NEW_INITIALIZER, $2, $1.token_line, NULL);
}
;

delete_expression : DELETE cast_expression 
{
	$$ = ASTMake2(AST_DELETE_EXPR, NULL, $2, $1.token_line, NULL);
}
| DELETE '[' ']' cast_expression
{
	$$ = ASTMake2(AST_DELETE_ARRAY_EXPR, NULL, $4, $1.token_line, NULL);
}
| DOS_DOS_PUNTS DELETE cast_expression 
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake2(AST_DELETE_EXPR, global_op, $3, $1.token_line, NULL);
}
| DOS_DOS_PUNTS DELETE '[' ']' cast_expression
{
	AST global_op = ASTLeaf(AST_GLOBAL_SCOPE, $1.token_line, NULL);

	$$ = ASTMake2(AST_DELETE_ARRAY_EXPR, global_op, $5, $1.token_line, NULL);
}
;

cast_expression : unary_expression
{
	$$ = $1;
}
| '(' type_id ')' cast_expression
{
	$$ = ASTMake2(AST_CAST_EXPRESSION, $2, $4, $1.token_line, NULL);
}
;

// pm stands for "pointer to member"
pm_expression : cast_expression
{
	$$ = $1;
}
| pm_expression PTR_OP_MUL cast_expression
{
	$$ = ASTMake2(AST_POINTER_TO_POINTER_MEMBER, $1, $3, ASTLine($1), NULL);
}
| pm_expression '.' '*' cast_expression
{
	$$ = ASTMake2(AST_POINTER_TO_MEMBER, $1, $4, ASTLine($1), NULL);
}
;

multiplicative_expression : pm_expression
{
	$$ = $1;
}
| multiplicative_expression '*' pm_expression
{
	$$ = ASTMake2(AST_MULT_OP, $1, $3, ASTLine($1), NULL);
}
| multiplicative_expression '/' pm_expression
{
	$$ = ASTMake2(AST_DIV_OP, $1, $3, ASTLine($1), NULL);
}
| multiplicative_expression '%' pm_expression
{
	$$ = ASTMake2(AST_MOD_OP, $1, $3, ASTLine($1), NULL);
}
;

additive_expression : multiplicative_expression
{
	$$ = $1;
}
| additive_expression '+' multiplicative_expression
{
	$$ = ASTMake2(AST_ADD_OP, $1, $3, ASTLine($1), NULL);
}
| additive_expression '-' multiplicative_expression
{
	$$ = ASTMake2(AST_MINUS_OP, $1, $3, ASTLine($1), NULL);
}
;

shift_expression : additive_expression
{
	$$ = $1;
}
| shift_expression LEFT additive_expression
{
	$$ = ASTMake2(AST_SHL_OP, $1, $3, ASTLine($1), NULL);
}
| shift_expression RIGHT additive_expression
{
	$$ = ASTMake2(AST_SHR_OP, $1, $3, ASTLine($1), NULL);
}
;

relational_expression : shift_expression
{
	$$ = $1;
}
| relational_expression '<' shift_expression
{
	$$ = ASTMake2(AST_LOWER_THAN, $1, $3, ASTLine($1), NULL);
}
| relational_expression '>' shift_expression
{
	$$ = ASTMake2(AST_GREATER_THAN, $1, $3, ASTLine($1), NULL);
}
| relational_expression GREATER_OR_EQUAL shift_expression
{
	$$ = ASTMake2(AST_GREATER_OR_EQUAL_THAN, $1, $3, ASTLine($1), NULL);
}
| relational_expression LESS_OR_EQUAL shift_expression
{
	$$ = ASTMake2(AST_LOWER_OR_EQUAL_THAN, $1, $3, ASTLine($1), NULL);
}
// GNU Extension
| relational_expression MAX_OPERATOR shift_expression
{
	$$ = ASTMake2(AST_GCC_MAX_OPERATION, $1, $3, ASTLine($1), NULL);
}
| relational_expression MIN_OPERATOR shift_expression
{
	$$ = ASTMake2(AST_GCC_MIN_OPERATION, $1, $3, ASTLine($1), NULL);
}
;

equality_expression : relational_expression
{
	$$ = $1;
}
| equality_expression EQUAL relational_expression
{
	$$ = ASTMake2(AST_EQUAL_OP, $1, $3, ASTLine($1), NULL);
}
| equality_expression NOT_EQUAL relational_expression
{
	$$ = ASTMake2(AST_DIFFERENT_OP, $1, $3, ASTLine($1), NULL);
}
;

and_expression : equality_expression
{
	$$ = $1;
}
| and_expression '&' equality_expression
{
	$$ = ASTMake2(AST_BITWISE_AND, $1, $3, ASTLine($1), NULL);
}
;

exclusive_or_expression : and_expression
{
	$$ = $1;
}
| exclusive_or_expression '^' and_expression
{
	$$ = ASTMake2(AST_BITWISE_XOR, $1, $3, ASTLine($1), NULL);
}
;

inclusive_or_expression : exclusive_or_expression
{
	$$ = $1;
}
| inclusive_or_expression '|' exclusive_or_expression
{
	$$ = ASTMake2(AST_BITWISE_OR, $1, $3, ASTLine($1), NULL);
}
;

logical_and_expression : inclusive_or_expression
{
	$$ = $1;
}
| logical_and_expression ANDAND inclusive_or_expression
{
	$$ = ASTMake2(AST_LOGICAL_AND, $1, $3, ASTLine($1), NULL);
}
;

logical_or_expression : logical_and_expression
{
	$$ = $1;
}
| logical_or_expression OROR logical_and_expression
{
	$$ = ASTMake2(AST_LOGICAL_OR, $1, $3, ASTLine($1), NULL);
}
;

conditional_expression : logical_or_expression
{
	$$ = $1;
}
| logical_or_expression '?' expression ':' assignment_expression
{
	$$ = ASTMake3(AST_CONDITIONAL_EXPRESSION, $1, $3, $5, ASTLine($1), NULL);
}
// GNU Extension
| logical_or_expression '?' ':' assignment_expression
{
	$$ = ASTMake2(AST_GCC_CONDITIONAL_EXPRESSION, $1, $4, ASTLine($1), NULL);
}
;

assignment_expression : conditional_expression
{
	$$ = $1;
}
| logical_or_expression assignment_operator assignment_expression
{
	$$ = ASTMake2($2, $1, $3, ASTLine($1), NULL);
}
| throw_expression
{
	$$ = $1;
}
;

expression : assignment_expression
{
	$$ = ASTMake1(AST_EXPRESSION, $1, ASTLine($1), NULL);
}
| expression ',' assignment_expression
{
	AST comma_expression = ASTMake2(AST_COMMA_OP, $1, $3, ASTLine($1), NULL);

	$$ = ASTMake1(AST_EXPRESSION, comma_expression, ASTLine(comma_expression), NULL);
}
;

assignment_operator : '='
{
	$$ = AST_ASSIGNMENT;
}
| MUL_ASSIGN
{
	$$ = AST_MUL_ASSIGNMENT;
}
| DIV_ASSIGN
{
	$$ = AST_DIV_ASSIGNMENT;
}
| ADD_ASSIGN
{
	$$ = AST_ADD_ASSIGNMENT;
}
| SUB_ASSIGN
{
	$$ = AST_SUB_ASSIGNMENT;
}
| LEFT_ASSIGN
{
	$$ = AST_SHL_ASSIGNMENT;
}
| RIGHT_ASSIGN
{
	$$ = AST_SHR_ASSIGNMENT;
}
| AND_ASSIGN
{
	$$ = AST_AND_ASSIGNMENT;
}
| OR_ASSIGN
{
	$$ = AST_OR_ASSIGNMENT;
}
| XOR_ASSIGN
{
	$$ = AST_XOR_ASSIGNMENT;
}
| MOD_ASSIGN
{
	$$ = AST_MOD_ASSIGNMENT;
}
// GNU Extensions
| MIN_OPERATOR_ASSIGN
{
	$$ = AST_GCC_MIN_ASSIGMENT;
}
| MAX_OPERATOR_ASSIGN 
{
	$$ = AST_GCC_MAX_ASSIGMENT;
}
;

constant_expression : conditional_expression
{
	$$ = ASTMake1(AST_CONSTANT_EXPRESSION, $1, ASTLine($1), NULL);
}
;

// *********************************************************
// A.2 - Lexical conventions
// *********************************************************

literal : DECIMAL_LITERAL
{
	$$ = ASTLeaf(AST_DECIMAL_LITERAL, $1.token_line, $1.token_text);
}
| OCTAL_LITERAL
{
	$$ = ASTLeaf(AST_OCTAL_LITERAL, $1.token_line, $1.token_text);
}
| HEXADECIMAL_LITERAL
{
	$$ = ASTLeaf(AST_HEXADECIMAL_LITERAL, $1.token_line, $1.token_text);
}
| HEXADECIMAL_FLOAT
{
    $$ = ASTLeaf(AST_HEXADECIMAL_FLOAT, $1.token_line, $1.token_text);
}
| FLOATING_LITERAL
{
	$$ = ASTLeaf(AST_FLOATING_LITERAL, $1.token_line, $1.token_text);
}
| BOOLEAN_LITERAL
{
	$$ = ASTLeaf(AST_BOOLEAN_LITERAL, $1.token_line, $1.token_text);
}
| CHARACTER_LITERAL
{
	$$ = ASTLeaf(AST_CHARACTER_LITERAL, $1.token_line, $1.token_text);
}
| string_literal
{
	$$ = $1;
}
;

// *********************************************************
// A.11 - Overloading
// *********************************************************

operator_function_id : OPERATOR operator
{
	$$ = ASTMake1(AST_OPERATOR_FUNCTION_ID, $2, $1.token_line, NULL);
}
| OPERATOR operator '<' template_argument_list '>'
{
	$$ = ASTMake2(AST_OPERATOR_FUNCTION_ID_TEMPLATE, $2, $4, $1.token_line, NULL);
}
| OPERATOR operator '<' '>'
{
	$$ = ASTMake2(AST_OPERATOR_FUNCTION_ID_TEMPLATE, $2, NULL, $1.token_line, NULL);
}
;

operator : TOKEN_NEW
{
	$$ = ASTLeaf(AST_NEW_OPERATOR, $1.token_line, NULL);
}
| DELETE
{
	$$ = ASTLeaf(AST_DELETE_OPERATOR, $1.token_line, NULL);
}
| TOKEN_NEW '[' ']'
{
	$$ = ASTLeaf(AST_NEW_ARRAY_OPERATOR, $1.token_line, NULL);
}
| DELETE '[' ']'
{
	$$ = ASTLeaf(AST_DELETE_ARRAY_OPERATOR, $1.token_line, NULL);
}
| '+'
{
	$$ = ASTLeaf(AST_ADD_OPERATOR, $1.token_line, NULL);
}
| '-'
{
	$$ = ASTLeaf(AST_MINUS_OPERATOR, $1.token_line, NULL);
}
| '*'
{
	$$ = ASTLeaf(AST_MULT_OPERATOR, $1.token_line, NULL);
}
| '/'
{
	$$ = ASTLeaf(AST_DIV_OPERATOR, $1.token_line, NULL);
}
| '%' 
{
	$$ = ASTLeaf(AST_MOD_OPERATOR, $1.token_line, NULL);
}
| '^'
{
	$$ = ASTLeaf(AST_BITWISE_XOR_OPERATOR, $1.token_line, NULL);
}
| '&'
{
	$$ = ASTLeaf(AST_BITWISE_AND_OPERATOR, $1.token_line, NULL);
}
| '|'
{
	$$ = ASTLeaf(AST_BITWISE_OR_OPERATOR, $1.token_line, NULL);
}
| '~'
{
	$$ = ASTLeaf(AST_BITWISE_NEG_OPERATOR, $1.token_line, NULL);
}
| '!'
{
	$$ = ASTLeaf(AST_LOGICAL_NOT_OPERATOR, $1.token_line, NULL);
}
| '='
{
	$$ = ASTLeaf(AST_ASSIGNMENT_OPERATOR, $1.token_line, NULL);
}
| '<'
{
	$$ = ASTLeaf(AST_LOWER_OPERATOR, $1.token_line, NULL);
}
| '>'
{
	$$ = ASTLeaf(AST_GREATER_OPERATOR, $1.token_line, NULL);
}
| ADD_ASSIGN
{
	$$ = ASTLeaf(AST_ADD_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| SUB_ASSIGN
{
	$$ = ASTLeaf(AST_SUB_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| MUL_ASSIGN
{
	$$ = ASTLeaf(AST_MUL_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| DIV_ASSIGN
{
	$$ = ASTLeaf(AST_DIV_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| MOD_ASSIGN
{
	$$ = ASTLeaf(AST_MOD_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| XOR_ASSIGN
{
	$$ = ASTLeaf(AST_XOR_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| AND_ASSIGN
{
	$$ = ASTLeaf(AST_AND_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| OR_ASSIGN
{
	$$ = ASTLeaf(AST_OR_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| LEFT
{
	$$ = ASTLeaf(AST_LEFT_OPERATOR, $1.token_line, NULL);
}
| RIGHT
{
	$$ = ASTLeaf(AST_RIGHT_OPERATOR, $1.token_line, NULL);
}
| LEFT_ASSIGN
{
	$$ = ASTLeaf(AST_LEFT_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| RIGHT_ASSIGN
{
	$$ = ASTLeaf(AST_RIGHT_ASSIGN_OPERATOR, $1.token_line, NULL);
}
| EQUAL
{
	$$ = ASTLeaf(AST_EQUAL_OPERATOR, $1.token_line, NULL);
}
| NOT_EQUAL
{
	$$ = ASTLeaf(AST_DIFFERENT_OPERATOR, $1.token_line, NULL);
}
| LESS_OR_EQUAL
{
	$$ = ASTLeaf(AST_LESS_OR_EQUAL_OPERATOR, $1.token_line, NULL);
}
| GREATER_OR_EQUAL
{
	$$ = ASTLeaf(AST_GREATER_OR_EQUAL_OPERATOR, $1.token_line, NULL);
}
| ANDAND
{
	$$ = ASTLeaf(AST_LOGICAL_AND_OPERATOR, $1.token_line, NULL);
}
| OROR
{
	$$ = ASTLeaf(AST_LOGICAL_OR_OPERATOR, $1.token_line, NULL);
}
| PLUSPLUS
{
	$$ = ASTLeaf(AST_INCREMENT_OPERATOR, $1.token_line, NULL);
}
| MINUSMINUS
{
	$$ = ASTLeaf(AST_DECREMENT_OPERATOR, $1.token_line, NULL);
}
| ','
{
	$$ = ASTLeaf(AST_COMMA_OPERATOR, $1.token_line, NULL);
}
| PTR_OP
{
	$$ = ASTLeaf(AST_POINTER_OPERATOR, $1.token_line, NULL);
}
| PTR_OP_MUL
{
	$$ = ASTLeaf(AST_POINTER_DERREF_OPERATOR, $1.token_line, NULL);
}
| '(' ')'
{
	$$ = ASTLeaf(AST_FUNCTION_CALL_OPERATOR, $1.token_line, NULL);
}
| '[' ']'
{
	$$ = ASTLeaf(AST_SUBSCRIPT_OPERATOR, $1.token_line, NULL);
}
;

// *********************************************************
// A.12 - Templates
// *********************************************************

template_id : IDENTIFIER '<' template_argument_list '>'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake2(AST_TEMPLATE_ID, identifier, $3, $1.token_line, NULL);
}
| IDENTIFIER '<' '>'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text);

	$$ = ASTMake2(AST_TEMPLATE_ID, identifier, NULL, $1.token_line, NULL);
}
;

template_argument_list : template_argument
{
	$$ = ASTListLeaf($1);
}
| template_argument_list ',' template_argument
{
	$$ = ASTList($1, $3);
}
;

template_argument : template_assignment_expression 
{
	$$ = ASTMake1(AST_TEMPLATE_EXPRESSION_ARGUMENT, 
			ASTMake1(AST_EXPRESSION, $1, ASTLine($1), NULL), 
			ASTLine($1), NULL);
}
| type_id 
{
	$$ = ASTMake1(AST_TEMPLATE_TYPE_ARGUMENT, $1, ASTLine($1), NULL);
}
// A template_assignment_expression can generate one of these
// | id_expression 
// {
// 	$$ = $1;
// }
;

template_declaration : TEMPLATE '<' template_parameter_list '>' templated_declaration
{
	$$ = ASTMake2(AST_TEMPLATE_DECLARATION, $3, $5, $1.token_line, NULL);
}
| EXPORT TEMPLATE '<' template_parameter_list '>' templated_declaration
{
	$$ = ASTMake2(AST_EXPORT_TEMPLATE_DECLARATION, $4, $6, $1.token_line, NULL);
}
;

template_parameter_list : template_parameter
{
	$$ = ASTListLeaf($1);
}
| template_parameter_list ',' template_parameter
{
	$$ = ASTList($1, $3);
}
;

template_parameter : type_parameter 
{
	$$ = $1;
}
| parameter_declaration 
{
	$$ = $1;
}
;

type_parameter : CLASS
{
	$$ = ASTMake2(AST_TYPE_PARAMETER_CLASS, NULL, NULL, $1.token_line, NULL);
}
| CLASS IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);
	
	$$ = ASTMake2(AST_TYPE_PARAMETER_CLASS, identifier, NULL, $1.token_line, NULL);
}
| CLASS '=' type_id
{
	$$ = ASTMake2(AST_TYPE_PARAMETER_CLASS, NULL, $3, $1.token_line, NULL);
}
| CLASS IDENTIFIER '=' type_id
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_TYPE_PARAMETER_CLASS, identifier, $4, $1.token_line, NULL);
}
| TYPENAME 
{
	$$ = ASTMake2(AST_TYPE_PARAMETER_TYPENAME, NULL, NULL, $1.token_line, NULL);
}
| TYPENAME IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_TYPE_PARAMETER_TYPENAME, identifier, NULL, $1.token_line, NULL);
}
| TYPENAME '=' type_id
{
	$$ = ASTMake2(AST_TYPE_PARAMETER_TYPENAME, NULL, $3, $1.token_line, NULL);
}
| TYPENAME IDENTIFIER '=' type_id
{
	AST identifier = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake2(AST_TYPE_PARAMETER_TYPENAME, identifier, $4, $1.token_line, NULL);
}
| TEMPLATE '<' template_parameter_list '>' CLASS
{
	$$ = ASTMake3(AST_TYPE_PARAMETER_TEMPLATE, $3, NULL, NULL, $1.token_line, NULL);
}
| TEMPLATE '<' template_parameter_list '>' CLASS IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $6.token_line, $6.token_text);

	$$ = ASTMake3(AST_TYPE_PARAMETER_TEMPLATE, $3, identifier, NULL, $1.token_line, NULL);
}
| TEMPLATE '<' template_parameter_list '>' CLASS '=' id_expression
{
	$$ = ASTMake3(AST_TYPE_PARAMETER_TEMPLATE, $3, NULL, $7, $1.token_line, NULL);
}
| TEMPLATE '<' template_parameter_list '>' CLASS IDENTIFIER '=' id_expression
{
	AST identifier = ASTLeaf(AST_SYMBOL, $6.token_line, $6.token_text);

	$$ = ASTMake3(AST_TYPE_PARAMETER_TEMPLATE, $3, identifier, $8, $1.token_line, NULL);
}
;

explicit_instantiation : TEMPLATE decl_specifier_seq declarator ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, NULL, $2, $3, $1.token_line, NULL);
}
| TEMPLATE decl_specifier_seq ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, NULL, $2, NULL, $1.token_line, NULL);
}
| TEMPLATE declarator ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, NULL, NULL, $2, $1.token_line, NULL);
}
// GNU Extensions
| storage_class_specifier TEMPLATE decl_specifier_seq declarator ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, $1, $3, $4, ASTLine($1), NULL);
}
| storage_class_specifier TEMPLATE decl_specifier_seq ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, $1, $3, NULL, ASTLine($1), NULL);
}
| storage_class_specifier TEMPLATE declarator ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, $1, NULL, $3, ASTLine($1), NULL);
}
| function_specifier TEMPLATE decl_specifier_seq declarator ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, $1, $3, $4, ASTLine($1), NULL);
}
| function_specifier TEMPLATE decl_specifier_seq ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, $1, $3, NULL, ASTLine($1), NULL);
}
| function_specifier TEMPLATE declarator ';'
{
	$$ = ASTMake3(AST_EXPLICIT_INSTANTIATION, $1, NULL, $3, ASTLine($1), NULL);
}
;

explicit_specialization : TEMPLATE '<' '>' templated_declaration
{
	$$ = ASTMake1(AST_EXPLICIT_SPECIALIZATION, $4, $1.token_line, NULL);
}
| TEMPLATE '<' '>' explicit_specialization
{
	$$ = ASTMake1(AST_EXPLICIT_SPECIALIZATION, $4, $1.token_line, NULL);
}
| TEMPLATE '<' '>' template_declaration
{
	$$ = ASTMake1(AST_EXPLICIT_SPECIALIZATION, $4, $1.token_line, NULL);
}
;


// These are the declarations that can follow a template preffix
// We give them known forms to ease semantic analysis
templated_declaration : decl_specifier_seq init_declarator ';'
{
	// Crafted form of simple declaration
	$$ = ASTMake2(AST_SIMPLE_DECLARATION, $1, ASTListLeaf($2), ASTLine($1), NULL);
}
| init_declarator ';'
{
	$$ = ASTMake2(AST_SIMPLE_DECLARATION, NULL, ASTListLeaf($1), ASTLine($1), NULL);
}
| decl_specifier_seq ';'
{
	$$ = ASTMake2(AST_SIMPLE_DECLARATION, $1, NULL, ASTLine($1), NULL);
}
| function_definition
{
	$$ = $1;
}
| template_declaration
{
	$$ = $1;
}
;

// *********************************************************
// A.12.1 - Template Expressions
// *********************************************************
// Les expressions tenen restriccions amb ">" al nivell mes extern

template_shift_expression : additive_expression
{
	$$ = $1;
}
| template_shift_expression LEFT additive_expression
{
	$$ = ASTMake2(AST_SHL_OP, $1, $3, ASTLine($1), NULL);
}
// AQUI NO
// | template_shift_expression RIGHT additive_expression
// {
// 	$$ = ASTMake2(AST_SHR_OP, $1, $3, ASTLine($1), NULL);
// }
;

template_relational_expression : template_shift_expression
{
	$$ = $1;
}
| template_relational_expression '<' template_shift_expression
{
	$$ = ASTMake2(AST_LOWER_THAN, $1, $3, ASTLine($1), NULL);
}
// AQUI NO
// | template_relational_expression '>' shift_expression
// {
// }
// AQUI NO
// | template_relational_expression GREATER_OR_EQUAL shift_expression
// {
// }
| template_relational_expression LESS_OR_EQUAL template_shift_expression
{
	$$ = ASTMake2(AST_LOWER_OR_EQUAL_THAN, $1, $3, ASTLine($1), NULL);
}
// GNU Extensions
| template_relational_expression MIN_OPERATOR shift_expression
{
	$$ = ASTMake2(AST_GCC_MIN_OPERATION, $1, $3, ASTLine($1), NULL);
}
;

template_equality_expression : template_relational_expression
{
	$$ = $1;
}
| template_equality_expression EQUAL template_relational_expression
{
	$$ = ASTMake2(AST_EQUAL_OP, $1, $3, ASTLine($1), NULL);
}
| template_equality_expression NOT_EQUAL template_relational_expression
{
	$$ = ASTMake2(AST_DIFFERENT_OP, $1, $3, ASTLine($1), NULL);
}
;

template_and_expression : template_equality_expression
{
	$$ = $1;
}
| template_and_expression '&' template_equality_expression
{
	$$ = ASTMake2(AST_BITWISE_AND, $1, $3, ASTLine($1), NULL);
}
;

template_exclusive_or_expression : template_and_expression
{
	$$ = $1;
}
| template_exclusive_or_expression '^' template_and_expression
{
	$$ = ASTMake2(AST_BITWISE_XOR, $1, $3, ASTLine($1), NULL);
}
;

template_inclusive_or_expression : template_exclusive_or_expression
{
	$$ = $1;
}
| template_inclusive_or_expression '|' template_exclusive_or_expression
{
	$$ = ASTMake2(AST_BITWISE_OR, $1, $3, ASTLine($1), NULL);
}
;

template_logical_and_expression : template_inclusive_or_expression
{
	$$ = $1;
}
| template_logical_and_expression ANDAND template_inclusive_or_expression
{
	$$ = ASTMake2(AST_LOGICAL_AND, $1, $3, ASTLine($1), NULL);
}
;

template_logical_or_expression : template_logical_and_expression
{
	$$ = $1;
}
| template_logical_or_expression OROR template_logical_and_expression
{
	$$ = ASTMake2(AST_LOGICAL_OR, $1, $3, ASTLine($1), NULL);
}
;

template_conditional_expression : template_logical_or_expression
{
	$$ = $1;
}
| template_logical_or_expression '?' template_expression ':' template_assignment_expression
{
	$$ = ASTMake3(AST_CONDITIONAL_EXPRESSION, $1, $3, $5, ASTLine($1), NULL);
}
// GNU Extension
| template_logical_or_expression '?' ':' template_assignment_expression
{
	$$ = ASTMake2(AST_GCC_CONDITIONAL_EXPRESSION, $1, $4, ASTLine($1), NULL);
}
;

template_assignment_expression : template_conditional_expression
{
	$$ = $1;
}
| template_logical_or_expression assignment_operator template_assignment_expression
{
	$$ = ASTMake2($2, $1, $3, ASTLine($1), NULL);
}
// Extremely unlikely
// | template_throw_expression
// {
// }
;

template_expression : template_assignment_expression
{
	$$ = ASTMake1(AST_EXPRESSION, $1, ASTLine($1), NULL);
}
| template_expression ',' template_assignment_expression
{
	AST comma_expression = ASTMake2(AST_COMMA_OP, $1, $3, ASTLine($1), NULL);

	$$ = ASTMake1(AST_EXPRESSION, comma_expression, ASTLine(comma_expression), NULL);
}
;


// *********************************************************
// A.13 - Exception handling
// *********************************************************

try_block : TRY compound_statement handler_seq
{
	$$ = ASTMake2(AST_TRY_BLOCK, $2, $3, $1.token_line, NULL);
}
;

handler_seq : handler
{
	$$ = ASTListLeaf($1);
}
| handler_seq handler
{
	$$ = ASTList($1, $2);
}
;

handler : CATCH '(' exception_declaration ')' compound_statement
{
	$$ = ASTMake2(AST_CATCH_HANDLER, $3, $5, $1.token_line, NULL);
}
;

exception_declaration : type_specifier_seq declarator  
{
	$$ = ASTMake2(AST_EXCEPTION_DECLARATION, $1, $2, ASTLine($1), NULL);
}
| type_specifier_seq abstract_declarator
{
	$$ = ASTMake2(AST_EXCEPTION_DECLARATION, $1, $2, ASTLine($1), NULL);
}
| type_specifier_seq 
{
	$$ = ASTMake2(AST_EXCEPTION_DECLARATION, $1, NULL, ASTLine($1), NULL);
}
| TRES_PUNTS
{
	$$ = ASTLeaf(AST_ANY_EXCEPTION, $1.token_line, NULL);
}
;

exception_specification : THROW '(' ')' 
{
	$$ = ASTMake1(AST_EXCEPTION_SPECIFICATION, NULL, $1.token_line, NULL);
}
| THROW '(' type_id_list ')'
{
	$$ = ASTMake1(AST_EXCEPTION_SPECIFICATION, $3, $1.token_line, NULL);
}
;

type_id_list : type_id 
{
	$$ = ASTListLeaf($1);
}
| type_id_list ',' type_id
{
	$$ = ASTList($1, $3);
}
;

throw_expression : THROW
{
	$$ = ASTMake1(AST_THROW_EXPRESSION, NULL, $1.token_line, NULL);
}
| THROW assignment_expression
{
	$$ = ASTMake1(AST_THROW_EXPRESSION, $2, $1.token_line, NULL);
}
;

unknown_pragma : UNKNOWN_PRAGMA
{
	$$ = ASTLeaf(AST_UNKNOWN_PRAGMA, $1.token_line, $1.token_text);
}

// This eases parsing, though it should be viewed as a lexical issue
string_literal : STRING_LITERAL
{
	$$ = ASTLeaf(AST_STRING_LITERAL, $1.token_line, $1.token_text);
}
| string_literal STRING_LITERAL
{
	// Let's concatenate here, it will ease everything

	char* str1 = ASTText($1);
	char* str2 = $2.token_text;
	char* text = calloc(strlen(str1) + strlen(str2) + 1, sizeof(*text));

	strcat(text, str1);

	// Remove "
	text[strlen(text)-1] = '\0';

	// Jump 'L', if any
	if (*str2 == 'L' || *str2 == 'l') str2++;
	// Jump '"'
	str2++;

	// Append the second string
	strcat(text, str2);

	$$ = ASTLeaf(AST_STRING_LITERAL, ASTLine($1), text);
}
;

// *********************************************************
// OpenMP 2.5
// *********************************************************
openmp_construct : parallel_construct
{
	$$ = $1;
}
| for_construct
{
	$$ = $1;
}
| sections_construct
{
	$$ = $1;
}
| single_construct
{
	$$ = $1;
}
| parallel_for_construct
{
	$$ = $1;
}
| parallel_sections_construct
{
	$$ = $1;
}
| master_construct
{
	$$ = $1;
}
| critical_construct
{
	$$ = $1;
}
| atomic_construct
{
	$$ = $1;
}
| ordered_construct
{
	$$ = $1;
}
// There is a bug in the specification of OpenMP 2.5, a threadprivate directive
// really CAN appear here even if in the grammar spec does not
| threadprivate_directive
{
	$$ = $1;
}
| custom_directive
{
	$$ = $1;
}
| custom_construct
{
	$$ = $1;
}
;

openmp_directive : barrier_directive
{
	$$ = $1;
}
| flush_directive
{
	$$ = $1;
}
;

// Custom OpenMP support
custom_directive : OMP_PRAGMA OMP_DIRECTIVE_TOKEN OMP_CUSTOM_DIRECTIVE custom_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_CUSTOM_DIRECTIVE, $4, $1.token_line, $3.token_text);
}
;

custom_construct : custom_construct_directive structured_block
{
	$$ = ASTMake2(AST_OMP_CUSTOM_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

custom_construct_directive : OMP_PRAGMA OMP_CUSTOM_DIRECTIVE custom_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CONSTRUCT_DIRECTIVE, $3, $1.token_line, $2.token_text);
}
| OMP_PRAGMA OMP_CONSTRUCT_TOKEN OMP_CUSTOM_DIRECTIVE custom_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CONSTRUCT_DIRECTIVE, $4, $1.token_line, $3.token_text);
}
;

custom_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| custom_clause_seq
{
	$$ = $1;
}
;

// I think this is the more general
custom_clause_seq : parallel_for_clause
{
	$$ = ASTListLeaf($1);
}
| custom_parameter_clause
{
	$$ = ASTListLeaf($1);
}
| custom_clause_seq parallel_for_clause
{
	$$ = ASTList($1, $2);
}
;

custom_parameter_clause : '(' expression_list ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_PARAMETER_CLAUSE, $2, $1.token_line, NULL);
}
;

custom_clause : OMP_CUSTOM_CLAUSE '(' expression_list ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, $3, $1.token_line, $1.token_text);
}
| OMP_CUSTOM_CLAUSE '(' ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, NULL, $1.token_line, $1.token_text);
}
;

structured_block : statement
{
	$$ = $1;
}
;

parallel_construct : parallel_directive structured_block
{
	$$ = ASTMake2(AST_OMP_PARALLEL_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

parallel_directive : OMP_PRAGMA OMP_PARALLEL parallel_clause_seq_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_DIRECTIVE, $3, $1.token_line, NULL);
}
;

parallel_clause_seq_opt : /* empty */
{
	$$ = NULL;
}
| parallel_clause_seq
{
	$$ = $1;
}
;

parallel_clause_seq : parallel_clause
{
	$$ = ASTListLeaf($1);
}
| parallel_clause_seq parallel_clause
{
	$$ = ASTList($1, $2);
}
| parallel_clause_seq ',' parallel_clause
{
	$$ = ASTList($1, $3);
}
;

parallel_clause : unique_parallel_clause
{
	$$ = $1;
}
| data_clause
{
	$$ = $1;
}
| custom_clause
{
	$$ = $1;
}
;

unique_parallel_clause : OMP_IF '(' expression ')' 
{
	$$ = ASTMake1(AST_OMP_IF_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_NUM_THREADS '(' expression ')'
{
	$$ = ASTMake1(AST_OMP_NUM_THREADS_CLAUSE, $3, $1.token_line, NULL);
}
;

for_construct : for_directive iteration_statement
{
	$$ = ASTMake2(AST_OMP_FOR_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

for_directive : OMP_PRAGMA OMP_FOR for_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_FOR_DIRECTIVE, $3, $1.token_line, NULL);
}
;

for_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| for_clause_seq
{
	$$ = $1;
}
;

for_clause_seq : for_clause
{
	$$ = ASTListLeaf($1);
}
| for_clause_seq ',' for_clause
{
	$$ = ASTList($1, $3);
}
| for_clause_seq for_clause
{
	$$ = ASTList($1, $2);
}
;

for_clause : unique_for_clause 
{
	$$ = $1;
}
| data_clause
{
	$$ = $1
}
| nowait_clause
{
	$$ = $1;
}
| custom_clause
{
	$$ = $1;
}
;

unique_for_clause : OMP_ORDERED
{
	$$ = ASTLeaf(AST_OMP_ORDERED_CLAUSE, $1.token_line, NULL);
}
| OMP_SCHEDULE '(' schedule_kind ')'
{
	$$ = ASTMake2(AST_OMP_SCHEDULE_CLAUSE, $3, NULL, $1.token_line, NULL);
}
| OMP_SCHEDULE '(' schedule_kind ',' expression ')'
{
	$$ = ASTMake2(AST_OMP_SCHEDULE_CLAUSE, $3, $5, $1.token_line, NULL);
}
;

schedule_kind : OMP_STATIC
{
	$$ = ASTLeaf(AST_OMP_STATIC_SCHEDULE, $1.token_line, NULL);
}
| OMP_DYNAMIC
{
	$$ = ASTLeaf(AST_OMP_DYNAMIC_SCHEDULE, $1.token_line, NULL);
}
| OMP_GUIDED
{
	$$ = ASTLeaf(AST_OMP_GUIDED_SCHEDULE, $1.token_line, NULL);
}
| OMP_RUNTIME
{
	$$ = ASTLeaf(AST_OMP_RUNTIME_SCHEDULE, $1.token_line, NULL);
}
;

sections_construct : sections_directive section_scope
{
	$$ = ASTMake2(AST_OMP_SECTIONS_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

sections_directive : OMP_PRAGMA OMP_SECTIONS sections_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_SECTIONS_DIRECTIVE, $3, $1.token_line, NULL);
}
;

sections_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| sections_clause_seq
{
	$$ = $1;
}
;

sections_clause_seq : sections_clause
{
	$$ = ASTListLeaf($1);
}
| sections_clause_seq sections_clause
{
	$$ = ASTList($1, $2);
}
| sections_clause_seq ',' sections_clause
{
	$$ = ASTList($1, $3);
}
;

sections_clause : data_clause
{
	$$ = $1;
}
| nowait_clause
{
	$$ = $1;
}
| custom_clause
{
}
;

nowait_clause :  OMP_NOWAIT
{
	$$ = ASTLeaf(AST_OMP_NOWAIT_CLAUSE, $1.token_line, NULL);
}
;

section_scope : '{' section_sequence '}'
{
	$$ = $2;
}
;

section_sequence : section_directive structured_block
{
	$$ = ASTListLeaf(
			ASTMake2(AST_OMP_SECTION, $1, $2, ASTLine($1), NULL)
			);
}
| structured_block
{
	$$ = ASTListLeaf(
			ASTMake2(AST_OMP_SECTION, NULL, $1, ASTLine($1), NULL)
			);
}
| section_sequence section_directive structured_block
{
	$$ = ASTList($1, 
			ASTMake2(AST_OMP_SECTION, $2, $3, ASTLine($2), NULL)
			);
}
;

section_directive : OMP_PRAGMA OMP_SECTION OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_SECTION_DIRECTIVE, $1.token_line, NULL);
}
;

single_construct : single_directive structured_block
{
	$$ = ASTMake2(AST_OMP_SINGLE_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

single_directive : OMP_PRAGMA OMP_SINGLE single_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_SINGLE_DIRECTIVE, $3, $1.token_line, NULL);
}
;

single_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| single_clause_seq
{
	$$ = $1;
}
;

single_clause_seq : single_clause
{
	$$ = ASTListLeaf($1);
}
| single_clause_seq ',' single_clause
{
	$$ = ASTList($1, $3);
}
| single_clause_seq single_clause
{
	$$ = ASTList($1, $2);
}
;

single_clause : data_clause
{
	$$ = $1;
}
| nowait_clause
{
	$$ = $1;
}
| custom_clause
{
	$$ = $1;
}
;

parallel_for_construct : parallel_for_directive iteration_statement
{
	$$ = ASTMake2(AST_OMP_PARALLEL_FOR_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

parallel_for_directive : OMP_PRAGMA OMP_PARALLEL_FOR parallel_for_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_FOR_DIRECTIVE, $3, $1.token_line, NULL);
}
;

parallel_for_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| parallel_for_clause_seq 
{
	$$ = $1;
}
;

parallel_for_clause_seq : parallel_for_clause
{
	$$ = ASTListLeaf($1);
}
| parallel_for_clause_seq ',' parallel_for_clause
{
	$$ = ASTList($1, $3);
}
| parallel_for_clause_seq parallel_for_clause
{
	$$ = ASTList($1, $2);
}
;

parallel_for_clause : unique_parallel_clause
{
	$$ = $1;
}
| unique_for_clause
{
	$$ = $1;
}
| data_clause
{
	$$ = $1;
}
| custom_clause
{
	$$ = $1;
}
;

parallel_sections_construct : parallel_sections_directive section_scope
{
	$$ = ASTMake2(AST_OMP_PARALLEL_SECTIONS_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

parallel_sections_directive : OMP_PRAGMA OMP_PARALLEL_SECTIONS parallel_sections_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_SECTIONS_DIRECTIVE, $3, $1.token_line, NULL);
}
;

parallel_sections_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| parallel_sections_clause_seq
{
	$$ = $1;
}
;

parallel_sections_clause_seq : parallel_sections_clause
{
	$$ = ASTListLeaf($1);
}
| parallel_sections_clause_seq ',' parallel_sections_clause
{
	$$ = ASTList($1, $3);
}
| parallel_sections_clause_seq parallel_sections_clause
{
	$$ = ASTList($1, $2);
}
;

parallel_sections_clause : unique_parallel_clause
{
	$$ = $1;
}
| data_clause
{
	$$ = $1;
}
| custom_clause
{
	$$ = $1;
}
;

master_construct : master_directive structured_block
{
	$$ = ASTMake2(AST_OMP_MASTER_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

master_directive : OMP_PRAGMA OMP_MASTER OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_MASTER_DIRECTIVE, $1.token_line, NULL);
}
;

critical_construct : critical_directive structured_block
{
	$$ = ASTMake2(AST_OMP_CRITICAL_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

critical_directive : OMP_PRAGMA OMP_CRITICAL region_phrase_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_CRITICAL_DIRECTIVE, $3, $1.token_line, NULL);
}
;

region_phrase_opt : /* empty */
{
	$$ = NULL;
}
| region_phrase
{
	$$ = $1;
}
;

region_phrase : '(' IDENTIFIER ')'
{
	// Cast it into an expression, makes things a lot easier
	AST critical_region_phrase = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake1(AST_EXPRESSION, critical_region_phrase, $1.token_line, NULL);
}
;

barrier_directive : OMP_PRAGMA OMP_BARRIER OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_BARRIER_DIRECTIVE, $1.token_line, NULL);
}
;

atomic_construct : atomic_directive expression_statement
{
	$$ = ASTMake2(AST_OMP_ATOMIC_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

atomic_directive : OMP_PRAGMA OMP_ATOMIC OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_ATOMIC_DIRECTIVE, $1.token_line, NULL);
}
;

flush_directive : OMP_PRAGMA OMP_FLUSH flush_vars_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_FLUSH_DIRECTIVE, $3, $1.token_line, NULL);
}
;

flush_vars_opt : /* empty */
{
	$$ = NULL;
}
| flush_vars
{
	$$ = $1;
}
;

flush_vars : '(' variable_list ')'
{
	$$ = $2;
}
;

ordered_construct : ordered_directive structured_block
{
	$$ = ASTMake2(AST_OMP_ORDERED_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

ordered_directive : OMP_PRAGMA OMP_ORDERED OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_ORDERED_DIRECTIVE, $1.token_line, NULL);
}
;

threadprivate_directive : OMP_PRAGMA OMP_THREADPRIVATE '(' variable_list ')' OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_THREADPRIVATE_DIRECTIVE, $4, $1.token_line, NULL);
}
;

data_clause : OMP_PRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_PRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_COPYPRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_COPYPRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_FIRSTPRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_FIRSTPRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_LASTPRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_LASTPRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_SHARED '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_SHARED_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_SHARED ')'
{
	$$ = ASTLeaf(AST_OMP_DEFAULT_SHARED_CLAUSE, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_NONE ')'
{
	$$ = ASTLeaf(AST_OMP_DEFAULT_NONE_CLAUSE, $1.token_line, NULL);
}
| OMP_REDUCTION '(' reduction_operator ':' variable_list ')'
{
	$$ = ASTMake2(AST_OMP_REDUCTION_CLAUSE, 
			$3, $5, $1.token_line, NULL);
}
| OMP_COPYIN '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_COPYIN_CLAUSE, $3, $1.token_line, NULL);
}
;

reduction_operator : '+' 
{
	$$ = ASTLeaf(AST_ADD_OPERATOR, $1.token_line, NULL);
}
| '*'
{
	$$ = ASTLeaf(AST_MULT_OPERATOR, $1.token_line, NULL);
}
| '-'
{
	$$ = ASTLeaf(AST_MINUS_OPERATOR, $1.token_line, NULL);
}
| '&'
{
	$$ = ASTLeaf(AST_BITWISE_AND_OPERATOR, $1.token_line, NULL);
}
| '^'
{
	$$ = ASTLeaf(AST_BITWISE_XOR_OPERATOR, $1.token_line, NULL);
}
| '|'
{
	$$ = ASTLeaf(AST_BITWISE_OR_OPERATOR, $1.token_line, NULL);
}
| ANDAND
{
	$$ = ASTLeaf(AST_LOGICAL_AND_OPERATOR, $1.token_line, NULL);
}
| OROR
{
	$$ = ASTLeaf(AST_LOGICAL_OR_OPERATOR, $1.token_line, NULL);
}
;

variable_list : id_expression
{
	$$ = ASTListLeaf($1);
}
| variable_list ',' id_expression
{
	$$ = ASTList($1, $3);
}
;

%%

static AST ambiguityHandler (YYSTYPE x0, YYSTYPE x1)
{
	// return ASTMake2(AST_AMBIGUITY, x0.ast, x1.ast, 0, NULL);
	AST son0 = x0.ast;
	AST son1 = x1.ast;

	if (son0 == son1) 
	{
		fprintf(stderr, "Ambiguity function received two trees that are the same!");
		exit(EXIT_FAILURE);
	}

	if (ASTType(son0) == AST_AMBIGUITY)
	{
		if (ASTType(son1) == AST_AMBIGUITY)
		{
			int original_son0 = son0->num_ambig;

			son0->num_ambig += son1->num_ambig;
			son0->ambig = (AST*) realloc(son0->ambig, sizeof(*(son0->ambig)) * son0->num_ambig);
			
			int i;
			for (i = 0; i < son1->num_ambig; i++)
			{
				son0->ambig[original_son0 + i] = son1->ambig[i];
			}

			return son0;
		}
		else
		{
			son0->num_ambig++;
			son0->ambig = (AST*) realloc(son0->ambig, sizeof(*(son0->ambig)) * son0->num_ambig);
			son0->ambig[son0->num_ambig-1] = duplicate_ast(son1);

			return son0;
		}
	}
	else if (ASTType(son1) == AST_AMBIGUITY)
	{
		son1->num_ambig++;
		son1->ambig = (AST*) realloc(son1->ambig, sizeof(*(son1->ambig)) * son1->num_ambig);
		son1->ambig[son1->num_ambig-1] = duplicate_ast(son0);

		return son1;
	}
	else
	{
		AST result = ASTLeaf(AST_AMBIGUITY, 0, NULL);

		result->num_ambig = 2;
		result->ambig = calloc(sizeof(*(result->ambig)), result->num_ambig);
		// This avoids some problems with bison reusing stacks
		result->ambig[0] = duplicate_ast(son0);
		result->ambig[1] = duplicate_ast(son1);
		result->line = son0->line;
		result->filename = son0->filename;

		return result;
	}
}

void yyerror(AST* parsed_tree, const char* c)
{
	// Current token
	extern char* mcxxtext;
	fprintf(stderr, "%s:%d error : '%s'\n", scanning_now.current_filename, scanning_now.line_number, c);
    fprintf(stderr, "Error near token '%s'\n", mcxxtext);
	exit(EXIT_FAILURE);
}
