%{
/*
   Parser of ISO/IEC 14882 - C++

   Must be compiled with rofi-bison-2.1. 
   Ask it at <rferrer@ac.upc.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "cxx-ast.h"

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
// Sometimes we need lots of memory
#define YYMAXDEPTH (1000000)

void yyerror(const char* c);

extern int yylex();


%}

%glr-parser

%union {
	token_atrib_t token_atrib;
};


%default-merge <ambiguityHandler>

%{
static YYSTYPE ambiguityHandler (YYSTYPE x0, YYSTYPE x1);
%}

%token ADD_ASSIGN
%token ANDAND
%token AND_ASSIGN
%token ASM
%token AUTO
%token BOOL
%token BOOLEAN_LITERAL
%token BREAK
%token CASE
%token CATCH
%token CHAR
%token CHARACTER_LITERAL
%token CLASS
%token CONST
%token CONST_CAST
%token CONTINUE
%token DECIMAL_LITERAL
%token DEFAULT
%token DELETE
%token DIV_ASSIGN
%token DO
%token DOS_DOS_PUNTS
%token DOUBLE
%token DYNAMIC_CAST
%token ELSE
%token ENUM
%token EQUAL
%token EXPLICIT
%token EXPORT
%token EXTERN
%token FLOAT
%token FLOATING_LITERAL
%token FOR
%token FRIEND
%token GOTO
%token GREATER_OR_EQUAL
%token HEXADECIMAL_LITERAL
%token IDENTIFIER
%token IF
%token INLINE
%token INT
%token LEFT
%token LEFT_ASSIGN
%token LESS_OR_EQUAL
%token LONG
%token MINUSMINUS
%token MOD_ASSIGN
%token MUL_ASSIGN
%token MUTABLE
%token NAMESPACE
%token NEW
%token NOT_EQUAL
%token OCTAL_LITERAL
%token OPERATOR
%token OR_ASSIGN
%token OROR
%token PLUSPLUS
%token PRIVATE
%token PROTECTED
%token PTR_OP
%token PTR_OP_MUL
%token PUBLIC
%token REGISTER
%token REINTERPRET_CAST
%token RETURN
%token RIGHT
%token RIGHT_ASSIGN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token STATIC_CAST
%token STRING_LITERAL
%token STRUCT
%token SUB_ASSIGN
%token SWITCH
%token TEMPLATE
%token THIS
%token THROW
%token TRES_PUNTS
%token TRY
%token TYPEDEF
%token TYPEID
%token TYPENAME
%token UNION
%token UNSIGNED
%token USING
%token VIRTUAL
%token VOID
%token VOLATILE
%token WCHAR_T
%token WHILE
%token XOR_ASSIGN

// GNU Extensions
%token BUILTIN_VA_ARG
%token ALIGNOF
%token EXTENSION
%token REAL
%token IMAG
%token LABEL
%token COMPLEX
%token TYPEOF
%token RESTRICT
%token ATTRIBUTE
%token THREAD

%token MAX_OPERATOR
%token MIN_OPERATOR
%token MAX_OPERATOR_ASSIGN
%token MIN_OPERATOR_ASSIGN

// No s'envien mai
%token TODO_TOKEN_1

%start translation_unit

%%

// *********************************************************
// A.3 - Basic concepts
// *********************************************************

translation_unit : declaration_sequence
{
}
| /* empty */
{
}
;

// *********************************************************
// A.6. - Declarations
// *********************************************************

declaration_sequence : declaration
{
}
| declaration_sequence declaration
{
}
;

declaration : block_declaration 
{
}
| function_definition
{
}
| template_declaration
{
}
| explicit_instantiation
{
}
| explicit_specialization
{
}
| linkage_specification
{
}
| namespace_definition
{
}
// GNU Extension
| EXTENSION declaration
{
}
;

linkage_specification : EXTERN string_literal '{' declaration_sequence '}'
{
}
| EXTERN string_literal '{' '}'
{
}
| EXTERN string_literal declaration
{
}
;

namespace_definition : named_namespace_definition
{
}
| unnamed_namespace_definition
{
}
;

// extension_namespace_definition genera exactament el mateix
// que aquesta regla, per aixo més val eliminar-la
named_namespace_definition : NAMESPACE IDENTIFIER '{' declaration_sequence '}'
{
}
| NAMESPACE IDENTIFIER '{' '}'
{
}
;

unnamed_namespace_definition : NAMESPACE '{' declaration_sequence '}'
{
}
| NAMESPACE '{' '}'
{
}
;

block_declaration : simple_declaration
{
}
| asm_definition
{
}
| namespace_alias_definition
{
}
| using_declaration
{
}
| using_directive
{
}
// GNU Extension
| EXTENSION block_declaration
{
}
| label_declaration 
{
}
;

/* GNU Extension */
label_declaration : LABEL label_declarator_seq ';'
{
}
;

label_declarator_seq : IDENTIFIER 
{
}
| label_declarator_seq ',' IDENTIFIER
{
}
;
/* End of GNU extension */

/* GNU Extension */
attributes : attributes attribute
{
}
| attribute
{
}
;

attribute : ATTRIBUTE '(' '(' attribute_list ')' ')'
{
}
| ATTRIBUTE '(''(' ')'')'
{
}
;

attribute_list : attribute
{
}
| attribute_list ',' attribute
{
}
;

attribute : IDENTIFIER
{
}
| IDENTIFIER '(' IDENTIFIER ')'
{
}
| IDENTIFIER '(' IDENTIFIER ',' expression_list ')'
{
}
| IDENTIFIER '(' expression_list ')'
{
}
;
/* End of GNU extension */

asm_definition : ASM '(' string_literal ')' ';'
{
}
// GNU EXtensions
| ASM VOLATILE '(' string_literal ')' ';'
{
}
| ASM '(' string_literal ':' ')' ';'
{
}
| ASM VOLATILE '(' string_literal ':' ')' ';'
{
}
| ASM '(' string_literal ':' asm_operand_list ')' ';'
{
}
| ASM VOLATILE '(' string_literal ':' asm_operand_list ')' ';'
{
}
| ASM '(' string_literal ':' ':' ')' ';'
{
}
| ASM VOLATILE '(' string_literal ':' ':' ')' ';'
{
}
| ASM '(' string_literal ':' asm_operand_list ':' ')' ';'
{
}
| ASM VOLATILE '(' string_literal ':' asm_operand_list ':' ')' ';'
{
}
| ASM '(' string_literal ':' ':' asm_operand_list ')' ';'
{
}
| ASM VOLATILE '(' string_literal ':' ':' asm_operand_list ')' ';'
{
}
| ASM '(' string_literal ':' asm_operand_list ':' asm_operand_list ')' ';'
{
}
| ASM VOLATILE '(' string_literal ':' asm_operand_list ':' asm_operand_list ')' ';'
{
}
;

/* GNU Extensions */
asm_operand_list : asm_operand
{
}
| asm_operand ',' asm_operand
{
}
;

asm_operand : string_literal '(' expression ')' 
{
}
| '[' string_literal ']' string_literal '(' expression ')'
{
}
;
/* End of GNU extensions */

namespace_alias_definition : NAMESPACE IDENTIFIER '=' qualified_namespace_specifier ';'
{
}
;

qualified_namespace_specifier : IDENTIFIER
{
}
| nested_name_specifier IDENTIFIER
{
}
| DOS_DOS_PUNTS IDENTIFIER
{
}
| DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
;

using_declaration : USING nested_name_specifier unqualified_id ';'
{
}
| USING DOS_DOS_PUNTS nested_name_specifier unqualified_id ';'
{
}
| USING TYPENAME nested_name_specifier unqualified_id ';'
{
}
| USING TYPENAME DOS_DOS_PUNTS nested_name_specifier unqualified_id ';'
{
}
| USING DOS_DOS_PUNTS unqualified_id ';'
{
}
;


using_directive : USING NAMESPACE IDENTIFIER ';'
{
}
| USING NAMESPACE DOS_DOS_PUNTS IDENTIFIER ';'
{
}
| USING NAMESPACE nested_name_specifier IDENTIFIER ';'
{
}
| USING NAMESPACE DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
;

/*
   L'estàndard diu

simple_declaration :
  decl_specifier_seq[opt] init_declarator_list[opt]

*/
  
simple_declaration : decl_specifier_seq init_declarator_list ';' 
{
}
| init_declarator_list ';' 
{
}
| decl_specifier_seq ';' 
{
}
| ';'
{
}
;

decl_specifier_seq : decl_specifier
{
}
| decl_specifier_seq decl_specifier_seq
{
}
;

decl_specifier : storage_class_specifier
{
}
| type_specifier
{
}
| function_specifier
{
}
| FRIEND
{
}
| TYPEDEF
{
}
// GNU Extension
| attributes
{
}
;

storage_class_specifier : AUTO 
{
}
| REGISTER
{
}
| STATIC
{
}
| EXTERN
{
}
| MUTABLE
{
}
// GNU Extension
| THREAD
{
}
;

function_specifier : INLINE
{
}
| VIRTUAL
{
}
| EXPLICIT
{
}
;

type_specifier : simple_type_specifier
{
}
| class_specifier
{
}
| enum_specifier
{
}
| elaborated_type_specifier
{
}
| cv_qualifier
{
}
// GNU Extensions
| COMPLEX
{
}
;

type_specifier_seq : type_specifier 
{
}
| type_specifier_seq type_specifier 
{
}
// GNU Extensions
| attributes type_specifier_seq
{
}
;

simple_type_specifier : type_name
{
}
| DOS_DOS_PUNTS type_name
{
}
| nested_name_specifier type_name
{
}
| DOS_DOS_PUNTS nested_name_specifier type_name
{
}
| nested_name_specifier TEMPLATE template_id
{
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
}
| builtin_types
{
}
// GNU Extension
| TYPEOF unary_expression
{
}
| TYPEOF '(' type_id ')'
{
}
;

// Regla simplificada
type_name : IDENTIFIER
{
}
| template_id
{
}
;

builtin_types : CHAR
{
}
| WCHAR_T
{
}
| BOOL
{
}
| SHORT
{
}
| INT
{
}
| LONG
{
}
| SIGNED
{
}
| UNSIGNED
{
}
| FLOAT
{
}
| DOUBLE
{
}
| VOID
{
}
;

elaborated_type_specifier : class_key IDENTIFIER
{
}
| class_key DOS_DOS_PUNTS IDENTIFIER
{
}
| class_key nested_name_specifier IDENTIFIER
{
}
| class_key DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
| class_key TEMPLATE template_id
{
}
| class_key DOS_DOS_PUNTS TEMPLATE template_id
{
}
| class_key nested_name_specifier TEMPLATE template_id
{
}
| class_key DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
}
| class_key template_id
{
}
| class_key DOS_DOS_PUNTS template_id
{
}
| class_key nested_name_specifier template_id
{
}
| class_key DOS_DOS_PUNTS nested_name_specifier template_id
{
}
| ENUM IDENTIFIER
{
}
| ENUM DOS_DOS_PUNTS IDENTIFIER
{
}
| ENUM nested_name_specifier IDENTIFIER
{
}
| ENUM DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
| TYPENAME nested_name_specifier IDENTIFIER
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
| TYPENAME nested_name_specifier template_id
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier template_id
{
}
| TYPENAME nested_name_specifier TEMPLATE template_id
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
}
// GNU Extensions
| class_key attributes DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
| class_key attributes nested_name_specifier IDENTIFIER
{
}
| class_key attributes DOS_DOS_PUNTS IDENTIFIER
{
}
| class_key attributes IDENTIFIER
{
}
| class_key attributes template_id
{
}
| class_key attributes DOS_DOS_PUNTS template_id
{
}
| class_key attributes nested_name_specifier template_id
{
}
| class_key attributes DOS_DOS_PUNTS nested_name_specifier template_id
{
}
| class_key attributes TEMPLATE template_id
{
}
| class_key attributes DOS_DOS_PUNTS TEMPLATE template_id
{
}
| class_key attributes nested_name_specifier TEMPLATE template_id
{
}
| class_key attributes DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id
{
}
| ENUM attributes IDENTIFIER
{
}
| ENUM attributes DOS_DOS_PUNTS IDENTIFIER
{
}
| ENUM attributes nested_name_specifier IDENTIFIER
{
}
| ENUM attributes DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
;



// *********************************************************
// A.7 - Declarators
// *********************************************************

// Això és ambigu ja que
// A b(c) es pot entendre com
//
//  b -> declarator
//  (c) -> initializer;
//    o bé
//  b(c) -> declarator
//
// la distinció es purament semàntica ja que si "c" es un type_name
// llavors estem declarant una funció "b" que retorna un A
// altrament b es un objecte de tipus A construït per valor.
init_declarator_list : init_declarator
{
}
| init_declarator_list ',' init_declarator
{
}
;

init_declarator : declarator 
{
}
| declarator initializer
{
}
// GNU Extensions
| declarator asm_specification 
{
}
| declarator attributes
{
}
| declarator asm_specification attributes
{
}
| declarator initializer
{
}
| declarator asm_specification initializer
{
}
| declarator attributes initializer
{
}
| declarator asm_specification attributes initializer
{
}
;

/* GNU Extension */
asm_specification : ASM '(' string_literal ')'
{
}
;
/* End of GNU Extension */

declarator : direct_declarator
{
}
| ptr_operator declarator
{
}
// GNU Extensions
| attributes direct_declarator
{
}
| attributes ptr_operator declarator
{
}
;

ptr_operator : '*'
{
}
| '*' cv_qualifier_seq
{
}
| nested_name_specifier '*'
{
}
| nested_name_specifier '*' cv_qualifier_seq
{
}
| DOS_DOS_PUNTS nested_name_specifier '*'
{
}
| DOS_DOS_PUNTS nested_name_specifier '*' cv_qualifier_seq
{
}
| '&'
{
}
// GNU Extensions
| '&' cv_qualifier_seq
{
}
;

cv_qualifier_seq : cv_qualifier
{
}
| cv_qualifier_seq cv_qualifier
{
}
;

cv_qualifier : CONST
{
}
| VOLATILE
{
}
// GNU Extension
| RESTRICT
{
}
;

direct_declarator : declarator_id
{
}
| direct_declarator '(' parameter_declaration_clause ')'
{
}
| direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
{
}
| direct_declarator '(' parameter_declaration_clause ')' exception_specification
{
}
| direct_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
}
| direct_declarator '[' constant_expression ']'
{
}
| direct_declarator '[' ']'
{
}
| '(' declarator ')'
{
}
;

/*
Això és ambigu ja que

  id_expression pot acabar sent un qualified_name que es de la forma A::B:: C
  type_name pot ser de la forma A::B:: C
*/
declarator_id : id_expression 
{
}
| nested_name_specifier type_name 
{
}
| DOS_DOS_PUNTS type_name
{
}
| DOS_DOS_PUNTS nested_name_specifier type_name
{
}
;


enum_specifier : ENUM IDENTIFIER '{' enumeration_list '}'
{
}
| ENUM '{' enumeration_list '}'
{
}
| ENUM IDENTIFIER '{' '}'
{
}
| ENUM '{' '}'
{
}
// GNU Extensions
| ENUM '{' '}' attributes
{
}
| ENUM IDENTIFIER '{' '}' attributes
{
}
| ENUM '{' enumeration_list '}' attributes
{
}
| ENUM IDENTIFIER '{' enumeration_list '}' attributes
{
}
;

enumeration_list : enumeration_list ',' enumeration_definition
{
}
| enumeration_definition
{
}
;

enumeration_definition : IDENTIFIER
{
}
| IDENTIFIER '=' constant_expression
{
}
;

type_id : type_specifier_seq
{
}
| type_specifier_seq abstract_declarator
{
}
;

abstract_declarator : ptr_operator
{
}
| ptr_operator abstract_declarator
{
}
| direct_abstract_declarator
{
}
// GNU Extensions
| attributes ptr_operator
{
}
| attributes ptr_operator abstract_declarator
{
}
| attributes direct_abstract_declarator
{
}
;

direct_abstract_declarator : '(' abstract_declarator ')'
{
}
| '(' parameter_declaration_clause ')'
{
}
| '(' parameter_declaration_clause ')' cv_qualifier_seq
{
}
| '(' parameter_declaration_clause ')' exception_specification
{
}
| '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
}
| direct_abstract_declarator '(' parameter_declaration_clause ')'
{
}
| direct_abstract_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq
{
}
| direct_abstract_declarator '(' parameter_declaration_clause ')' exception_specification
{
}
| direct_abstract_declarator '(' parameter_declaration_clause ')' cv_qualifier_seq exception_specification
{
}
| '[' constant_expression ']'
{
}
| '[' ']'
{
}
| direct_abstract_declarator '[' constant_expression ']'
{
}
| direct_abstract_declarator '[' ']'
{
}
;

parameter_declaration_clause : parameter_declaration_list 
{
}
| parameter_declaration_clause ',' TRES_PUNTS
{
} 
// Com funciona aixo ?
| TRES_PUNTS
{
}
// No m'agrada aixo, pero bueno xD
| /* empty */
{
}
;

parameter_declaration_list : parameter_declaration
{
}
| parameter_declaration_list ',' parameter_declaration
{
}
;

parameter_declaration : decl_specifier_seq declarator
{
}
| decl_specifier_seq declarator '=' assignment_expression
{
}
| decl_specifier_seq 
{
}
| decl_specifier_seq abstract_declarator
{
}
| decl_specifier_seq  '=' assignment_expression
{
}
| decl_specifier_seq abstract_declarator '=' assignment_expression
{
}
;

initializer : '=' initializer_clause
{
}
| '(' expression_list ')'
{
}
;

initializer_clause : assignment_expression
{
}
| '{' initializer_list '}'
{
}
| '{' initializer_list ',' '}'
{
}
| '{' '}'
{
}
;

initializer_list : initializer_clause
{
}
| initializer_list ',' initializer_clause
{
}
// GNU Extensions
| IDENTIFIER ':' initializer_clause
{
}
| initializer_list ',' IDENTIFIER ':' initializer_clause
{
}
;

function_definition : declarator function_body 
{
}
| declarator ctor_initializer function_body 
{
}
| decl_specifier_seq declarator function_body 
{
}
| decl_specifier_seq declarator ctor_initializer function_body 
{
}
// GNU Extensions
| EXTENSION function_definition
{
}
;

function_body : compound_statement
{
}
;

// *********************************************************
// A.8 - Classes
// *********************************************************

class_specifier : class_head '{' member_specification '}'
{
}
| class_head '{' '}'
{
}
;

class_head : class_key 
{
}
| class_key IDENTIFIER
{
}
| class_key base_clause
{
}
| class_key IDENTIFIER base_clause
{
}
| class_key nested_name_specifier IDENTIFIER 
{
}
| class_key nested_name_specifier IDENTIFIER base_clause
{
}
| class_key template_id
{
}
| class_key template_id base_clause
{
}
| class_key nested_name_specifier template_id
{
}
| class_key nested_name_specifier template_id base_clause
{
}
// GNU Extensions
| class_key attributes
{
}
| class_key attributes IDENTIFIER
{
}
| class_key attributes base_clause
{
}
| class_key attributes IDENTIFIER base_clause
{
}
| class_key attributes nested_name_specifier IDENTIFIER
{
}
| class_key attributes nested_name_specifier IDENTIFIER base_clause
{
}
| class_key attributes template_id 
{
}
| class_key attributes nested_name_specifier template_id 
{
}
| class_key attributes template_id base_clause
{
}
| class_key attributes nested_name_specifier template_id base_clause
{
}
;

class_key : CLASS
{
}
| STRUCT
{
}
| UNION
{
}
;

member_specification : member_declaration
{
}
| access_specifier ':'
{
}
| member_declaration member_specification
{
}
| access_specifier ':' member_specification
{
}
;

member_declaration : decl_specifier_seq member_declarator_list ';' 
{
}
| decl_specifier_seq ';' 
{
}
| member_declarator_list ';' 
{
}
| function_definition ';'
{
}
| function_definition 
{
}
| nested_name_specifier unqualified_id ';'
{
}
| DOS_DOS_PUNTS nested_name_specifier unqualified_id ';'
{
}
| nested_name_specifier TEMPLATE unqualified_id ';'
{
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE unqualified_id ';'
{
}
| using_declaration
{
}
| template_declaration
{
}
// GNU Extension
| EXTENSION member_declaration
{
}
;

member_declarator_list : member_declarator
{
}
| member_declarator_list ',' member_declarator
{
}
;

member_declarator : declarator 
{
}
| declarator constant_initializer
{
}
// - El susbsumirem amb constant_initializer -
// | declarator pure_specifier
// {
// }
| ':' expression
{
}
| IDENTIFIER ':' constant_expression
{
}
// GNU Extensions
| declarator attributes 
{
}
// - El susbsumirem amb constant_initializer -
// | declarator attributes pure_specifier
// {
// }
| declarator attributes constant_initializer
{
}
| IDENTIFIER attributes ':' constant_expression
{
}
| attributes ':' constant_expression
{
}
;

constant_initializer : '=' constant_expression
{
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
}
;

base_specifier_list : base_specifier
{
}
| base_specifier_list ',' base_specifier
{
}
;

base_specifier : type_name
{
}
| DOS_DOS_PUNTS type_name
{
}
| DOS_DOS_PUNTS nested_name_specifier type_name
{
}
| nested_name_specifier type_name
{
}
// Sense virtual
| access_specifier type_name
{
}
| access_specifier DOS_DOS_PUNTS type_name
{
}
| access_specifier nested_name_specifier type_name
{
}
| access_specifier DOS_DOS_PUNTS nested_name_specifier type_name
{
}
// Amb virtual abans
| VIRTUAL type_name
{
}
| VIRTUAL access_specifier type_name
{
}
| VIRTUAL DOS_DOS_PUNTS type_name
{
}
| VIRTUAL access_specifier DOS_DOS_PUNTS type_name
{
}
| VIRTUAL nested_name_specifier type_name
{
}
| VIRTUAL access_specifier nested_name_specifier type_name
{
}
| VIRTUAL DOS_DOS_PUNTS nested_name_specifier type_name
{
}
| VIRTUAL access_specifier DOS_DOS_PUNTS nested_name_specifier type_name
{
}
// Amb el virtual després de l'accés
| access_specifier VIRTUAL type_name
{
}
| access_specifier VIRTUAL DOS_DOS_PUNTS type_name
{
}
| access_specifier VIRTUAL nested_name_specifier type_name
{
}
| access_specifier VIRTUAL DOS_DOS_PUNTS nested_name_specifier type_name
{
}
;

access_specifier : PRIVATE
{
}
| PROTECTED
{
}
| PUBLIC
{
}
;

// *********************************************************
// A.5. - Statements
// *********************************************************


statement : no_if_statement
{
}
| if_statement
{
}
| if_else_statement
{
}
;

no_if_statement : labeled_statement
{
}
| expression_statement 
{
}
| compound_statement
{
}
| selection_statement
{
}
| iteration_statement
{
}
| jump_statement
{
}
// L'estàndard diu que les declaracions
// que es confonen amb expressions
// han de ser declaracions
| declaration_statement  
{
}
| try_block
{
}
;


labeled_statement : IDENTIFIER ':' statement
{
}
| CASE constant_expression ':' statement
{
}
| DEFAULT ':' statement
{
}
// GNU Extension
| CASE constant_expression TRES_PUNTS constant_expression ':' statement
{
}
;

expression_statement : ';'
{
}
| expression ';'
{
}
;

declaration_statement : block_declaration
{
}
;

compound_statement : '{' statement_seq '}'
{
}
| '{' '}'
;

statement_seq : statement
{
}
| statement_seq statement
{
}
;

// Ambiguitat del IF
// Aqui podem generar de tot
if_statement : IF '(' condition ')' statement
{
}
;

// Aqui nomes generarem if's q tenen else a dins
if_else_statement : IF '(' condition ')' if_else_eligible_statements ELSE statement
{
}
;

// Aqui nomes generarem if's q tenen else a dins
if_else_eligible_statements : no_if_statement
{
}
| if_else_statement
{
}
;

selection_statement : SWITCH '(' condition ')' statement
{
}
;

condition : expression
{
}
| type_specifier_seq declarator '=' assignment_expression
{
}
// GNU Extension
| type_specifier_seq declarator asm_specification attributes '=' assignment_expression
{
}
| type_specifier_seq declarator attributes '=' assignment_expression
{
}
| type_specifier_seq declarator asm_specification '=' assignment_expression
{
}
;

iteration_statement : WHILE '(' condition ')' statement
{
}
| DO statement WHILE '(' expression ')' ';'
{
}
| FOR '(' for_init_statement ';' ')' statement
{
}
| FOR '(' for_init_statement condition ';' ')' statement 
{
}
| FOR '(' for_init_statement condition ';' expression ')' statement
{
}
;

for_init_statement : expression_statement
{
}
| simple_declaration
{
}
;

jump_statement : BREAK ';'
{
}
| CONTINUE ';'
{
}
| RETURN ';'
{
}
| RETURN expression ';'
{
}
| GOTO IDENTIFIER
{
}
// GNU Extension
| GOTO '*' expression ';'
{
}
;

// *********************************************************
// A.10 - Special member functions
// *********************************************************

conversion_function_id : OPERATOR conversion_type_id
{
}
;

conversion_type_id : type_specifier_seq 
{
}
| type_specifier_seq conversion_declarator
{
}
;

conversion_declarator : ptr_operator
{
}
| ptr_operator conversion_declarator
{
}
;

ctor_initializer : ':' mem_initializer_list
{
}
;

mem_initializer_list : mem_initializer
{
}
| mem_initializer_list ',' mem_initializer
{
}
;

mem_initializer : mem_initializer_id '(' ')'
{
}
| mem_initializer_id '(' expression_list ')'
{
}
// GNU Extensions
| '(' expression_list ')'
{
}
| '(' ')'
{
}
;

mem_initializer_id : IDENTIFIER
{
}
| nested_name_specifier IDENTIFIER
{
}
| DOS_DOS_PUNTS nested_name_specifier IDENTIFIER
{
}
| DOS_DOS_PUNTS IDENTIFIER
{
}
| template_id
{
}
| nested_name_specifier template_id
{
}
| DOS_DOS_PUNTS nested_name_specifier template_id
{
}
| DOS_DOS_PUNTS template_id
{
}
;

// *********************************************************
// A.4 - Expressions
// *********************************************************

primary_expression : literal
{
}
| THIS
{
}
| '(' expression ')' 
{
}
| id_expression
{
}
// GNU Extensions
| '(' compound_statement ')'
{
}
| BUILTIN_VA_ARG '(' assignment_expression ',' type_id ')'
{
}
;

id_expression : qualified_id
{
}
| unqualified_id
{
}
;

qualified_id : nested_name_specifier unqualified_id
{
}
| DOS_DOS_PUNTS nested_name_specifier unqualified_id
{
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE unqualified_id
{
}
| DOS_DOS_PUNTS operator_function_id 
{
}
| DOS_DOS_PUNTS template_id
{
}
;

nested_name_specifier : class_or_namespace_name DOS_DOS_PUNTS 
{
}
| class_or_namespace_name DOS_DOS_PUNTS nested_name_specifier
{
}
| class_or_namespace_name DOS_DOS_PUNTS TEMPLATE nested_name_specifier
{
}
;

class_or_namespace_name : template_id
{
}
| IDENTIFIER
{
}
;

unqualified_id : IDENTIFIER
{
}
| operator_function_id
{
}
| conversion_function_id
{
}
| '~' IDENTIFIER
{
}
| template_id
{
}
;

postfix_expression : primary_expression
{
}
| postfix_expression '[' expression ']'
{
}
| postfix_expression '(' ')' 
{
}
| postfix_expression '(' expression_list ')' 
{
}
| simple_type_specifier '(' ')' 
{
}
| simple_type_specifier '(' expression_list ')' 
{
}
| TYPENAME nested_name_specifier IDENTIFIER '(' ')'
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier IDENTIFIER '(' ')'
{
}
| TYPENAME nested_name_specifier IDENTIFIER '(' expression_list ')'
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier IDENTIFIER '(' expression_list ')'
{
}
| TYPENAME nested_name_specifier template_id '(' ')'
{
}
| TYPENAME nested_name_specifier template_id '(' expression_list ')'
{
}
| TYPENAME nested_name_specifier TEMPLATE template_id '(' ')'
{
}
| TYPENAME nested_name_specifier TEMPLATE template_id '(' expression_list ')'
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier template_id '(' ')'
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier template_id '(' expression_list ')'
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id '(' ')'
{
}
| TYPENAME DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id '(' expression_list ')'
{
}
| postfix_expression '.' id_expression
{
}
| postfix_expression '.' TEMPLATE id_expression
{
}
| postfix_expression PTR_OP id_expression
{
}
| postfix_expression PTR_OP TEMPLATE id_expression
{
}
| postfix_expression '.' pseudo_destructor_name
{
}
| postfix_expression PTR_OP pseudo_destructor_name
{
}
| postfix_expression PLUSPLUS
{
}
| postfix_expression MINUSMINUS
{
}
| DYNAMIC_CAST '<' type_id '>' '(' expression ')'
{
}
| STATIC_CAST '<' type_id '>' '(' expression ')' 
{
}
| REINTERPRET_CAST '<' type_id '>' '(' expression ')'
{
}
| CONST_CAST '<' type_id '>' '(' expression ')'
{
}
| TYPEID '(' expression ')' 
{
}
| TYPEID '(' type_id ')' 
{
}
// GNU Extensions
| '(' type_id ')' '{' initializer_list '}'
{
}
| '(' type_id ')' '{' initializer_list ',' '}'
{
}
;

expression_list : expression 
{
}
| expression_list ',' expression
{
}
;

pseudo_destructor_name : type_name DOS_DOS_PUNTS '~' type_name
{
}
| nested_name_specifier type_name DOS_DOS_PUNTS '~' type_name
{
}
| DOS_DOS_PUNTS type_name DOS_DOS_PUNTS '~' type_name
{
}
| DOS_DOS_PUNTS nested_name_specifier type_name DOS_DOS_PUNTS '~' type_name
{
}
| nested_name_specifier TEMPLATE template_id DOS_DOS_PUNTS '~' type_name
{
}
| DOS_DOS_PUNTS nested_name_specifier TEMPLATE template_id DOS_DOS_PUNTS '~' type_name
{
}
| '~' type_name
{
}
| nested_name_specifier '~' type_name
{
}
| DOS_DOS_PUNTS nested_name_specifier '~' type_name
{
}
;

unary_expression : postfix_expression
{
}
| PLUSPLUS cast_expression
{
}
| MINUSMINUS cast_expression
{
}
| unary_operator cast_expression
{
}
| SIZEOF unary_expression  
{
}
| SIZEOF '(' type_id ')' 
{
}
| new_expression
{
}
| delete_expression
{
}
// GNU Extensions
| EXTENSION cast_expression
{
}
| ALIGNOF unary_expression
{
}
| ALIGNOF '(' type_id ')'
{
}
| REAL cast_expression
{
}
| IMAG cast_expression
{
}
| ANDAND IDENTIFIER
{
}
;

unary_operator : '*'
{
}
| '&' 
{
}
| '+'
{
}
| '-'
{
}
| '!'
{
}
| '~'
{
}
;

new_expression : NEW new_type_id 
{
}
| NEW new_type_id new_initializer
{
}
| NEW new_placement new_type_id 
{
}
| NEW new_placement new_type_id new_initializer
{
}
| DOS_DOS_PUNTS NEW new_type_id 
{
}
| DOS_DOS_PUNTS NEW new_type_id new_initializer
{
}
| DOS_DOS_PUNTS NEW new_placement new_type_id 
{
}
| DOS_DOS_PUNTS NEW new_placement new_type_id new_initializer
{
}
| NEW '(' type_id ')' 
{
}
| NEW '(' type_id ')' new_initializer
{
}
| NEW new_placement '(' type_id ')' 
{
}
| NEW new_placement '(' type_id ')' new_initializer
{
}
| DOS_DOS_PUNTS NEW '(' type_id ')' 
{
}
| DOS_DOS_PUNTS NEW '(' type_id ')' new_initializer
{
}
| DOS_DOS_PUNTS NEW new_placement '(' type_id ')' 
{
}
| DOS_DOS_PUNTS NEW new_placement '(' type_id ')' new_initializer
{
}
;

new_placement : '(' expression_list ')'
{
}
;

new_type_id : type_specifier_seq
{
}
| type_specifier_seq new_declarator
{
}
;

new_declarator : ptr_operator 
{
}
| ptr_operator new_declarator
{
}
| direct_new_declarator
{
}
;

direct_new_declarator : '[' expression ']'
{
}
| direct_new_declarator '[' constant_expression ']'
{
}
;

new_initializer : '(' ')'
{
}
| '(' expression_list ')'
{
}
;

delete_expression : DELETE cast_expression 
{
}
| DELETE '[' ']' cast_expression
{
}
| DOS_DOS_PUNTS DELETE cast_expression 
{
}
| DOS_DOS_PUNTS DELETE '[' ']' cast_expression
{
}
;

cast_expression : unary_expression
{
}
| '(' type_id ')' cast_expression
{
}
;

// pm stands for "pointer to member"
pm_expression : cast_expression
{
}
| pm_expression PTR_OP_MUL cast_expression
{
}
| pm_expression '.' '*' cast_expression
{
}
;

multiplicative_expression : pm_expression
{
}
| multiplicative_expression '*' pm_expression
{
}
| multiplicative_expression '/' pm_expression
{
}
| multiplicative_expression '%' pm_expression
{
}
;

additive_expression : multiplicative_expression
{
}
| additive_expression '+' multiplicative_expression
{
}
| additive_expression '-' multiplicative_expression
{
}
;

shift_expression : additive_expression
{
}
| shift_expression LEFT additive_expression
{
}
| shift_expression RIGHT additive_expression
{
}
;

relational_expression : shift_expression
{
}
| relational_expression '<' shift_expression
{
}
| relational_expression '>' shift_expression
{
}
| relational_expression GREATER_OR_EQUAL shift_expression
{
}
| relational_expression LESS_OR_EQUAL shift_expression
{
}
// GNU Extension
| relational_expression MAX_OPERATOR shift_expression
{
}
| relational_expression MIN_OPERATOR shift_expression
{
}
;

equality_expression : relational_expression
{
}
| equality_expression EQUAL relational_expression
{
}
| equality_expression NOT_EQUAL relational_expression
{
}
;

and_expression : equality_expression
{
}
| and_expression '&' equality_expression
{
}
;

exclusive_or_expression : and_expression
{
}
| exclusive_or_expression '^' and_expression
{
}
;

inclusive_or_expression : exclusive_or_expression
{
}
| inclusive_or_expression '|' exclusive_or_expression
{
}
;

logical_and_expression : inclusive_or_expression
{
}
| logical_and_expression ANDAND inclusive_or_expression
{
}
;

logical_or_expression : logical_and_expression
{
}
| logical_or_expression OROR logical_and_expression
{
}
;

conditional_expression : logical_or_expression
{
}
| logical_or_expression '?' expression ':' assignment_expression
{
}
// GNU Extension
| logical_or_expression '?' ':' assignment_expression
{
}
;

assignment_expression : conditional_expression
{
}
| logical_or_expression assignment_operator assignment_expression
{
}
| throw_expression
{
}
;

expression : assignment_expression
{
}
| expression ',' assignment_expression
{
}
;

assignment_operator : '='
{
}
| MUL_ASSIGN
{
}
| DIV_ASSIGN
{
}
| ADD_ASSIGN
{
}
| SUB_ASSIGN
{
}
| LEFT_ASSIGN
{
}
| RIGHT_ASSIGN
{
}
| AND_ASSIGN
{
}
| OR_ASSIGN
{
}
| XOR_ASSIGN
{
}
| LEFT_ASSIGN
{
}
| RIGHT_ASSIGN
{
}
| MOD_ASSIGN
{
}
// GNU Extensions
| MIN_OPERATOR_ASSIGN
{
}
| MAX_OPERATOR_ASSIGN 
{
}
;

constant_expression : conditional_expression
{
}
;

// *********************************************************
// A.2 - Lexical conventions
// *********************************************************

literal : DECIMAL_LITERAL
{
}
| OCTAL_LITERAL
{
}
| HEXADECIMAL_LITERAL
{
}
| FLOATING_LITERAL
{
}
| BOOLEAN_LITERAL
{
}
| CHARACTER_LITERAL
{
}
| string_literal
{
}
;

// *********************************************************
// A.11 - Overloading
// *********************************************************

operator_function_id : OPERATOR operator
{
}
| OPERATOR operator '<' template_argument_list '>'
{
}
| OPERATOR operator '<' '>'
{
}
;

operator : NEW
{
}
| DELETE
{
}
| NEW '[' ']'
{
}
| DELETE '[' ']'
{
}
| '+'
{
}
| '-'
{
}
| '*'
{
}
| '/'
{
}
| '%' 
{
}
| '^'
{
}
| '&'
{
}
| '|'
{
}
| '~'
{
}
| '!'
{
}
| '='
{
}
| '<'
{
}
| '>'
{
}
| ADD_ASSIGN
{
}
| SUB_ASSIGN
{
}
| MUL_ASSIGN
{
}
| DIV_ASSIGN
{
}
| MOD_ASSIGN
{
}
| XOR_ASSIGN
{
}
| AND_ASSIGN
{
}
| OR_ASSIGN
{
}
| LEFT
{
}
| RIGHT
{
}
| LEFT_ASSIGN
{
}
| RIGHT_ASSIGN
{
}
| EQUAL
{
}
| NOT_EQUAL
{
}
| LESS_OR_EQUAL
{
}
| GREATER_OR_EQUAL
{
}
| ANDAND
{
}
| OROR
{
}
| PLUSPLUS
{
}
| MINUSMINUS
{
}
| ','
{
}
| PTR_OP
{
}
| PTR_OP_MUL
{
}
| '(' ')'
{
}
| '[' ']'
{
}
;

// *********************************************************
// A.12 - Templates
// *********************************************************

template_id : IDENTIFIER '<' template_argument_list '>'
{
}
| IDENTIFIER '<' '>'
{
}
;

template_argument_list : template_argument
{
}
| template_argument_list ',' template_argument
{
}
;

template_argument : template_assignment_expression 
{
}
| type_id 
{
}
| id_expression 
{
}
;

template_declaration : TEMPLATE '<' template_parameter_list '>' declaration
{
}
// Nobody will care of this
| EXPORT TEMPLATE '<' template_parameter_list '>' declaration
{
}
;

template_parameter_list : template_parameter
{
}
| template_parameter_list ',' template_parameter
{
}
;

template_parameter : type_parameter 
{
}
| parameter_declaration 
{
}
;

type_parameter : CLASS
{
}
| CLASS IDENTIFIER
{
}
| CLASS '=' type_id
{
}
| CLASS IDENTIFIER '=' type_id
{
}
| TYPENAME 
{
}
| TYPENAME IDENTIFIER
{
}
| TYPENAME '=' type_id
{
}
| TYPENAME IDENTIFIER '=' type_id
{
}
| TEMPLATE '<' template_parameter_list '>' CLASS
{
}
| TEMPLATE '<' template_parameter_list '>' CLASS IDENTIFIER
{
}
| TEMPLATE '<' template_parameter_list '>' CLASS '=' id_expression
{
}
| TEMPLATE '<' template_parameter_list '>' CLASS IDENTIFIER '=' id_expression
{
}
;

/*
   Although the standard says `declaration', what it really means is:

   explicit-instantiation:
      template decl-specifier-seq [opt] declarator [opt] ;
 */

explicit_instantiation : TEMPLATE decl_specifier_seq declarator ';'
{
}
| TEMPLATE decl_specifier_seq ';'
{
}
| TEMPLATE declarator ';'
{
}
// GNU Extensions
| storage_class_specifier TEMPLATE decl_specifier_seq declarator ';'
{
}
| storage_class_specifier TEMPLATE decl_specifier_seq ';'
{
}
| storage_class_specifier TEMPLATE declarator ';'
{
}
| function_specifier TEMPLATE decl_specifier_seq declarator ';'
{
}
| function_specifier TEMPLATE decl_specifier_seq ';'
{
}
| function_specifier TEMPLATE declarator ';'
{
}
;

// explicit_specialization : TEMPLATE '<' '>' declaration
// {
// }
// ;
explicit_specialization : TEMPLATE '<' '>' decl_specifier_seq init_declarator ';'
{
}
| TEMPLATE '<' '>' init_declarator ';'
{
}
| TEMPLATE '<' '>' decl_specifier_seq ';'
{
}
| TEMPLATE '<' '>' function_definition 
{
}
| TEMPLATE '<' '>' explicit_specialization
{
}
| TEMPLATE '<' '>' template_declaration
{
}
;


// *********************************************************
// A.12.1 - Template Expressions
// *********************************************************
// Les expressions tenen restriccions amb ">" al nivell mes extern

template_relational_expression : shift_expression
{
}
| template_relational_expression '<' shift_expression
{
}
// AQUI NO
// | relational_expression '>' shift_expression
// {
// }
// AQUI NO
// | template_relational_expression GREATER_OR_EQUAL shift_expression
// {
// }
| relational_expression LESS_OR_EQUAL shift_expression
{
}
// GNU Extensions
| template_relational_expression MIN_OPERATOR shift_expression
{
}
;

template_equality_expression : template_relational_expression
{
}
| template_equality_expression EQUAL template_relational_expression
{
}
| template_equality_expression NOT_EQUAL template_relational_expression
{
}
;

template_and_expression : template_equality_expression
{
}
| template_and_expression '&' template_equality_expression
{
}
;

template_exclusive_or_expression : template_and_expression
{
}
| template_exclusive_or_expression '^' template_and_expression
{
}
;

template_inclusive_or_expression : template_exclusive_or_expression
{
}
| template_inclusive_or_expression '|' template_exclusive_or_expression
{
}
;

template_logical_and_expression : template_inclusive_or_expression
{
}
| template_inclusive_or_expression ANDAND template_inclusive_or_expression
{
}
;

template_logical_or_expression : template_logical_and_expression
{
}
| template_logical_or_expression OROR template_logical_and_expression
{
}
;

template_conditional_expression : template_logical_or_expression
{
}
| template_logical_or_expression '?' template_expression ':' template_assignment_expression
{
}
// GNU Extension
| template_logical_or_expression '?' ':' template_assignment_expression
{
}
;

template_assignment_expression : template_conditional_expression
{
}
| template_logical_or_expression assignment_operator template_assignment_expression
{
}
// No crec q ningu llenci una excepcio dins d'un template argument XD
// | template_throw_expression
// {
// }
;

template_expression : template_assignment_expression
{
}
| template_expression ',' assignment_operator
{
}
;


// *********************************************************
// A.13 - Exception handling
// *********************************************************

try_block : TRY compound_statement handler_seq
{
}
;

handler_seq : handler
{
}
| handler_seq handler
{
}
;

handler : CATCH '(' exception_declaration ')' compound_statement
{
}
;

exception_declaration : type_specifier declarator  
{
}
| type_specifier_seq abstract_declarator
{
}
| type_specifier_seq 
{
}
| TRES_PUNTS
{
}
;

exception_specification : THROW '(' ')' 
{
}
| THROW '(' type_id_list ')'
{
}
;

type_id_list : type_id 
{
}
| type_id_list ',' type_id
{
}
;

throw_expression : THROW
{
}
| THROW assignment_expression
{
}
;

// This eases parsing, though it should be viewed as a lexical issue
string_literal : STRING_LITERAL
{
}
| string_literal STRING_LITERAL
{
}
;


%%

int num_crides = 0;

static YYSTYPE ambiguityHandler (YYSTYPE x0, YYSTYPE x1)
{
	extern int no_line;
	num_crides++;
	fprintf(stderr, "Line %d --- AMBIGUITY HANDLER ---\n", no_line);

	return x0;
}

void yyerror(const char* c)
{
	extern char* yytext;
	extern int no_line;
	fprintf(stderr, "Line %d Error : '%s'\n", no_line, c);
    fprintf(stderr, "Token '%s'\n", yytext);
	exit(EXIT_FAILURE);
}

int main(int argc, char* argv[])
{
	// yydebug = 1;
	yyparse();

	fprintf(stderr, "Parse successful! #ambig=%d\n", num_crides);
	return 0;
}
