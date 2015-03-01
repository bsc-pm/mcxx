/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> UNKNOWN_PRAGMA "<unknown-pragma>"
%type<ast> unknown_pragma

/*!ifnot FORTRAN2003*/
%token<token_atrib> VERBATIM_PRAGMA "<verbatim pragma>"
%token<token_atrib> VERBATIM_CONSTRUCT "<verbatim construct>"
%token<token_atrib> VERBATIM_TYPE "<verbatim type clause>"
%token<token_atrib> VERBATIM_TEXT "<verbatim text>"

%type<ast> verbatim_construct
/*!endif*/

%token<token_atrib> PRAGMA_CUSTOM "<pragma-custom>"
%token<token_atrib> PRAGMA_CUSTOM_NEWLINE "<pragma-custom-newline>"
%token<token_atrib> PRAGMA_CUSTOM_DIRECTIVE "<pragma-custom-directive>"
%token<token_atrib> PRAGMA_CUSTOM_CONSTRUCT "<pragma-custom-construct>"
%token<token_atrib> PRAGMA_CUSTOM_END_CONSTRUCT "<pragma-custom-end-construct>"
%token<token_atrib> PRAGMA_CUSTOM_CONSTRUCT_NOEND "<pragma-custom-construct-noend>"
%token<token_atrib> PRAGMA_CUSTOM_END_CONSTRUCT_NOEND "<pragma-custom-end-construct-noend>"

%token<token_atrib> PRAGMA_CUSTOM_CLAUSE "<pragma-custom-clause>"

%token<token_atrib> PRAGMA_CLAUSE_ARG_TEXT "<pragma-clause-argument-text>"

%type<ast> pragma_custom_directive
%type<ast> pragma_custom_line_directive
%type<ast> pragma_custom_line_construct
%type<ast> pragma_custom_construct_statement
/*!ifnot FORTRAN2003*/
%type<ast> pragma_custom_construct_declaration
%type<ast> pragma_custom_construct_member_declaration
/*!endif*/
/*!if FORTRAN2003*/
%type<ast> pragma_custom_construct_external_procedure
%type<ast> pragma_custom_construct_external_procedure_0
%type<ast2> pragma_custom_construct_range
%type<ast2> pragma_custom_noend_construct_range
%type<ast2> pragma_custom_noend_shared_term_do_range
%type<ast> pragma_custom_noend_line_construct
%type<ast> pragma_custom_line_or_noend_construct
%type<ast> pragma_custom_end_construct
%type<ast> pragma_custom_end_construct_noend
%type<ast> pragma_custom_construct_internal_program_unit
%type<ast> pragma_custom_construct_module_subprogram_unit
%type<ast> pragma_custom_construct_interface_body
%type<ast> explicit_external_procedure
%type<ast> pragma_custom_shared_term_do_construct
/*!endif*/
%type<ast> pragma_custom_clause
%type<ast> pragma_custom_clause_seq
%type<ast> pragma_custom_clause_opt_seq

// %type<ast> pragma_expression_entity
// %type<ast> pragma_expression_entity_list

%type<ast> pragma_clause_arg_list

%type<token_atrib> pragma_clause_arg
%type<token_atrib> pragma_clause_arg_item 
%type<token_atrib> pragma_clause_arg_text

/*!endif*/
/*!if GRAMMAR_RULES*/

// ****************************
//   Unknown pragma
// ****************************

/*!ifnot FORTRAN2003*/
unknown_pragma : UNKNOWN_PRAGMA
{
	$$ = ASTLeaf(AST_UNKNOWN_PRAGMA, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

common_block_declaration : unknown_pragma
{
    $$ = $1;
}
;

member_declaration : unknown_pragma
{
    $$ = $1;
}
;
/*!endif*/

/*!if FORTRAN2003*/
unknown_pragma : UNKNOWN_PRAGMA eos
{
	$$ = ASTLeaf(AST_UNKNOWN_PRAGMA, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;
program_unit : unknown_pragma
;
non_top_level_program_unit_stmt: unknown_pragma
;
internal_subprogram : unknown_pragma
;
module_subprogram : unknown_pragma
;
interface_specification : unknown_pragma
;
/*!endif*/

// ****************************
//   Pragma rules
// ****************************

/*!ifnot FORTRAN2003*/
nondeclarating_statement : pragma_custom_construct_statement
{
    $$ = $1;
}
| pragma_custom_directive
{
    $$ = $1;
}
;

declaration : pragma_custom_construct_declaration
{
    $$ = $1;
}
| pragma_custom_directive
{
	$$ = $1;
}
;

member_declaration : pragma_custom_construct_member_declaration
{
    $$ = $1;
}
| pragma_custom_directive
{
    $$ = $1;
}
;
/*!endif*/
/*!if FORTRAN2003*/
non_top_level_program_unit_stmt: pragma_custom_construct_statement
{
    $$ = $1;
}
| pragma_custom_directive
{
    $$ = $1;
}
;

program_unit : pragma_custom_construct_external_procedure
;

pragma_custom_construct_external_procedure : PRAGMA_CUSTOM pragma_custom_line_construct pragma_custom_construct_external_procedure_0
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

pragma_custom_construct_external_procedure_0 : explicit_external_procedure
| pragma_custom_construct_external_procedure
;

explicit_external_procedure : explicit_main_program
| external_subprogram
;

/*!endif*/

// Pragma custom

pragma_custom_directive : PRAGMA_CUSTOM pragma_custom_line_directive
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_DIRECTIVE, $2, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

/*!ifnot FORTRAN2003*/
pragma_custom_construct_declaration : PRAGMA_CUSTOM pragma_custom_line_construct declaration
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

pragma_custom_construct_member_declaration : PRAGMA_CUSTOM pragma_custom_line_construct member_declaration
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

pragma_custom_construct_statement : PRAGMA_CUSTOM pragma_custom_line_construct statement
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;
/*!endif*/
/*!if FORTRAN2003*/
pragma_custom_construct_statement : PRAGMA_CUSTOM pragma_custom_line_construct pragma_custom_construct_range
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3[0], $3[1], make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM pragma_custom_noend_line_construct pragma_custom_noend_construct_range
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3[0], $3[1], make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

// This case allows a sequence of statements but forces a end construct to appear
pragma_custom_construct_range : block pragma_custom_end_construct
{
    $$[0] = $1;
    $$[1] = $2;
}
;


// These cases only allows a single statements but does not require an end construct to appear
pragma_custom_noend_construct_range : non_top_level_program_unit_stmt pragma_custom_end_construct_noend
{
    $$[0] = ASTMake1(AST_COMPOUND_STATEMENT, ASTListLeaf($1), ast_get_locus($1), NULL);
    $$[1] = $2;
}
| non_top_level_program_unit_stmt
{
    $$[0] = ASTMake1(AST_COMPOUND_STATEMENT, ASTListLeaf($1), ast_get_locus($1), NULL);
    $$[1] = NULL;
}
;

/*

  Unusual cases for shared term do construct like the following

  DO 42 I = 1, 100
  !$OMP PARALLEL DO
  DO 42 J = 1, 100
    .. FOO (I, J) ..
  42 CONTINUE

*/
shared_term_do_construct : pragma_custom_shared_term_do_construct
;

pragma_custom_shared_term_do_construct : PRAGMA_CUSTOM pragma_custom_noend_line_construct pragma_custom_noend_shared_term_do_range
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3[0], $3[1], make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

pragma_custom_noend_shared_term_do_range : shared_term_do_construct pragma_custom_end_construct_noend
{
    $$[0] = ASTMake1(AST_COMPOUND_STATEMENT, ASTListLeaf($1), ast_get_locus($1), NULL);
    $$[1] = $2;
}
| shared_term_do_construct
{
    $$[0] = ASTMake1(AST_COMPOUND_STATEMENT, ASTListLeaf($1), ast_get_locus($1), NULL);
    $$[1] = NULL;
}
;

pragma_custom_end_construct : PRAGMA_CUSTOM PRAGMA_CUSTOM_END_CONSTRUCT pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $3, NULL, make_locus(@2.first_filename, @2.first_line, @2.first_column), $2.token_text);
}
| PRAGMA_CUSTOM PRAGMA_CUSTOM_END_CONSTRUCT pragma_custom_clause_opt_seq '(' pragma_clause_arg_list ')' PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $3, $5, make_locus(@2.first_filename, @2.first_line, @2.first_column), $2.token_text);
}
;

pragma_custom_end_construct_noend : PRAGMA_CUSTOM PRAGMA_CUSTOM_END_CONSTRUCT_NOEND pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $3, NULL, make_locus(@2.first_filename, @2.first_line, @2.first_column), $2.token_text);
}
;


pragma_custom_noend_line_construct : PRAGMA_CUSTOM_CONSTRUCT_NOEND pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $2, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM_CONSTRUCT_NOEND '(' pragma_clause_arg_list ')' pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $5, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

module_subprogram : pragma_custom_construct_module_subprogram_unit
;

pragma_custom_line_or_noend_construct : pragma_custom_line_construct
|  pragma_custom_noend_line_construct
;

pragma_custom_construct_module_subprogram_unit : PRAGMA_CUSTOM pragma_custom_line_or_noend_construct module_subprogram
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

internal_subprogram : pragma_custom_construct_internal_program_unit
;

pragma_custom_construct_internal_program_unit : PRAGMA_CUSTOM pragma_custom_line_or_noend_construct internal_subprogram
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

interface_body : pragma_custom_construct_interface_body
;

pragma_custom_construct_interface_body : PRAGMA_CUSTOM pragma_custom_line_or_noend_construct interface_body
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;
/*!endif*/

pragma_custom_line_directive : PRAGMA_CUSTOM_DIRECTIVE pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $2, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM_DIRECTIVE '(' pragma_clause_arg_list ')' pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $5, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM_NEWLINE
{
    // This is a degenerated case caused by wrong designed pragmas
    $$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, NULL, NULL, make_locus("", 0, 0), NULL);
}
;

pragma_custom_line_construct : PRAGMA_CUSTOM_CONSTRUCT pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $2, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM_CONSTRUCT '(' pragma_clause_arg_list ')' pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $5, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

pragma_custom_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| pragma_custom_clause_seq
{
	$$ = $1;
}
;

pragma_custom_clause_seq : pragma_custom_clause
{
	$$ = ASTListLeaf($1);
}
| pragma_custom_clause_seq ',' pragma_custom_clause
{
	$$ = ASTList($1, $3);
}
| pragma_custom_clause_seq pragma_custom_clause
{
	$$ = ASTList($1, $2);
}
;

pragma_custom_clause : PRAGMA_CUSTOM_CLAUSE '(' pragma_clause_arg_list ')'
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM_CLAUSE '(' ')'
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| PRAGMA_CUSTOM_CLAUSE 
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

pragma_clause_arg_list : pragma_clause_arg
{
    AST node = ASTLeaf(AST_PRAGMA_CLAUSE_ARG, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);

    $$ = ASTListLeaf(node);
}
;

pragma_clause_arg : pragma_clause_arg_item
{
    $$ = $1;
}
| pragma_clause_arg pragma_clause_arg_item
{
    $$.token_text = strappend($1.token_text, $2.token_text);
}
;

pragma_clause_arg_item : pragma_clause_arg_text
{
    $$ = $1;
}
;

pragma_clause_arg_text : PRAGMA_CLAUSE_ARG_TEXT
{
    $$ = $1;
}
;

/*!ifnot FORTRAN2003*/
// Verbatim construct
verbatim_construct : VERBATIM_PRAGMA VERBATIM_TYPE '(' identifier_token ')' VERBATIM_TEXT
{
    AST ident = ASTLeaf(AST_SYMBOL, make_locus(@4.first_filename, @4.first_line, @4.first_column), $4.token_text);

    $$ = ASTMake1(AST_VERBATIM, ident, make_locus(@1.first_filename, @1.first_line, @1.first_column), $6.token_text);
}
| VERBATIM_PRAGMA VERBATIM_TEXT
{
    $$ = ASTMake1(AST_VERBATIM, NULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $2.token_text);
}
;

common_block_declaration : verbatim_construct
{
    $$ = $1;
}
;

member_declaration : verbatim_construct
{
    $$ = $1;
}
;
/*!endif*/

/*!endif*/
