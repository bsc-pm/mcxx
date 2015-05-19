/*!if GRAMMAR_PROLOGUE */

%token<token_atrib> INTEL_ASSUME "__assume"
%token<token_atrib> INTEL_ASSUME_ALIGNED "__assume_aligned"

/*!endif*/
/*!if GRAMMAR_RULES*/

primary_expression : INTEL_ASSUME '(' expression ')'
{
    $$ = ASTMake1(AST_INTEL_ASSUME, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
| INTEL_ASSUME_ALIGNED '(' assignment_expression ',' assignment_expression ')'
{
    $$ = ASTMake2(AST_INTEL_ASSUME_ALIGNED, $3, $5, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;

/*!endif*/
