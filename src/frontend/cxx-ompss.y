/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION "<ompss-dependency-expression>"

%type<ast> ompss_dependency_expr
%type<ast> ompss_old_multidependences

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION ompss_dependency_expr
{
    $$ = $2;
}
;

ompss_dependency_expr : assignment_expression
{
    $$ = $1;
}
| ompss_old_multidependences
{
    $$ = $1;
}
;

ompss_old_multidependences : '{' multiexpression_body '}'
{
    warn_printf("%s: warning: enclosing multi-dependences with '{' and '}' is deprecated\n",
               locus_to_str(make_locus(@1.first_filename, @1.first_line, @1.first_column)));
    info_printf("%s: info: use '{/' and '/}' instead\n",
               locus_to_str(make_locus(@1.first_filename, @1.first_line, @1.first_column)));
    $$ = $2;
}
/*!endif*/
