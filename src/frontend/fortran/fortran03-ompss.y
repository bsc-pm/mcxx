/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION "<ompss-dependency-expression>"

%type<ast> ompss_dependency_expr
%type<ast> ompss_multi_dependency

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION ompss_dependency_expr EOS
{
    $$ = $2;
}
;

ompss_dependency_expr : expr
{
    $$ = $1;
}
| ompss_multi_dependency
{
    $$ = $1;
}
;

ompss_multi_dependency : '[' multiexpression_implied_do ']'
{
    warn_printf("%s: warning: enclosing multi-dependences with '[' and ']' is deprecated\n",
               locus_to_str(make_locus(@1.first_filename, @1.first_line, @1.first_column)));
    info_printf("%s: info: use '{/' and '/}' instead\n",
               locus_to_str(make_locus(@1.first_filename, @1.first_line, @1.first_column)));
    $$ = $2;
}
| TOKEN_LPARENT_SLASH multiexpression_implied_do TOKEN_SLASH_RPARENT
{
    warn_printf("%s: warning: enclosing multi-dependences with '(/ and '/) is deprecated\n",
               locus_to_str(make_locus(@1.first_filename, @1.first_line, @1.first_column)));
    info_printf("%s: info: use '{/' and '/}' instead\n",
               locus_to_str(make_locus(@1.first_filename, @1.first_line, @1.first_column)));
    $$ = $2;
}
;


/*!endif*/
