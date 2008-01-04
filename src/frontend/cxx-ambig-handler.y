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

    return ast_make_ambiguous(son0, son1);
}
