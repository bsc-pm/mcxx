/*!if GRAMMAR_CODE*/
#include "cxx-utils.h"

static AST ambiguityHandler (YYSTYPE x0, YYSTYPE x1)
{
	AST son0 = x0.ast;
	AST son1 = x1.ast;

	if (son0 == son1) 
	{
		internal_error("Ambiguity function received two trees that are the same!\n", 0);
	}

    return ast_make_ambiguous(son0, son1);
}
/*!endif*/
