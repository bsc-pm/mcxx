/*!if GRAMMAR_CODE*/
#include "cxx-utils.h"

static AST ambiguityHandler (YYSTYPE x0, YYSTYPE x1)
{
	AST son0 = x0.ast;
	AST son1 = x1.ast;

	if (son0 == son1) 
	{
        return son1;
	}

    return ast_make_ambiguous(son0, son1);
}
/*!endif*/
