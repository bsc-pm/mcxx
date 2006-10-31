#include "tl-traverse.hpp"

namespace TL
{
	AST Traverse::get_ast(TL::AST_t* node) const
	{
		return node->_ast;
	}

	TL::AST_t* Traverse::wrap_ast(AST a) const
	{
		return AST_t::wrap_ast(a);
	}

	void DepthTraverse::add_predicate(Predicate* pred, TraverseFunctor* functor)
	{
		CondAction c(pred, functor);
		_pred_list.push_back(c);
	}

	void DepthTraverse::traverse(TL::AST_t* node)
	{
		NoOperation no_op;
		TraverseFunctor* functor = &no_op;

		for (unsigned int i = 0; i < _pred_list.size(); i++)
		{
			Predicate* pred = _pred_list[i].first;

			if ((*pred)(*node))
			{
				functor = _pred_list[i].second;
			}
		}

		AST ast = this->get_ast(node);

		Context ctx;

		functor->preorder(ctx, node);

		for (int i = 0; i < ASTNumChildren(ast); i++)
		{
			AST child = ASTChild(ast, i);

			if (child != NULL)
			{
				traverse(this->wrap_ast(child));
			}
		}

		functor->postorder(ctx, node);
	}
}
