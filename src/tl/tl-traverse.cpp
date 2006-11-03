#include "tl-traverse.hpp"

namespace TL
{
	void DepthTraverse::add_predicate(Predicate& pred, TraverseFunctor& functor)
	{
		CondAction c(&pred, &functor);
		_pred_list.push_back(c);
	}

	void DepthTraverse::traverse(TL::AST_t node)
	{
		TraverseFunctor no_op;
		TraverseFunctor* functor = &no_op;

		for (unsigned int i = 0; i < _pred_list.size(); i++)
		{
			Predicate* pred = _pred_list[i].first;

			if ((*pred)(node))
			{
				functor = _pred_list[i].second;
                break;
			}
		}

		AST ast = node._ast;

		Context ctx;

		functor->preorder(ctx, node);

		for (int i = 0; i < ASTNumChildren(ast); i++)
		{
			AST child = ASTChild(ast, i);

			if (child != NULL)
			{
                AST_t w_child(child);
				traverse(w_child);
			}
		}

		functor->postorder(ctx, node);
	}
}
