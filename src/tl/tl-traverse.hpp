#ifndef TL_TRAVERSE_HPP
#define TL_TRAVERSE_HPP

#include <vector>
#include "tl-context.hpp"
#include "tl-scopelink.hpp"
#include "tl-ast.hpp"
#include "tl-predicate.hpp"

namespace TL
{
	class TraverseFunctor
	{
		private:
		public:
			virtual void preorder(Context ctx, AST_t node) { }
			virtual void postorder(Context ctx, AST_t node) { }

			virtual ~TraverseFunctor() { }
	};

    class Traverse { };

	class DepthTraverse : public Traverse
	{
		private:
			typedef std::pair<Predicate<AST_t>*, TraverseFunctor*> CondAction;
			std::vector<CondAction> _pred_list;
		public:
			void add_predicate(Predicate<AST_t>& pred, TraverseFunctor& functor);
			void traverse(AST_t node, ScopeLink scope_link);
	};
}

#endif // TL_TRAVERSE_HPP
