#ifndef TL_TRAVERSE_HPP
#define TL_TRAVERSE_HPP

#include <utility>
#include <vector>
#include "tl-context.hpp"
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
			typedef std::pair<Predicate, TraverseFunctor> CondAction;
			std::vector<CondAction> _pred_list;
		public:
			void add_predicate(Predicate& pred, TraverseFunctor& functor);
			void traverse(TL::AST_t node, TL::ScopeLink scope_link);
	};
}

#endif // TL_TRAVERSE_HPP
