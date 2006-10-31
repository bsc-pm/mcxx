#ifndef TL_TRAVERSE_HPP
#define TL_TRAVERSE_HPP

#include <utility>
#include <vector>
#include "tl-ast.hpp"
#include "tl-predicate.hpp"
#include "tl-context.hpp"

namespace TL
{
	class TraverseFunctor
	{
		private:
		public:
			virtual void preorder(const Context& ctx, AST_t* node) = 0;
			virtual void postorder(const Context& ctx, AST_t* node) = 0;

			virtual ~TraverseFunctor() { }
	};

	class NoOperation : public TraverseFunctor
	{
		private:
		public:
			virtual void preorder(const Context& ctx, AST_t* node) { }
			virtual void postorder(const Context& ctx, AST_t* node) { }

			virtual ~NoOperation() { }
	};

	class Traverse
	{
		protected:
			AST get_ast(TL::AST_t* ast) const;
			AST_t* wrap_ast(AST a) const;
	};

	class DepthTraverse : public Traverse
	{
		private:
			typedef std::pair<Predicate*, TraverseFunctor*> CondAction;
			std::vector<CondAction> _pred_list;
		public:
			void add_predicate(Predicate* pred, TraverseFunctor* functor);
			void traverse(TL::AST_t* node);
	};
}

#endif // TL_TRAVERSE_HPP
