#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"
#include <iostream>
#include <set>

extern "C" 
{
    TL::CompilerPhase* give_compiler_phase_object(void)
    {
        return new TL::OpenMPTransform();
    }
}

namespace TL
{
    class ParallelConstruct : public PredicateBool<OMP_IS_PARALLEL_CONSTRUCT>
    {
    };

    class IdExpression : public PredicateBool<LANG_IS_ID_EXPRESSION>
    {
    };

	class OMPContext
	{
		public:
			int num_parallels;
	};

	class ParallelFunctor : public TraverseFunctor
	{
		private:
			OMPContext& _omp_context;
		public:
			ParallelFunctor(OMPContext& omp_context)
				: _omp_context(omp_context)
			{
			}

			virtual void preorder(Context ctx, AST_t node)
			{
				_omp_context.num_parallels++;
			}

			virtual void postorder(Context ctx, AST_t node)
			{
				_omp_context.num_parallels--;

				AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);
				
				// Now get all the references in it
				IdExpression id_expression;
				AST_list_t references_list = construct_body.get_all_subtrees_predicate(id_expression);
				AST_list_t::iterator it;
				for (it = references_list.begin(); it != references_list.end(); it++)
				{
					std::string id_expr_str = it->prettyprint();

					Source derref_id_expr_str;
					derref_id_expr_str << "(*" << id_expr_str << ")";

					Scope scope = ctx.scope_link.get_scope(*it);

					AST_t derref_expr = derref_id_expr_str.parse_expression(scope);

					it->replace_with(derref_expr);
				}
			}

			virtual ~ParallelFunctor() { }
	};

    void OpenMPTransform::run(DTO& dto)
    {
        AST_t translation_unit = dto["translation_unit"];
        ScopeLink scope_link = dto["scope_link"];
        Scope global_scope = scope_link.get_scope(translation_unit);

		OMPContext omp_context;

		DepthTraverse depth_traverse;

		ParallelConstruct on_parallel;
		ParallelFunctor parallel_functor(omp_context);

		depth_traverse.add_predicate(on_parallel, parallel_functor);
    }
}
