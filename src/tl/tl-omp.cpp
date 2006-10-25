#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-contextlink.hpp"
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
	class ParallelConstructPred : public Predicate
	{
		public:
			virtual bool operator()(const AST_t& ast) const
			{
				TL::Object* attr = ast.get_attribute(OMP_IS_PARALLEL_CONSTRUCT);

				return (attr != NULL
						&& attr->is_bool()
						&& ((bool)(*attr)));
			}
	};

	class IdExpressionPred : public Predicate
	{
		public:
			virtual bool operator()(const AST_t& ast) const
			{
				TL::Object* attr = ast.get_attribute(LANG_IS_ID_EXPRESSION);

				return (attr != NULL
						&& attr->is_bool()
						&& ((bool)(*attr)));
			}
	};

	void OpenMPTransform::run(DTO& data_flow)
	{
		TL::AST_t* ast = dynamic_cast<TL::AST_t*>(data_flow["ast"]);
		TL::ContextLink* context_link = dynamic_cast<TL::ContextLink*>(data_flow["context_link"]);

		ParallelConstructPred parallel_construct_pred;

		AST_list_t par_constructs = ast->get_all_subtrees_predicate(parallel_construct_pred);

		AST_list_t::iterator it;
		for (it = par_constructs.begin(); it != par_constructs.end(); it++)
		{
			std::cerr << "--- OMP PARALLEL --" << std::endl;
			std::cerr << it->prettyprint() << std::endl;
			std::cerr << "-------------------" << std::endl;

			IdExpressionPred id_expression_pred;
			AST_list_t id_expressions = it->get_all_subtrees_predicate(id_expression_pred);

			AST_list_t::iterator it2;
			for (it2 = id_expressions.begin(); it2 != id_expressions.end(); it2++)
			{
				std::string id_expr_str = it2->prettyprint();

				std::cerr << "Retrieving symbol for entity '" << id_expr_str << "'" << std::endl;

				TL::Context* ctx = context_link->get_context(*it2);
				if (ctx == NULL)
				{
					std::cerr << "Context for '"<< id_expr_str << "' was not found" << std::endl;
					continue;
				}

				TL::Symbol* symbol = ctx->get_symbol_from_id_expr(*it2);

				if (symbol == NULL)
				{
					std::cerr << "Symbol for '" << id_expr_str << "' was not found" << std::endl;
					continue;
				}

				std::cerr << "Symbol name as contained in the context '" 
					<< symbol->get_name() << "'" 
					<< "(" << symbol << ")" 
					<< std::endl;
			}
		}
	}
}
