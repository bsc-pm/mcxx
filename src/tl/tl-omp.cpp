#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-context.hpp"
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
			virtual bool operator()(AST_t ast) const
			{
				TL::Object* attr = ast.attributes(OMP_IS_PARALLEL_CONSTRUCT);

				return (attr != NULL
						&& attr->is_bool()
						&& ((bool)(*attr)));
			}
	};

	class IdExpressionPred : public Predicate
	{
		public:
			virtual bool operator()(AST_t ast) const
			{
				TL::Object* attr = ast.attributes(LANG_IS_ID_EXPRESSION);

				return (attr != NULL
						&& attr->is_bool()
						&& ((bool)(*attr)));
			}
	};

	void OpenMPTransform::run(DTO& data_flow)
	{
		TL::AST_t* ast = dynamic_cast<TL::AST_t*>(data_flow["ast"]);
		// TL::Context* context = dynamic_cast<TL::Context*>(data_flow["context"]);

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

			std::set<std::string> id_expression_names;
			AST_list_t::iterator it2;
			for (it2 = id_expressions.begin(); it2 != id_expressions.end(); it2++)
			{
				id_expression_names.insert(it2->prettyprint());
			}

			std::cerr << "List of id expressions found in the parallel construct" << std::endl;
			std::set<std::string>::iterator it_names;
			for (it_names = id_expression_names.begin(); 
					it_names != id_expression_names.end(); 
					it_names++)
			{
				std::cerr << (*it_names) << std::endl;
			}
		}


	}
}
