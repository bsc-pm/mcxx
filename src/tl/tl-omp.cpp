#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-context.hpp"
#include <iostream>

extern "C" 
{
	TL::CompilerPhase* give_compiler_phase_object(void)
	{
		return new TL::OpenMPTransform();
	}
}

namespace TL
{
	class ParallelConstruct : public Predicate
	{
		public:
			virtual bool operator()(AST_t ast) const
			{
				TL::Object* attr = ast.attributes("omp.is_parallel_construct");

				return (attr != NULL
						&& attr->is_bool()
						&& ((bool)(*attr)));
			}
	};

	void OpenMPTransform::run(DTO& data_flow)
	{
		TL::AST_t* ast = dynamic_cast<TL::AST_t*>(data_flow["ast"]);
		TL::Context* context = dynamic_cast<TL::Context*>(data_flow["context"]);

		ParallelConstruct parallelConstructPred;

		AST_list_t parConstructs = ast->get_all_subtrees_predicate(parallelConstructPred);

		AST_list_t::iterator it;
		for (it = parConstructs.begin(); it != parConstructs.end(); it++)
		{
			std::cerr << "--- OMP PARALLEL --" << std::endl;
			std::cerr << it->prettyprint() << std::endl;
			std::cerr << "-------------------" << std::endl;
		}
	}
}
