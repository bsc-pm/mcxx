#ifndef TL_OMP_HPP
#define TL_OMP_HPP

#include "tl-compilerphase.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"

namespace TL
{
	class OpenMPTransform : public CompilerPhase
	{
		public :
			AST_t translation_unit;
			ScopeLink scope_link;
			Scope global_scope;

			int num_parallels;
			int parallel_nesting;
			virtual void run(DTO& data_flow);

			virtual ~OpenMPTransform() { }
	};
}

extern "C"
{
	TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
