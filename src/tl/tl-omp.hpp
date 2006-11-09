#ifndef TL_OMP_HPP
#define TL_OMP_HPP

#include "tl-compilerphase.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "tl-dto.hpp"

namespace TL
{
	namespace OpenMP
	{
		class Directive;
		class Construct : public LangConstruct
		{
			public:
				Construct(AST_t ref)
					: LangConstruct(ref)
				{
				}

				AST_t body();
				Directive directive();
		};

		class Clause;
		class DefaultClause;
		class ReductionClause;
		class CustomClause;

		class Directive : public LangConstruct
		{
			public:
				Directive(AST_t ref)
					: LangConstruct(ref)
				{
				}

				Clause nowait_clause();
				Clause num_threads_clause();
				Clause if_clause();

				Clause shared_clause();
				Clause private_clause();
				Clause firstprivate_clause();
				Clause lastprivate_clause();

				Clause copyin_clause();
				Clause copyprivate_clause();

				DefaultClause default_clause();

				ReductionClause reduction_clause();

				CustomClause custom_clause(std::string& src);
		};

		class Clause : public LangConstruct
		{
			public:
				Clause(AST_t ref)
					: LangConstruct(ref) 
				{ 
				}
				ObjectList<Symbol> symbols();
		};

		class DefaultClause : public LangConstruct
		{
			public:
				DefaultClause(AST_t ref)
					: LangConstruct(ref)
				{
				}

				bool is_none();
				bool is_shared();
		};

		class ReductionClause : public LangConstruct
		{
			public:
				ReductionClause(AST_t ref)
					: LangConstruct(ref)
				{
				}

				AST_t operation();

				ObjectList<Symbol> symbols();
		};

		class CustomClause : public LangConstruct
		{
			public:
				CustomClause(AST_t ref)
					: LangConstruct(ref)
				{
				}

				// Pending
		};

		class ParallelConstruct : public Construct
		{
			public:
				ParallelConstruct(AST_t ref)
					: Construct(ref)
				{
				}
		};

		class OpenMPPhase : public CompilerPhase
		{
			private:
				class ParallelFunctor : public TraverseFunctor
				{
					private:
						OpenMPPhase& _phase;
					public:
						virtual void preorder(Context ctx, AST_t node) 
						{
							ParallelConstruct parallel_construct = node;
							_phase.on_parallel_pre.signal(parallel_construct);
						}

						virtual void postorder(Context ctx, AST_t node) 
						{
							ParallelConstruct parallel_construct = node;
							_phase.on_parallel_post.signal(parallel_construct);
						}

						ParallelFunctor(OpenMPPhase& phase)
							: _phase(phase)
						{
						}
				};
			protected:
				AST_t translation_unit;
				ScopeLink scope_link;
				Scope global_scope;
			public:
				Signal1<ParallelConstruct> on_parallel_pre;
				Signal1<ParallelConstruct> on_parallel_post;

				virtual void run(DTO& data_flow);
				virtual void init();
		};
	}
}

extern "C"
{
	TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
