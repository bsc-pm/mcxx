#ifndef TL_PRAGMASUPPORT_HPP
#define TL_PRAGMASUPPORT_HPP

#include <string>
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "cxx-attrnames.h"

namespace TL
{
	class PragmaCustomConstruct : public LangConstruct
	{
		public:
			PragmaCustomConstruct(AST_t ref, ScopeLink scope_link)
				: LangConstruct(ref, scope_link)
			{
			}

			std::string get_pragma();
			std::string get_directive();

			bool is_directive();
			bool is_construct();

			Statement get_statement();
	};

	typedef std::map<std::string, Signal1<PragmaCustomConstruct> > CustomFunctorMap;

	class PragmaDispatcher : TraverseFunctor
	{
		private:
			std::string _pragma_handled;
			CustomFunctorMap& _pre_map;
			CustomFunctorMap& _post_map;

			void dispatch_pragma_construct(CustomFunctorMap& search_map, Context ctx, AST_t node);
		public:
			PragmaDispatcher(const std::string& pragma_handled, CustomFunctorMap& pre_map,
					CustomFunctorMap& post_map);

			virtual void preorder(Context ctx, AST_t node);
			virtual void postorder(Context ctx, AST_t node);
	};

	class PragmaCompilerPhase : public CompilerPhase
	{
		private:
			std::string _pragma_handled;
			PragmaDispatcher _pragma_dispatcher;
		public:
			PragmaCompilerPhase(const std::string& pragma_handled);
			virtual void run(DTO& data_flow);

			CustomFunctorMap on_directive_pre;
			CustomFunctorMap on_directive_post;
	};
}

#endif // TL_PRAGMASUPPORT_HPP
