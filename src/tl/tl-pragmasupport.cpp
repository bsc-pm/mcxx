#include "tl-pragmasupport.hpp"

namespace TL
{
	PragmaDispatcher::PragmaDispatcher(const std::string& pragma_handled, 
			CustomFunctorMap& pre_map, CustomFunctorMap& post_map)
		: _pragma_handled(pragma_handled), _pre_map(pre_map), _post_map(post_map)
	{
	}

	void PragmaDispatcher::preorder(Context ctx, AST_t node)
	{
		dispatch_pragma_construct(_pre_map, ctx, node);
	}

	void PragmaDispatcher::postorder(Context ctx, AST_t node)
	{
		dispatch_pragma_construct(_post_map, ctx, node);
	}

	void PragmaDispatcher::dispatch_pragma_construct(CustomFunctorMap& search_map, Context ctx, AST_t node)
	{
		PragmaCustomConstruct pragma_custom_construct(node, ctx.scope_link);

		// If this is a handled pragma in this class
		if (pragma_custom_construct.get_pragma() == _pragma_handled)
		{
			// Search its functor
			if (search_map.find(pragma_custom_construct.get_directive()) != search_map.end())
			{
				Signal1<PragmaCustomConstruct>& functor = search_map[pragma_custom_construct.get_directive()];
				functor.signal(pragma_custom_construct);
			}
		}
	}

	std::string PragmaCustomConstruct::get_pragma() 
	{
		TL::String result = this->get_ast().get_attribute(LANG_PRAGMA_CUSTOM);
		return result;
	}

	std::string PragmaCustomConstruct::get_directive() 
	{
		TL::AST_t pragma_line = this->get_ast().get_attribute(LANG_PRAGMA_CUSTOM_LINE);

		TL::String result = this->get_ast().get_attribute(LANG_PRAGMA_CUSTOM_DIRECTIVE);

		return result;
	}

	bool PragmaCustomConstruct::is_directive()
	{
		TL::Bool is_directive = this->get_ast().get_attribute(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE);

		return is_directive;
	}

	bool PragmaCustomConstruct::is_construct()
	{
		TL::Bool is_construct = this->get_ast().get_attribute(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT);

		return is_construct;
	}

	Statement PragmaCustomConstruct::get_statement()
	{
		AST_t tree = this->get_ast().get_attribute(LANG_PRAGMA_CUSTOM_STATEMENT);
		Statement result(tree, this->get_scope_link());

		return result;
	}

	PragmaCompilerPhase::PragmaCompilerPhase(const std::string& pragma_handled)
		: _pragma_handled(pragma_handled), _pragma_dispatcher(pragma_handled, on_directive_pre, on_directive_post)
	{
	}

	void PragmaCompilerPhase::run(DTO& data_flow)
	{
		// get the translation_unit tree
		AST_t translation_unit = data_flow["translation_unit"];
		// get the scope_link
		ScopeLink scope_link = data_flow["scope_link"];
		// Get the global_scope
		Scope global_scope = scope_link.get_scope(translation_unit);

		// Instantiate a DepthTraverse
		DepthTraverse depth_traverse;

		depth_traverse.traverse(translation_unit, scope_link);
	}
}
