#include "tl-transformtargetdeclare.hpp"

#include "tl-streamtransformhelper.hpp"
#include "tl-targetinfo.hpp"

namespace TL
{

// TransformTargetDeclare constructor ------------------------------------------
TransformTargetDeclare::
TransformTargetDeclare
		( const PragmaCustomConstruct& pragma_custom_construct
		, TargetInfo* target_info
		)
		: _pragma_custom_construct(pragma_custom_construct)
		, _target_info(target_info)
{
}

// TransformTargetDeclare destructor -------------------------------------------
TransformTargetDeclare::
~TransformTargetDeclare()
{
}

// transform -------------------------------------------------------------------
void
TransformTargetDeclare::
transform
		( void
		)
{
	Source declares_src= this->generate_declares();
	
	// Pragma code add
	FunctionDefinition function_definition 
		= _pragma_custom_construct.get_enclosing_function();
		
	AST_t function_ast = function_definition.get_ast();
	ScopeLink function_scope_link = function_definition.get_scope_link();
	
	AST_t task_add_tree = declares_src
			.parse_global(function_ast, function_scope_link);
		
	function_definition.get_ast().prepend_sibling_function(task_add_tree);
}

// generate_declares -----------------------------------------------------------
std::string
TransformTargetDeclare::
generate_declares
		( void
		) const
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			declare_ostream_all(_target_info->get_ostream_target_info_set());
	ss << StreamTransformHelper::
			declare_istream_all(_target_info->get_istream_target_info_set());

	return ss.str();
}


}
