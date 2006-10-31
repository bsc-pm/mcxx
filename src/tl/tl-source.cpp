#include "tl-source.hpp"
#include "cxx-ambiguity.h"

namespace TL
{
	Source& Source::operator<<(const std::string& str)
	{
		_code += str;
		return *this;
	}

	std::string Source::get_source()
	{
		return _code;
	}

	AST_t* Source::parse_expression(TL::Scope* ctx)
	{
		std::string mangled_text = "@EXPRESSION@ " + _code;
		const char* str = mangled_text.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		solve_possibly_ambiguous_expression(a, ctx->_st, default_decl_context);

		return TL::AST_t::wrap_ast(a);
	}
	
	AST_t* Source::parse_statement(TL::Scope* ctx, TL::ScopeLink* scope_link)
	{
		std::string mangled_text = "@STATEMENT@ " + _code;
		const char* str = mangled_text.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		build_scope_statement_with_scope_link(a, ctx->_st, scope_link->_scope_link);

		return TL::AST_t::wrap_ast(a);
	}

	AST_t* Source::parse_global(TL::Scope* ctx, TL::ScopeLink* scope_link)
	{
		const char* str = _code.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		build_scope_translation_unit_tree_with_global_scope(a, ctx->_st, scope_link->_scope_link);

		return TL::AST_t::wrap_ast(a);
	}
}
