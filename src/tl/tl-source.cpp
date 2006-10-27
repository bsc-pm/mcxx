#include "tl-source.hpp"

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

	AST_t* Source::parse_global(TL::Scope* ctx, TL::ScopeLink* scope_link)
	{
		const char* str = _code.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		build_scope_translation_unit_tree_with_global_scope(a, ctx->_st, scope_link->_scope_link);

		return new TL::AST_t(a);
	}
}
