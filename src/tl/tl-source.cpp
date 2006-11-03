#include "tl-source.hpp"
#include "cxx-ambiguity.h"
#include <sstream>

namespace TL
{
	Source& Source::operator<<(const std::string& str)
	{
		_code += str;
		return *this;
	}

	Source& Source::operator<<(int num)
	{
        std::stringstream ss;
        ss << num;
		_code += ss.str();
		return *this;
	}

    Source& Source::operator<<(const Source& src)
    {
        this->_code += src._code;
        return *this;
    }

	std::string Source::get_source()
	{
		return _code;
	}

	AST_t Source::parse_expression(TL::Scope ctx)
	{
		std::string mangled_text = "@EXPRESSION@ " + _code;
		const char* str = mangled_text.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		solve_possibly_ambiguous_expression(a, ctx._st, default_decl_context);

		AST_t result(a);
        return result;
	}
	
	AST_t Source::parse_statement(TL::Scope ctx, TL::ScopeLink scope_link)
	{
		std::string mangled_text = "@STATEMENT@ " + _code;
		const char* str = mangled_text.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		build_scope_statement_with_scope_link(a, ctx._st, scope_link._scope_link);

        AST_t result(a);
		return result;
	}

	AST_t Source::parse_global(TL::Scope ctx, TL::ScopeLink scope_link)
	{
		const char* str = _code.c_str();

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		build_scope_translation_unit_tree_with_global_scope(a, ctx._st, scope_link._scope_link);

        AST_t result(a);
		return result;
	}

	bool Source::operator==(Source src) const
	{
		return this->_code == src._code;
	}

	bool Source::operator<(Source src) const
	{
		return this->_code < src._code;
	}

	Source& Source::operator=(Source src)
	{
		this->_code = src._code;
		return (*this);
	}

    Source& Source::append_with_separator(Source src, const std::string& separator)
    {
        if (all_blanks())
        {
            this->_code = src._code;
        }
        else
        {
            this->_code += (separator + src._code);
        }

        return (*this);
    }

    bool Source::all_blanks() const
    {
        bool blanks = true;
        int len = _code.size();
        for (int i = 0; (i < len) && blanks; i++)
        {
            blanks &= (_code[i] == ' ') || (_code[i] == '\t');
        }
        return blanks;
    }
}
