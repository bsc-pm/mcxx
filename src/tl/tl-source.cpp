#include "tl-source.hpp"
#include "cxx-ambiguity.h"
#include <iostream>
#include <sstream>
#include "cxx-printscope.h"
#include "cxx-utils.h"

namespace TL
{
	std::string SourceRef::get_source() const
	{
		return _src->get_source();
	}

	void Source::append_text_chunk(const std::string& str)
	{
		if ((*_chunk_list).empty())
		{
			(*_chunk_list).push_back(new SourceText(str));
		}
		else
		{
			SourceChunk* last = *((*_chunk_list).rbegin());

			if (last->is_source_text())
			{
				// Collapse two adjacent texts
				SourceText* text = dynamic_cast<SourceText*>(last);
				text->_source += str;
			}
			else
			{
				(*_chunk_list).push_back(new SourceText(str));
			}
		}
	}

	void Source::append_source_ref(Source& src)
	{
		(*_chunk_list).push_back(new SourceRef(src));
	}

	Source& Source::operator<<(const std::string& str)
	{
		append_text_chunk(str);
		return *this;
	}

	Source& Source::operator<<(int num)
	{
        std::stringstream ss;
        ss << num;
		append_text_chunk(ss.str());
		return *this;
	}

    Source& Source::operator<<(Source& src)
    {
		Source *new_src = new Source(src);
		append_source_ref(*new_src);
        return *this;
    }

	std::string Source::get_source(bool with_newlines) const
	{
		std::string temp_result;
		for(std::vector<SourceChunk*>::const_iterator it = (*_chunk_list).begin();
				it != (*_chunk_list).end();
				it++)
		{
			temp_result += (*it)->get_source();
		}
		std::string result;

		if (!with_newlines)
		{
			result = temp_result;
		}
		else
		{
			// Eases debugging
			for (unsigned int i = 0; i < temp_result.size(); i++)
			{
				char c = temp_result[i];

				result += c;
				if (c == ';' || c == '{' || c == '}')
				{
					result += '\n';
				}
			}
		}

		return result;
	}

	AST_t Source::parse_expression(TL::Scope ctx)
	{
		std::string mangled_text = "@EXPRESSION@ " + this->get_source(true);
		char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

		AST a;

        CXX_LANGUAGE()
        {
            mcxxparse(&a);
        }

        C_LANGUAGE()
        {
            mc99parse(&a);
        }

		solve_possibly_ambiguous_expression(a, ctx._st, default_decl_context);

		AST_t result(a);
        return result;
	}

	AST_t Source::parse_expression(TL::Scope ctx, TL::ScopeLink scope_link)
	{
		std::string mangled_text = "@EXPRESSION@ " + this->get_source(true);
		char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

		AST a;

        CXX_LANGUAGE()
        {
            mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            mc99parse(&a);
        }

		solve_possibly_ambiguous_expression(a, ctx._st, default_decl_context);

		AST_t result(a);

        scope_link_set(scope_link._scope_link, a, ctx._st);

        return result;
	}

	AST_t Source::parse_member(TL::Scope ctx, TL::ScopeLink scope_link, Type class_type)
	{
		std::string mangled_text = "@MEMBER@ " + this->get_source(true);
		char* str = strdup(mangled_text.c_str());

		mcxx_prepare_string_for_scanning(str);

		AST a;
		mcxxparse(&a);

		build_scope_member_specification_with_scope_link(ctx._st, a, AS_PUBLIC, 
				class_type._type_info, default_decl_context, scope_link._scope_link);

		return AST_t(a);
	}
	
	AST_t Source::parse_statement(TL::Scope ctx, TL::ScopeLink scope_link)
	{
		std::string mangled_text = "@STATEMENT@ " + this->get_source(true);
		char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

		AST a;

        CXX_LANGUAGE()
        {
            mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            mc99parse(&a);
        }

		build_scope_statement_with_scope_link(a, ctx._st, scope_link._scope_link);

        AST_t result(a);
		return result;
	}

    AST_t Source::parse_declaration(TL::Scope ctx, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
		std::string mangled_text = "@DECLARATION@ " + this->get_source(true);
		char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

		AST a;

        CXX_LANGUAGE()
        {
            mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            mc99parse(&a);
        }

        decl_context_tag decl_context = default_decl_context;

        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::ALLOW_REDECLARATION) == Source::ALLOW_REDECLARATION)
        {
            decl_context.decl_flags = (decl_flags_t)((int)(decl_context.decl_flags) | DF_ALLOW_REDEFINITION);
        }

		build_scope_declaration_sequence_with_scope_link(a, ctx._st, decl_context, scope_link._scope_link);

        AST_t result(a);
		return result;
    }

	AST_t Source::parse_global(TL::Scope ctx, TL::ScopeLink scope_link)
	{
		char* str = strdup(this->get_source(true).c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

		AST a;

        CXX_LANGUAGE()
        {
            mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            mc99parse(&a);
        }
        
        decl_context_tag decl_context = default_decl_context;

		build_scope_translation_unit_tree_with_global_scope(a, ctx._st, scope_link._scope_link, decl_context);

        AST_t result(a);
		return result;
	}

	bool Source::operator==(const Source& src) const
	{
		return this->get_source() == src.get_source();
	}

	bool Source::operator!=(const Source &src) const
	{
		return !(this->operator==(src));
	}

	bool Source::operator<(const Source &src) const
	{
		return this->get_source() < src.get_source();
	}

	Source& Source::operator=(const Source& src)
	{
		if (this != &src)
		{
			*(this->_chunk_list) = *(src._chunk_list);
		}
		return (*this);
	}

    Source& Source::append_with_separator(const std::string& src, const std::string& separator)
    {
        if (all_blanks())
        {
            append_text_chunk(src);
        }
        else
        {
			append_text_chunk(separator + src);
        }

        return (*this);
    }

	Source& Source::append_with_separator(Source& src, const std::string& separator)
	{
        if (!all_blanks())
        {
			append_text_chunk(separator);
        }
		append_source_ref(src);

        return (*this);
	}

    bool Source::all_blanks() const
    {
		if ((*_chunk_list).empty())
		{
			return true;
		}

        bool blanks = true;
		std::string code = this->get_source();
        int len = code.size();

        for (int i = 0; (i < len) && blanks; i++)
        {
            blanks &= (code[i] == ' ') || (code[i] == '\t');
        }

        return blanks;
    }
}
