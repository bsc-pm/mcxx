#include "tl-scope.hpp"

namespace TL
{
	void Scope::convert_to_vector(scope_entry_list_t* entry_list, std::vector<Symbol>& out)
	{
		while (entry_list != NULL)
		{
            Symbol s(entry_list->entry);
			out.push_back(s);
			entry_list = entry_list->next;
		}
	}

	void Scope::get_head(const std::vector<Symbol>& in, Symbol& out)
	{
		if (in.size() > 0)
		{
            std::vector<Symbol>::const_iterator it = in.begin();
			out = (*it);
		}
	}

	tl_type_t* Scope::get_extended_attribute(const std::string& str) const
	{
		return NULL;
	}

	std::vector<Symbol> Scope::get_symbols_from_name(const std::string& str)
	{
		std::vector<Symbol> result;
		// Fix this for C++
		scope_entry_list_t* entry_list = query_unqualified_name(_st, const_cast<char*>(str.c_str()));

		convert_to_vector(entry_list, result);

		return result;
	}

	Symbol Scope::get_symbol_from_name(const std::string& str)
	{
		std::vector<Symbol> list = this->get_symbols_from_name(str);

		Symbol result = NULL;
		get_head(list, result);

		return result;
	}

	std::vector<Symbol> Scope::get_symbols_from_id_expr(TL::AST_t ast)
	{
		std::vector<Symbol> result;
		AST _ast = ast._ast;

		scope_entry_list_t* entry_list = query_id_expression(_st, _ast, 
				FULL_UNQUALIFIED_LOOKUP, default_decl_context);

		convert_to_vector(entry_list, result);

		return result;
	}

	Symbol Scope::get_symbol_from_id_expr(TL::AST_t ast)
	{
		std::vector<Symbol> list = this->get_symbols_from_id_expr(ast);

        Symbol result(NULL);
		get_head(list, result);

		return result;
	}

	Scope& Scope::operator=(Scope sc)
	{
		this->_st = sc._st;
		return (*this);
	}

	bool Scope::operator<(Scope sc)
	{
		return (this->_st < sc._st);
	}

	bool Scope::operator==(Scope sc)
	{
		return (this->_st == sc._st);
	}
}
