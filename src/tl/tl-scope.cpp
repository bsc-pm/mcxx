#include "tl-scope.hpp"

namespace TL
{
	std::map<scope_entry_t*, Symbol*> SymbolMapping::_map;

	Symbol* SymbolMapping::get_symbol(scope_entry_t* entry)
	{
		if (_map.find(entry) == _map.end())
		{
			Symbol* sym = new Symbol(entry);
			_map[entry] = sym;
			return sym;
		}
		else
		{
			return _map[entry];
		}
	}

	void Scope::convert_to_vector(scope_entry_list_t* entry_list, std::vector<Symbol*>& out)
	{
		while (entry_list != NULL)
		{
			out.push_back(SymbolMapping::get_symbol(entry_list->entry));
			entry_list = entry_list->next;
		}
	}

	void Scope::get_head(const std::vector<Symbol*>& in, Symbol*& out)
	{
		out = NULL;
		if (in.size() > 0)
		{
			out = *(in.begin());
		}
	}

	tl_type_t* Scope::get_extended_attribute(const std::string& str) const
	{
		return NULL;
	}

	std::vector<Symbol*> Scope::get_symbols_from_name(const std::string& str)
	{
		std::vector<Symbol*> result;
		// Fix this for C++
		scope_entry_list_t* entry_list = query_unqualified_name(_st, const_cast<char*>(str.c_str()));

		convert_to_vector(entry_list, result);

		return result;
	}

	Symbol* Scope::get_symbol_from_name(const std::string& str)
	{
		std::vector<Symbol*> list = this->get_symbols_from_name(str);

		Symbol* result = NULL;
		get_head(list, result);

		return result;
	}

	std::vector<Symbol*> Scope::get_symbols_from_id_expr(TL::AST_t* ast)
	{
		std::vector<Symbol*> result;
		AST _ast = ast->_ast;

		scope_entry_list_t* entry_list = query_id_expression(_st, _ast, 
				FULL_UNQUALIFIED_LOOKUP, default_decl_context);

		convert_to_vector(entry_list, result);

		return result;
	}

	Symbol* Scope::get_symbol_from_id_expr(TL::AST_t* ast)
	{
		std::vector<Symbol*> list = this->get_symbols_from_id_expr(ast);

		Symbol* result = NULL;
		get_head(list, result);

		return result;
	}

}
