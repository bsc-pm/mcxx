#include "tl-scope.hpp"
#include "cxx-printscope.h"

namespace TL
{
	void Scope::printscope()
	{
		print_scope(_st);
	}

	void Scope::convert_to_vector(scope_entry_list_t* entry_list, ObjectList<Symbol>& out)
	{
		while (entry_list != NULL)
		{
            Symbol s(entry_list->entry);
			out.push_back(s);
			entry_list = entry_list->next;
		}
	}

	void Scope::get_head(const ObjectList<Symbol>& in, Symbol& out)
	{
		if (in.size() > 0)
		{
			ObjectList<Symbol>::const_iterator it = in.begin();
			out = (*it);
		}
		else
		{
			out = Symbol::invalid();
		}
	}

	tl_type_t* Scope::get_extended_attribute(const std::string& str) const
	{
		return NULL;
	}

	ObjectList<Symbol> Scope::get_symbols_from_name(const std::string& str) const
	{
		ObjectList<Symbol> result;
		// Fix this for C++
		scope_entry_list_t* entry_list = query_unqualified_name(_st, const_cast<char*>(str.c_str()));

		convert_to_vector(entry_list, result);

		return result;
	}

	Symbol Scope::get_symbol_from_name(const std::string& str) const
	{
		ObjectList<Symbol> list = this->get_symbols_from_name(str);

		Symbol result(NULL);
		get_head(list, result);

		return result;
	}

	ObjectList<Symbol> Scope::get_symbols_from_id_expr(TL::AST_t ast) const
	{
		ObjectList<Symbol> result;
		AST _ast = ast._ast;

		scope_entry_list_t* entry_list = query_id_expression(_st, _ast, 
				FULL_UNQUALIFIED_LOOKUP, default_decl_context);

		convert_to_vector(entry_list, result);

		return result;
	}

	Symbol Scope::get_symbol_from_id_expr(TL::AST_t ast) const
	{
		ObjectList<Symbol> list = this->get_symbols_from_id_expr(ast);

        Symbol result(NULL);
		get_head(list, result);

		return result;
	}

    Scope Scope::temporal_scope() const
    {
        scope_t* st = new_block_scope(_st, _st->prototype_scope, _st->function_scope);

        return Scope(st);
    }

	Scope& Scope::operator=(Scope sc)
	{
		this->_st = sc._st;
		return (*this);
	}

	bool Scope::operator<(Scope sc) const
	{
		return (this->_st < sc._st);
	}

	bool Scope::operator==(Scope sc) const
	{
		return (this->_st == sc._st);
	}

	bool Scope::operator!=(Scope sc) const
	{
		return !(this->operator==(sc));
	}

}
