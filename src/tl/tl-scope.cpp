/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#include "tl-scope.hpp"
#include "cxx-scope.h"
#include "cxx-printscope.h"
#include "cxx-utils.h"
#include "hash_iterator.h"
#include "uniquestr.h"

namespace TL
{
    void Scope::printscope()
    {
        print_scope(_decl_context);
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

    tl_type_t* Scope::get_extended_attribute(const std::string&) const
    {
        return NULL;
    }

    ObjectList<Symbol> Scope::get_symbols_from_name(const std::string& str) const
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* entry_list = query_unqualified_name_str(_decl_context, const_cast<char*>(str.c_str()));

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

    ObjectList<Symbol> Scope::get_symbols_from_id_expr(TL::AST_t ast, bool examine_uninstantiated) const
    {
        ObjectList<Symbol> result;
        AST _ast = ast._ast;

        decl_flags_t flags = DF_NONE;

        if (!examine_uninstantiated)
            flags = DF_DEPENDENT_TYPENAME;

        scope_entry_list_t* entry_list = query_id_expression_flags(_decl_context, _ast, flags);

        convert_to_vector(entry_list, result);

        return result;
    }

    Symbol Scope::get_symbol_from_id_expr(TL::AST_t ast, bool examine_uninstantiated) const
    {
        ObjectList<Symbol> list = this->get_symbols_from_id_expr(ast, examine_uninstantiated);

        Symbol result(NULL);
        get_head(list, result);

        return result;
    }

    Scope Scope::temporal_scope() const
    {
        decl_context_t block_context = new_block_context(_decl_context);

        return Scope(block_context);
    }

    Scope& Scope::operator=(Scope sc)
    {
        this->_decl_context = sc._decl_context;
        return (*this);
    }

    bool Scope::operator<(Scope sc) const
    {
        return (this->_decl_context.current_scope < sc._decl_context.current_scope);
    }

    bool Scope::operator==(Scope sc) const
    {
        return (this->_decl_context.current_scope == sc._decl_context.current_scope);
    }

    bool Scope::operator!=(Scope sc) const
    {
        return !(this->operator==(sc));
    }

    ObjectList<Symbol> Scope::get_all_symbols(bool include_hidden)
    {
        ObjectList<Symbol> result;

        // This should be a bit more encapsulated in cxx-scope.c
        Iterator *it;

        it = (Iterator*) hash_iterator_create(_decl_context.current_scope->hash);
        for ( iterator_first(it); 
                !iterator_finished(it); 
                iterator_next(it))
        {
            scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);
            scope_entry_list_t* it = entry_list;

            while (it != NULL)
            {
                scope_entry_t* entry = it->entry;

                // Well, do_not_print is what we use to hide symbols :)
                if (!entry->do_not_print
                        || include_hidden)
                {
                    Symbol sym(entry);
                    result.append(sym);
                }

                it = it->next;
            }

        }

        return result;
    }

    Symbol Scope::new_artificial_symbol(const std::string& artificial_name, bool reuse_symbol)
    {
        scope_entry_t* sym_res = NULL;
        if (reuse_symbol)
        {
            scope_entry_list_t* sym_res_list = ::query_in_scope_str(_decl_context, artificial_name.c_str());

            if (sym_res_list != NULL)
            {
                sym_res = sym_res_list->entry;
                return Symbol(sym_res);
            }
        }

        // Create the symbol anyway
        sym_res = ::new_symbol(_decl_context, _decl_context.current_scope, uniquestr(artificial_name.c_str()));
        sym_res->kind = SK_OTHER;

        return Symbol(sym_res);
    }

    void Scope::insert_symbol(Symbol sym)
    {
        insert_entry(_decl_context.current_scope, sym.get_internal_symbol());
    }

    ObjectList<Symbol> Scope::cascade_lookup(const std::string& str)
    {
        scope_entry_list_t* entry_list = ::cascade_lookup(_decl_context, str.c_str());
        ObjectList<Symbol> result;
        convert_to_vector(entry_list, result);
        return result;
    }

    ObjectList<TemplateParameter> Scope::get_template_parameters() const
    {
        ObjectList<TemplateParameter> result;

        if (_decl_context.template_parameters != NULL)
        {
            for (int i = 0; i < _decl_context.template_parameters->num_template_parameters; i++)
            {
                result.append(TemplateParameter(
                            _decl_context.template_parameters->template_parameters[i]
                            ));
            }
        }

        return result;
    }
}
