/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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
#include "tl-symbol.hpp"
#include "cxx-scope.h"
#include "cxx-printscope.h"
#include "cxx-utils.h"
#include "cxx-entrylist.h"
#include "uniquestr.h"
#include "cxx-koenig.h"
#include "cxx-exprtype.h"

namespace TL
{
    void Scope::printscope()
    {
        print_scope(_decl_context);
    }

    void Scope::convert_to_vector(scope_entry_list_t* entry_list, ObjectList<Symbol>& out)
    {
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(entry_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            Symbol s(entry_list_iterator_current(it));
            out.push_back(s);
        }
        entry_list_iterator_free(it);
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
        scope_entry_list_t* entry_list = query_name_str(_decl_context, uniquestr(str.c_str()), NULL);

        convert_to_vector(entry_list, result);

        entry_list_free(entry_list);

        return result;
    }

    ObjectList<Symbol> Scope::get_symbols_from_name_in_scope(const std::string& str) const
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* entry_list = query_in_scope_str(_decl_context, uniquestr(str.c_str()), NULL);

        convert_to_vector(entry_list, result);

        entry_list_free(entry_list);

        return result;
    }

    Symbol Scope::get_symbol_from_name(const std::string& str) const
    {
        ObjectList<Symbol> list = this->get_symbols_from_name(str);

        Symbol result(NULL);
        get_head(list, result);

        return result;
    }

    Symbol Scope::get_symbol_from_name_in_scope(const std::string& str) const
    {
        ObjectList<Symbol> list = this->get_symbols_from_name_in_scope(str);

        Symbol result(NULL);
        get_head(list, result);

        return result;
    }

    Scope Scope::get_global_scope() 
    {
        return CURRENT_COMPILED_FILE->global_decl_context;
    }

    Scope Scope::temporal_scope() const
    {
        const decl_context_t* block_context = new_block_context(_decl_context);

        return Scope(block_context);
    }

    Scope& Scope::operator=(Scope sc)
    {
        this->_decl_context = sc._decl_context;
        return (*this);
    }

    bool Scope::operator<(Scope sc) const
    {
        return (this->_decl_context->current_scope < sc._decl_context->current_scope);
    }

    bool Scope::operator==(Scope sc) const
    {
        return (this->_decl_context->current_scope == sc._decl_context->current_scope);
    }

    bool Scope::operator!=(Scope sc) const
    {
        return !(this->operator==(sc));
    }

    struct walk_scope_data_t
    {
        ObjectList<Symbol> &result;
        bool include_hidden;

        walk_scope_data_t(ObjectList<Symbol>& result_,
                bool include_hidden_)
            : result(result_), 
            include_hidden(include_hidden_) { }
    };

    static void walk_scope(const void* key, void* info, void* data)
    {
        // This should be a bit more encapsulated
        walk_scope_data_t* walk_data = (walk_scope_data_t*)(data);

        scope_entry_list_t* entry_list = (scope_entry_list_t*) info;
        scope_entry_list_iterator_t* it = NULL;

        for (it = entry_list_iterator_begin(entry_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            // Well, do_not_print is what we use to hide symbols :)
            if (!entry->do_not_print
                    || walk_data->include_hidden)
            {
                Symbol sym(entry);
                walk_data->result.append(sym);
            }
        }
        entry_list_iterator_free(it);
    }

    ObjectList<Symbol> Scope::get_all_symbols(bool include_hidden)
    {
        ObjectList<Symbol> result;

        walk_scope_data_t walk_data(result, include_hidden);
        dhash_ptr_walk(_decl_context->current_scope->dhash, (dhash_ptr_walk_fn*)walk_scope, &walk_data);

        return result;
    }

    Symbol Scope::new_artificial_symbol(const std::string& artificial_name, bool reuse_symbol)
    {
        scope_entry_t* sym_res = NULL;
        if (reuse_symbol)
        {
            scope_entry_list_t* sym_res_list = ::query_in_scope_str(_decl_context, uniquestr(artificial_name.c_str()), NULL);

            if (sym_res_list != NULL)
            {
                sym_res = entry_list_head(sym_res_list);
                return Symbol(sym_res);
            }
        }

        // Create the symbol anyway
        sym_res = ::new_symbol(_decl_context, _decl_context->current_scope, uniquestr(artificial_name.c_str()));
        sym_res->kind = SK_OTHER;

        return Symbol(sym_res);
    }

    Symbol Scope::new_symbol(const std::string& name)
    {
        scope_entry_t* sym_res = ::new_symbol(_decl_context, _decl_context->current_scope, uniquestr(name.c_str()));
        return Symbol(sym_res);
    }

    void Scope::insert_symbol(Symbol sym)
    {
        insert_entry(_decl_context->current_scope, sym.get_internal_symbol());
    }

    bool Scope::scope_is_enclosed_by(Scope potential_encloser) const
    {
        if (_decl_context->current_scope != potential_encloser.get_decl_context()->current_scope)
        {
            return ::scope_is_enclosed_by(_decl_context->current_scope, potential_encloser.get_decl_context()->current_scope);
        }
        else
        {
            return false;
        }
    }

    Symbol Scope::get_class_of_scope()
    {
        return _decl_context->class_scope->related_entry;
    }

    Symbol Scope::get_related_symbol() const
    {
        return _decl_context->current_scope->related_entry;
    }

    template_parameter_list_t* Scope::get_template_parameters() const
    {
       return _decl_context->template_parameters;
    }

    Symbol Scope::get_symbol_this() const
    {
        return resolve_symbol_this(_decl_context);
    }
}
