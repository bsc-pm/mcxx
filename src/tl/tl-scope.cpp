/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include "cxx-scope.h"
#include "cxx-printscope.h"
#include "cxx-utils.h"
#include "cxx-entrylist.h"
#include "uniquestr.h"
#include "cxx-koenig.h"

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
        scope_entry_list_t* entry_list = query_name_str(_decl_context, const_cast<char*>(str.c_str()));

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

    ObjectList<Symbol> Scope::get_symbols_from_id_expr(TL::AST_t ast, bool examine_uninstantiated) const
    {
        ObjectList<Symbol> result;
        AST _ast = ast._ast;

        decl_flags_t flags = DF_NONE;

        if (!examine_uninstantiated)
            flags = DF_DEPENDENT_TYPENAME;

        scope_entry_list_t* entry_list = query_id_expression_flags(_decl_context, _ast, flags);

        convert_to_vector(entry_list, result);

        entry_list_free(entry_list);

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
        rb_tree_walk(_decl_context.current_scope->hash, walk_scope, &walk_data);

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
                sym_res = entry_list_head(sym_res_list);
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

    ObjectList<Symbol> Scope::cascade_lookup(const std::string& str, const std::string& filename, int line)
    {
        scope_entry_list_t* entry_list = ::cascade_lookup(_decl_context, str.c_str(), DF_NONE, filename.c_str(), line);
        ObjectList<Symbol> result;
        convert_to_vector(entry_list, result);
        entry_list_free(entry_list);

        return result;
    }

    ObjectList<TemplateParameter> Scope::get_template_parameters() const
    {
        ObjectList<TemplateParameter> result;

        if (_decl_context.template_parameters != NULL)
        {
            for (int i = 0; i < _decl_context.template_parameters->num_parameters; i++)
            {
                result.append(TemplateParameter(
                            _decl_context.template_parameters,
                            i
                            ));
            }
        }

        return result;
    }

    Symbol Scope::get_class_of_scope()
    {
        return _decl_context.class_scope->related_entry;
    }

    ObjectList<Symbol> Scope::koenig_lookup(ObjectList<Type> arguments, AST_t id_expr)
    {
        internal_error("Not yet implemented", 0);
        //* int num_args = arguments.size();
        //* type_t** argument_list = new type_t*[arguments.size()];

        //* int i = 0;
        //* for (ObjectList<Type>::iterator it = arguments.begin(); it != arguments.end(); it++, i++)
        //* {
        //*     argument_list[i] = it->get_internal_type();
        //* }

        //* scope_entry_list_t* entry_list = ::koenig_lookup(num_args, argument_list, _decl_context, id_expr.get_internal_ast());

        //* ObjectList<Symbol> result;
        //* convert_to_vector(entry_list, result);
        //* entry_list_free(entry_list);

        //* delete[] argument_list;

        //* return result;
    }

    AST_t Scope::wrap_symbol_name(const std::string& str)
    {
        AST a = ASTLeaf(AST_SYMBOL, NULL, 0, str.c_str());
        return a;
    }

    Symbol Scope::get_related_symbol() const
    {
        return _decl_context.current_scope->related_entry;
    }
}
