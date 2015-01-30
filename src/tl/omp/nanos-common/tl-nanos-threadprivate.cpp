/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#include "tl-nanos-threadprivate.hpp"
#include "cxx-utils.h"

namespace TL
{

#if 0
static void add_thread_to_declarations_in_tree(const ObjectList<Symbol> &symbol_list, const Declaration &decl)
{
    bool decl_is_in_class_scope = decl.get_scope().is_class_scope();

    ObjectList<DeclaredEntity> entities = decl.get_declared_entities();

    DeclarationSpec decl_spec = decl.get_declaration_specifiers();

    Source remade_declaration;
    for (ObjectList<DeclaredEntity>::iterator it = entities.begin();
            it != entities.end();
            it++)
    {
        remade_declaration
            << decl_spec.prettyprint();

        if (symbol_list.contains(it->get_declared_symbol()))
        {
            remade_declaration << " __thread";
        }

        remade_declaration << " " << it->prettyprint() << ";\n";
    }

    AST_t tree(NULL);
    if (!decl_is_in_class_scope)
    {
        tree = remade_declaration.parse_declaration(decl.get_ast(), 
                decl.get_scope_link(), 
                Source::ALLOW_REDECLARATION);
    }
    else
    {
        Symbol sym = decl.get_scope().get_class_of_scope();
        tree = remade_declaration.parse_member(decl.get_ast(),
                decl.get_scope_link(),
                sym);
    }

    decl.get_ast().replace(tree);
}

void Nanos::add_thread_to_declarations(const ObjectList<Symbol> &symbol_list, ScopeLink sl)
{
    ObjectList<AST_t> decl_list;
    decl_list.insert(symbol_list.map(functor(&Symbol::get_point_of_declaration)));

    for (ObjectList<AST_t>::iterator it = decl_list.begin();
            it != decl_list.end();
            it++)
    {
        add_thread_to_declarations_in_tree(symbol_list, Declaration(*it, sl));
    }

    decl_list.clear();
    for (ObjectList<Symbol>::const_iterator it = symbol_list.begin();
            it != symbol_list.end();
            it++)
    {
        if (it->is_member()
                && it->is_static()
                && it->is_defined())
        {
            AST_t def = it->get_point_of_definition();
            if (def.is_valid()
                    && def != it->get_point_of_declaration())
            {
                add_thread_to_declarations_in_tree(symbol_list, Declaration(def, sl));
            }
        }
    }
}
#endif

}
