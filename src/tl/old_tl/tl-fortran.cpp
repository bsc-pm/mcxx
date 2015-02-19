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



#include "tl-fortran.hpp"

namespace TL { namespace Fortran {

    const PredicateAttr ProgramUnit::predicate(LANG_IS_FORTRAN_PROGRAM_UNIT);

    const PredicateAttr Function::predicate(LANG_IS_FORTRAN_FUNCTION);
    const PredicateAttr Subroutine::predicate(LANG_IS_FORTRAN_SUBROUTINE);
    const PredicateAttr BlockData::predicate(LANG_IS_FORTRAN_BLOCK_DATA);
    const PredicateAttr Module::predicate(LANG_IS_FORTRAN_MODULE);

    const PredicateAttr SpecificationStatement::predicate(LANG_IS_FORTRAN_SPECIFICATION_STATEMENT);
    
    const PredicateAttr StopStatement::predicate(LANG_IS_FORTRAN_STOP_STATEMENT);
    
    ObjectList<Statement> ProgramUnit::get_statements()
    {
        ObjectList<Statement> result;
        AST_t program_unit_body = _ref.get_link_to_child(LANG_FORTRAN_PROGRAM_UNIT_BODY);
        
        if (!program_unit_body.is_valid())
            return result;

        AST_t statements = program_unit_body.get_link_to_child(LANG_FORTRAN_PROGRAM_UNIT_STATEMENTS);
        
        if (!statements.is_valid())
            return result;

        if (!statements.is_list())
        {
            std::cerr << "Warning: expecting a list here -- " << statements.get_locus_str() << std::endl;
            return result;
        }

        ASTIterator ast_it = statements.get_list_iterator();
        ast_it.rewind();
        
        while (!ast_it.end())
        {
            result.push_back(Statement(ast_it.item(), _scope_link));
            ast_it.next();
        }

        return result;
    }

    ObjectList<ProgramUnit> ProgramUnit::get_internal_program_units()
    {
        ObjectList<ProgramUnit> result;

        AST_t internal = _ref.get_link_to_child(LANG_FORTRAN_PROGRAM_UNIT_INTERNAL_SUBPROGRAMS);

        ASTIterator it = internal.get_list_iterator();
        while (!it.end())
        {
            result.push_back(ProgramUnit(it.item(), _scope_link));
            it.next();
        }

        return result;
    }

    Symbol ProgramUnit::get_related_symbol()
    {
        return this->get_scope().get_related_symbol();
    }
} }

