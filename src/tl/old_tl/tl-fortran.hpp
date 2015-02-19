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



#ifndef TL_FORTRAN
#define TL_FORTRAN

#ifdef HAVE_CONFIG_H
 #include "config.h"
#endif


#include "tl-common.hpp"
#include "tl-langconstruct.hpp"

namespace TL { namespace Fortran {

    //! Wraps a Fortran program unit
    /*!
      This class is the base class for ProgramUnits in fortran
      */
    class LIBTL_CLASS ProgramUnit : public LangConstruct
    {
        public:
            ProgramUnit(AST_t a, ScopeLink sl)
                : LangConstruct(a, sl) { }

            Symbol get_related_symbol();

            ObjectList<Statement> get_statements();
            ObjectList<ProgramUnit> get_internal_program_units();

            static const PredicateAttr predicate;
    };

    class LIBTL_CLASS Function : public ProgramUnit
    {
        public:
            Function(AST_t a, ScopeLink sl) 
                : ProgramUnit(a, sl) { }

            static const PredicateAttr predicate;
    };

    class LIBTL_CLASS Subroutine : public ProgramUnit
    {
        public:
            Subroutine(AST_t a, ScopeLink sl) 
                : ProgramUnit(a, sl) { }

            static const PredicateAttr predicate;
    };

    class LIBTL_CLASS BlockData : public ProgramUnit
    {
        public:
            BlockData(AST_t a, ScopeLink sl) 
                : ProgramUnit(a, sl) { }

            static const PredicateAttr predicate;
    };

    class LIBTL_CLASS Module : public ProgramUnit
    {
        public:
            Module(AST_t a, ScopeLink sl) 
                : ProgramUnit(a, sl) { }

            static const PredicateAttr predicate;
    };

    class LIBTL_CLASS SpecificationStatement : public Statement
    {
        public:
            SpecificationStatement(AST_t a, ScopeLink sl) 
                : Statement(a, sl) { }
            static const PredicateAttr predicate;
    };
    
    
    // Special Fortran Statements
    
    class LIBTL_CLASS StopStatement : public Statement
    {
        public:
            StopStatement(AST_t a, ScopeLink sl) 
                : Statement(a, sl) { }
            static const PredicateAttr predicate;
    };
} }

#endif
