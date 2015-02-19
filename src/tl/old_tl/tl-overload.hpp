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




#ifndef TL_OVERLOAD_HPP
#define TL_OVERLOAD_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    struct Overload
    {
        //! Solves the overload
        /*!
          This function can be used to solve overload. It offers an interface
          to the inner machinery of overload resolution used in C++. Do not call
          this function in C.

          \p candidate_functions List of symbols that will be used for overloading. The exact
          symbols are non relevant, but most of the time they are a list returned by a query
          to the Scope
          \p implicit_argument_type If you know that you are calling a member function pass
          the type of the class type. For non-member functions pass Type(NULL). Note that it
          is not possible to solve an overload of members and non-members at the same time.
          If you need this, like it happens in overloaded operators, call this function
          twice with two different candidate_functions lists: one of members and the other
          with non-members.
          \p argument_types This is the list of types you are going to use for the call. Note that
          in C++ a lvalue is roughly equivalent to a reference type.
          \p filename Location information: filename where the overload is being solved. This is used
          for potential instantiations
          \p line Location information: line number of the file where the overload is being solved. This
          is used for potential instantiations
          \p valid States if the overload succeeded. If this boolean is true, then the returning
          Symbol is meaningful. Otherwise the returning Symbol will not be valid.
          \p viable_functions Is the list actually used when performing overload, it includes all deduced specialized templates
          \p argument_conversor A list of symbols, with the same number of elements as argument_types, 
          with conversors used for a overload. If no conversor is used for that argument the Symbol
          is not valid.
          */
        static Symbol solve(
                ObjectList<Symbol> candidate_functions,
                Type implicit_argument_type,
                ObjectList<Type> argument_types, 
                const std::string filename,
                int line,
                bool &valid, 
                ObjectList<Symbol>& viable_functions,
                ObjectList<Symbol>& argument_conversor);
    };
};

#endif // TL_OVERLOAD_HPP
