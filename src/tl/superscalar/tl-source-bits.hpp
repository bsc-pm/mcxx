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




#ifndef TL_SOURCE_BITS_HPP
#define TL_SOURCE_BITS_HPP


#include "tl-augmented-symbol.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"


namespace TL
{
	class SourceBits
	{
		public:
            static Region handle_superscalar_declarator(AST_t ref_tree, 
                    ScopeLink scope_link, 
                    std::string const &declarator_string, 
                    Region::Direction direction,
                    Region::Reduction reduction, AugmentedSymbol &function_symbol, AugmentedSymbol &original_symbol);

			static Expression handle_superscalar_expression(AST_t ref_tree, 
				ScopeLink scope_link,
				std::string const &expression_string,
				Region &region);
			
#if 0
            static ObjectList<Region> handle_superscalar_declarator_list(AST_t ref_tree, 
                    ScopeLink scope_link, 
                    std::string const &declarator_string, 
                    Region::Direction direction,
                    Region::Reduction reduction, Symbol &original_symbol);
#endif

			static ObjectList<Expression> get_array_subscript_list(Type type, AST_t ref_tree, ScopeLink scope_link);
	};
	
}


#endif // TL_SOURCE_BITS_HPP
