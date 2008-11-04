/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2007 Barcelona Supercomputing Center

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef TL_FUNCTION_ROUTER_HPP
#define TL_FUNCTION_ROUTER_HPP


#include <string>

#include "tl-compilerphase.hpp"

#include "tl-augmented-symbol.hpp"


namespace TL
{
	class FunctionRouter : public CompilerPhase
	{
		private:
			void propagate_side_cohercion(AugmentedSymbol &caller, AugmentedSymbol &callee);
			void mark_task_side_recursively(AugmentedSymbol &symbol);
			void mark_non_task_side_recursively(AugmentedSymbol &symbol);
			
		public:
			virtual void run(DTO &dto);
	};
	
}


#endif // TL_FUNCTION_ROUTER_HPP
