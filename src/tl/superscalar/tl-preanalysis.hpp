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

#ifndef TL_PREANALYSIS_HPP
#define TL_PREANALYSIS_HPP


#include <map>

#include "tl-compilerphase.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"

#include "tl-augmented-symbol.hpp"
#include "tl-function-table.hpp"


namespace TL
{
	class PreAnalysis : public CompilerPhase
	{
		private:
			typedef std::map<std::string, AugmentedSymbol> function_table_t;
			
			class FunctionDefinitionHandler : public TraverseFunctor
			{
				private:
					FunctionTable &_function_table;
					
				public:
					FunctionDefinitionHandler(FunctionTable &function_table)
						: _function_table(function_table)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class FunctionCallHandler : public TraverseFunctor
			{
				private:
					AugmentedSymbol _caller_function;
					FunctionTable &_function_table;
					
				public:
					FunctionCallHandler(AugmentedSymbol caller_function, FunctionTable &function_table)
						: _caller_function(caller_function), _function_table(function_table)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			static PhaseStatus _status;
			
		public:
			virtual void pre_run(DTO &dto);
			virtual void run(DTO &dto);
			
			static void fail()
			{
				_status = PHASE_STATUS_ERROR;
			}
	};
	
}




#endif // TL_PREANALYSIS_HPP
