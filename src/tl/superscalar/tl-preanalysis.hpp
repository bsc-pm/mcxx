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


#include "tl-compilerphase.hpp"
#include "tl-objectlist.hpp"
#include "tl-predicateutils.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"

#include "tl-function-data.hpp"


namespace TL
{
	class PreAnalysis : public CompilerPhase
	{
		private:
			class FunctionDefinitionHandler : public TraverseFunctor
			{
				private:
					FunctionMap _function_map;
					
				public:
					FunctionDefinitionHandler(FunctionMap function_map)
						: _function_map(function_map)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class FunctionDeclarationHandler : public TraverseFunctor
			{
				private:
					FunctionMap _function_map;
					
				public:
					FunctionDeclarationHandler(FunctionMap function_map)
						: _function_map(function_map)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class FunctionCallHandler : public TraverseFunctor
			{
				private:
					FunctionInfo &_function_info;
					FunctionMap _function_map;
					
				public:
					FunctionCallHandler(FunctionInfo &function_info, FunctionMap function_map)
						: _function_info(function_info), _function_map(function_map)
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
