/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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


#ifndef TL_CODE_CONVERSION_HPP
#define TL_CODE_CONVERSION_HPP

#include <map>
#include <string>

#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-type.hpp"

#include "tl-ast-predicates.hpp"
#include "tl-function-data.hpp"


namespace TL
{
	class CodeConversion : public CompilerPhase
	{
		protected:
			static std::string basic_type_to_enum_string(Type const &type);
			
			class MallocHandler : public TraverseFunctor
			{
				public:
					MallocHandler()
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class CallocHandler : public TraverseFunctor
			{
				public:
					CallocHandler()
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class TaskCallHandler : public TraverseFunctor
			{
				private:
					FunctionMap _function_map;
					
				public:
					TaskCallHandler(FunctionMap function_map)
						: _function_map(function_map)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class FunctionDefinitionHandler : public TraverseFunctor
			{
				private:
					FunctionMap _function_map;
					ObjectList<AST_t> &_kill_list;
					
					bool _generate_task_side;
					bool _generate_non_task_side;
					
					DepthTraverse _body_traverser;
					
					PredicateAttr _function_call_predicate(LANG_IS_FUNCTION_CALL) ;
					TaskCallHandler _task_call_handler;
					CallToNamedFunctionPredicate _malloc_call_predicate;
					MallocHandler _malloc_handler;
					CallToNamedFunctionPredicate _calloc_call_predicate;
					CallocHandler _calloc_handler;
					
					bool _do_traverse;
					
					void handle_function_body(AST_t node, ScopeLink scope_link);
					
				public:
					FunctionDefinitionHandler(FunctionMap function_map, ObjectList<AST_t> &kill_list, ScopeLink scope_link, bool generate_task_side, bool generate_non_task_side, bool align_memory)
						: _function_map(function_map), _kill_list(kill_list),
						_generate_task_side(generate_task_side), _generate_non_task_side(generate_non_task_side),
						_body_traverser(),
						_function_call_predicate(), _task_call_handler(function_map),
						_malloc_call_predicate("malloc", scope_link), _malloc_handler(),
						_calloc_call_predicate("calloc", scope_link), _calloc_handler(),
						_do_traverse(generate_non_task_side)
					{
						if (generate_non_task_side)
						{
							_body_traverser.add_predicate(_function_call_predicate, _task_call_handler);
						}
						if (align_memory)
						{
							_body_traverser.add_predicate(_malloc_call_predicate, _malloc_handler);
							_body_traverser.add_predicate(_calloc_call_predicate, _calloc_handler);
						}
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class TaskDeclarationHandler
			{
				private:
					FunctionMap _function_map;
					Context _ctx;
					AST_t _declaration_node;
					
					bool _generate_task_side;
					bool _generate_non_task_side;
					
				public:
					TaskDeclarationHandler(FunctionMap function_map, Context ctx, AST_t declaration_node, bool generate_task_side, bool generate_non_task_side)
						: _function_map(function_map), _ctx(ctx), _declaration_node(declaration_node),
						_generate_task_side(generate_task_side), _generate_non_task_side(generate_non_task_side)
					{
					}
					
					void preorder(Context ctx, AST_t node, FunctionInfo &function_info);
					void postorder(Context ctx, AST_t node, FunctionInfo &function_info);
			};
			
			class DeclarationHandler : public TraverseFunctor
			{
				private:
					FunctionMap _function_map;
					ObjectList<AST_t> &_kill_list;
					bool _generate_task_side;
					bool _generate_non_task_side;
					
				public:
					DeclarationHandler(FunctionMap function_map, ObjectList<AST_t> &kill_list, bool generate_task_side, bool generate_non_task_side)
						: _function_map(function_map), _kill_list(kill_list),
						_generate_task_side(generate_task_side), _generate_non_task_side(generate_non_task_side)
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


#endif // TL_CODE_CONVERSION_HPP
