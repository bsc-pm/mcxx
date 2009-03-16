/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2007-2009 Barcelona Supercomputing Center

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

#ifndef TL_CODE_CONVERSION_HPP
#define TL_CODE_CONVERSION_HPP

#include <map>
#include <string>

#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-task-table.hpp"
#include "tl-traverse.hpp"
#include "tl-type.hpp"

#include "tl-ast-predicates.hpp"


namespace TL
{
	class CodeConversion : public CompilerPhase
	{
		protected:
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
				public:
					TaskCallHandler()
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class FunctionDefinitionHandler : public TraverseFunctor
			{
				private:
					ObjectList<AST_t> &_kill_list;
					
					bool _generate_task_side;
					bool _generate_non_task_side;
					
					DepthTraverse _body_traverser;
					
					PredicateAttr _function_call_predicate;
					TaskCallHandler _task_call_handler;
					CallToNamedFunctionPredicate _malloc_call_predicate;
					MallocHandler _malloc_handler;
					CallToNamedFunctionPredicate _calloc_call_predicate;
					CallocHandler _calloc_handler;
					
					bool _do_traverse;
					
					void handle_function_body(AST_t node, ScopeLink scope_link);
					
				public:
					FunctionDefinitionHandler(ObjectList<AST_t> &kill_list, ScopeLink scope_link, bool generate_task_side, bool generate_non_task_side, bool align_memory)
						: _kill_list(kill_list),
						_generate_task_side(generate_task_side), _generate_non_task_side(generate_non_task_side),
						_body_traverser(),
                        _function_call_predicate(LANG_IS_FUNCTION_CALL),
						_task_call_handler(),
						_malloc_call_predicate("malloc", scope_link), _malloc_handler(),
						_calloc_call_predicate("calloc", scope_link), _calloc_handler(),
						_do_traverse(generate_non_task_side)
					{
						if (align_memory)
						{
							_body_traverser.add_predicate(_malloc_call_predicate, _malloc_handler);
							_body_traverser.add_predicate(_calloc_call_predicate, _calloc_handler);
						}
						if (generate_non_task_side)
						{
							_body_traverser.add_predicate(_function_call_predicate, _task_call_handler);
						}
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			class DeclarationHandler : public TraverseFunctor
			{
				private:
					ObjectList<AST_t> &_kill_list;
					bool _generate_task_side;
					bool _generate_non_task_side;
					
				public:
					DeclarationHandler(ObjectList<AST_t> &kill_list, bool generate_task_side, bool generate_non_task_side)
						: _kill_list(kill_list),
						_generate_task_side(generate_task_side), _generate_non_task_side(generate_non_task_side)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			
			static PhaseStatus _status;
			
			
			void generate_task_ids(TaskTable &task_table, AST_t translation_unit, ScopeLink scope_link);
			void generate_task_adapters(TaskTable &task_table, AST_t translation_unit, ScopeLink scope_link);
			
			
		public:
			virtual void run(DTO &dto);
			
			static void fail()
			{
				_status = PHASE_STATUS_ERROR;
			}
	};
	
}


#endif // TL_CODE_CONVERSION_HPP
