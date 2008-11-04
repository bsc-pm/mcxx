/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2008 Barcelona Supercomputing Center

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

#ifndef TL_AUGMENTED_SYMBOL_H
#define TL_AUGMENTED_SYMBOL_H

#include <set>
#include <string>

#include "tl-symbol.hpp"
#include "tl-ast.hpp"
#include "tl-langconstruct.hpp"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-type.hpp"
#include "cxx-scope.h"

#include "cxx-ss-attrnames.h"
#include "tl-exceptions.hpp"
#include "tl-parameter-region-list.hpp"


namespace TL {
	
	class AugmentedSymbol : public Symbol
	{
		private:
			class SymbolList : public Object {
				private:
					typedef std::set<AugmentedSymbol> list_t;
					list_t *_list;
					
				public:
					typedef list_t::iterator iterator;
					typedef list_t::const_iterator const_iterator;
					
					SymbolList()
						: _list(NULL)
					{
					}
					
					SymbolList(RefPtr<Object> object)
					{
						RefPtr<SymbolList> cast = RefPtr<SymbolList>::cast_dynamic(object);
						if (cast.get_pointer() == NULL)
						{
							if (typeid(*object.get_pointer()) != typeid(Undefined))
							{
								std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
								throw FatalException();
							}
							
							_list = NULL;
						}
						else
						{
							_list = cast->_list;
						}
					}
					
					SymbolList(SymbolList const &symbolList)
						: Object(symbolList), _list(symbolList._list)
					{
					}
					
					void initialize()
					{
						_list = new list_t();
					}
					
					bool is_null() const
					{
						return (_list == NULL);
					}
					
					void insert(AugmentedSymbol const &symbol)
					{
						(*_list).insert(symbol);
					}
					
					void erase(iterator &it)
					{
						(*_list).erase(it);
					}
					
					void erase(Symbol const &symbol)
					{
						(*_list).erase(symbol);
					}
					
					const_iterator begin() const
					{
						return (*_list).begin();
					}
					
					iterator begin()
					{
						return (*_list).begin();
					}
					
					const_iterator end() const
					{
						return (*_list).end();
					}
					
					iterator end()
					{
						return (*_list).end();
					}
					
					void clear()
					{
						(*_list).clear();
					}
					
					size_t size() const
					{
						return (*_list).size();
					}
					
			};
			
		public:
			typedef SymbolList::iterator call_iterator;
			typedef SymbolList::const_iterator const_call_iterator;
			
			AugmentedSymbol(scope_entry_t* symbol)
				: Symbol(symbol)
			{
			}
			
			AugmentedSymbol(Symbol const &symbol)
				: Symbol(symbol)
			{
			}
			
			//! Constructs an AugmentedSymbol from a reference to Object
			AugmentedSymbol(RefPtr<Object> obj)
				: Symbol(obj)
			{
			}
			
			RefPtr<ParameterRegionList> get_parameter_region_list() const
			{
				RefPtr<Object> parameter_region_list( get_attribute(SYMBOL_SUPERSCALAR_PARAMETER_REGION_LIST) );
				return RefPtr<ParameterRegionList>::cast_dynamic(parameter_region_list);
			}
			void set_parameter_region_list(RefPtr<Object> parameter_region_list_ref)
			{
				set_attribute(SYMBOL_SUPERSCALAR_PARAMETER_REGION_LIST, parameter_region_list_ref);
			}
			
			Bool is_task() const
			{
				RefPtr<Object> task_attribute = get_attribute(SYMBOL_SUPERSCALAR_IS_TASK);
				return task_attribute;
			}
			void set_as_task(bool value)
			{
				set_attribute(SYMBOL_SUPERSCALAR_IS_TASK, value);
			}
			
			Bool has_high_priority() const
			{
				RefPtr<Object> attribute = get_attribute(SYMBOL_SUPERSCALAR_HAS_HIGH_PRIORITY);
				return attribute;
			}
			void set_high_priority(bool value)
			{
				RefPtr<Object> ref( new Bool(value) );
				set_attribute(SYMBOL_SUPERSCALAR_HAS_HIGH_PRIORITY, ref);
			}
			
			Bool is_blocking() const
			{
				RefPtr<Object> attribute = get_attribute(SYMBOL_SUPERSCALAR_IS_BLOCKING);
				return attribute;
			}
			void set_blocking(bool value)
			{
				RefPtr<Object> ref( new Bool(value) );
				set_attribute(SYMBOL_SUPERSCALAR_IS_BLOCKING, ref);
			}
			
			Bool has_coherced_sides() const
			{
				RefPtr<Object> attribute = get_attribute(SYMBOL_SUPERSCALAR_HAS_COHERCED_SIDES);
				return attribute;
			}
			void set_coherced_sides(bool value)
			{
				set_attribute(SYMBOL_SUPERSCALAR_HAS_COHERCED_SIDES, value);
			}
			
			Bool is_on_task_side() const
			{
				RefPtr<Object> attribute = get_attribute(SYMBOL_SUPERSCALAR_IS_ON_TASK_SIDE);
				return attribute;
			}
			void set_as_task_side(bool value)
			{
				set_attribute(SYMBOL_SUPERSCALAR_IS_ON_TASK_SIDE, value);
			}
			
			Bool is_on_non_task_side() const
			{
				RefPtr<Object> attribute = get_attribute(SYMBOL_SUPERSCALAR_IS_ON_NON_TASK_SIDE);
				return attribute;
			}
			void set_as_non_task_side(bool value)
			{
				set_attribute(SYMBOL_SUPERSCALAR_IS_ON_NON_TASK_SIDE, value);
			}
			
			Bool calls_to_taskside_coherced_function() const
			{
				RefPtr<Object> attribute = get_attribute(SYMBOL_SUPERSCALAR_CALLS_TO_TASKSIDE_COHERCED_FUNCTION);
				return attribute;
			}
			void set_calls_to_taskside_coherced_function(bool value)
			{
				set_attribute(SYMBOL_SUPERSCALAR_CALLS_TO_TASKSIDE_COHERCED_FUNCTION, value);
			}
			
			void add_caller_function(Symbol caller)
			{
				RefPtr<SymbolList> caller_list = RefPtr<SymbolList>::cast_dynamic(
					get_attribute(SYMBOL_SUPERSCALAR_CALLER_FUNCTIONS)
				);
				if (caller_list.get_pointer() == NULL)
				{
					caller_list = RefPtr<SymbolList> ( new SymbolList() );
					caller_list->initialize();
					set_attribute(SYMBOL_SUPERSCALAR_CALLER_FUNCTIONS, (RefPtr<Object>)caller_list);
				}
				// Insert or replace
				caller_list->insert(caller);
			}
			void add_callee_function(Symbol callee)
			{
				RefPtr<SymbolList> callee_list = RefPtr<SymbolList>::cast_dynamic(
					get_attribute(SYMBOL_SUPERSCALAR_CALLEE_FUNCTIONS)
				);
				if (callee_list.get_pointer() == NULL)
				{
					callee_list = RefPtr<SymbolList> ( new SymbolList() );
					callee_list->initialize();
					set_attribute(SYMBOL_SUPERSCALAR_CALLEE_FUNCTIONS, (RefPtr<Object>)callee_list);
				}
				// Insert or replace
				callee_list->insert(callee);
			}
			
			call_iterator begin_caller_functions()
			{
				RefPtr<SymbolList> caller_list = RefPtr<SymbolList>::cast_dynamic(
					get_attribute(SYMBOL_SUPERSCALAR_CALLER_FUNCTIONS)
				);
				if (caller_list.get_pointer() == NULL)
				{
					caller_list = RefPtr<SymbolList> ( new SymbolList() );
					caller_list->initialize();
					set_attribute(SYMBOL_SUPERSCALAR_CALLER_FUNCTIONS, (RefPtr<Object>)caller_list);
				}
				return caller_list->begin();
			}
			call_iterator end_caller_functions()
			{
				RefPtr<SymbolList> caller_list = RefPtr<SymbolList>::cast_dynamic(
					get_attribute(SYMBOL_SUPERSCALAR_CALLER_FUNCTIONS)
				);
				if (caller_list.get_pointer() == NULL)
				{
					caller_list = RefPtr<SymbolList> ( new SymbolList() );
					caller_list->initialize();
					set_attribute(SYMBOL_SUPERSCALAR_CALLER_FUNCTIONS, (RefPtr<Object>)caller_list);
				}
				return caller_list->end();
			}
			
			call_iterator begin_callee_functions()
			{
				RefPtr<SymbolList> callee_list = RefPtr<SymbolList>::cast_dynamic(
					get_attribute(SYMBOL_SUPERSCALAR_CALLEE_FUNCTIONS)
				);
				if (callee_list.get_pointer() == NULL)
				{
					callee_list = RefPtr<SymbolList> ( new SymbolList() );
					callee_list->initialize();
					set_attribute(SYMBOL_SUPERSCALAR_CALLEE_FUNCTIONS, (RefPtr<Object>)callee_list);
				}
				return callee_list->begin();
			}
			call_iterator end_callee_functions()
			{
				RefPtr<SymbolList> callee_list = RefPtr<SymbolList>::cast_dynamic(
					get_attribute(SYMBOL_SUPERSCALAR_CALLEE_FUNCTIONS)
				);
				if (callee_list.get_pointer() == NULL)
				{
					callee_list = RefPtr<SymbolList> ( new SymbolList() );
					callee_list->initialize();
					set_attribute(SYMBOL_SUPERSCALAR_CALLEE_FUNCTIONS, (RefPtr<Object>)callee_list);
				}
				return callee_list->end();
			}
			
	};
	
}


#endif // TL_AUGMENTED_SYMBOL_H
