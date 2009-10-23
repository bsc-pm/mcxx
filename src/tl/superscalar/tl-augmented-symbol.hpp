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
#include "tl-access-bounds-list.hpp"
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
			
			void generate_maximum_source(/* INOUT */ std::set<std::string> &values, /* INOUT */ Source &source)
			{
				std::set<std::string>::iterator first = values.begin();
				Source first_source;
				first_source
					<< *first;
				values.erase(first);
				
				// Base case
				if (values.empty())
				{
					source << first_source;
					return;
				}
				
				// Recursive case
				Source other_source;
				generate_maximum_source(/* INOUT */ values, /* INOUT */ other_source);
				
				source
					<< "(" << first_source << ">" << other_source << "?" << first_source << ":" << other_source << ")";
			}
			
			
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
			
			RefPtr<AccessBoundsList> get_parameter_access_bounds_list()
			{
				RefPtr<Object> object( get_attribute(SYMBOL_SUPERSCALAR_PARAMETER_ACCESS_BOUNDS_LIST) );
				if (typeid(*object.get_pointer()) == typeid(AccessBoundsList))
				{
					return RefPtr<AccessBoundsList>::cast_dynamic(object);
				}
				
				// Lazy creation
				ObjectList<Type> parameter_types = get_type().nonadjusted_parameters();
				RefPtr<ParameterRegionList> parameter_region_lists = get_parameter_region_list();
				
				if (parameter_types.size() != parameter_region_lists->size())
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": " << "Internal compiler error" << std::endl;
					throw FatalException();
				}
				int parameter_count = parameter_types.size();
				
				RefPtr<AccessBoundsList> access_bounds_list(new AccessBoundsList());
				for (int parameter_index = 0; parameter_index < parameter_count; parameter_index++)
				{
					RegionList &regionList = (*parameter_region_lists)[parameter_index];
					int region_count = regionList.size();
					Type const &parameter_type = parameter_types[parameter_index];
					
					access_bounds_list->add(AccessBounds());
					
					if (!parameter_type.is_array())
					{
						continue;
					}
					
					AccessBounds &access_bounds = (*access_bounds_list)[parameter_index];
					int dimension_count = regionList[0].get_dimension_count();
					std::set<std::string> candidate_bounds[dimension_count];
					
					for (int dimension_index=0; dimension_index < dimension_count; dimension_index++)
					{
						for (int region_index=0; region_index < region_count; region_index++)
						{
							// NOTE: Hopefully, the same expression will be prettyprinted identically
							candidate_bounds[dimension_index].insert(regionList[region_index][dimension_index].get_accessed_length().prettyprint());
						}
					}
					
					AST_t a_ref_ast = regionList[0][0].get_accessed_length().get_ast();
					ScopeLink a_scope_link = regionList[0][0].get_accessed_length().get_scope_link();
					for (int dimension_index=0; dimension_index < dimension_count; dimension_index++)
					{
						Source max_accessed_range_source;
						generate_maximum_source(candidate_bounds[dimension_index], max_accessed_range_source);
						AST_t max_accessed_range_ast = max_accessed_range_source.parse_expression(a_ref_ast, a_scope_link);
						Expression max_accessed_range(max_accessed_range_ast, a_scope_link);
						access_bounds.add(max_accessed_range);
					}
				} // For each parameter
				
				set_attribute(SYMBOL_SUPERSCALAR_PARAMETER_ACCESS_BOUNDS_LIST, access_bounds_list);
				return access_bounds_list;
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
