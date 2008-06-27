/*
    SMP superscalar Compiler
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
#ifndef TL_FUNCTION_DATA_HPP
#define TL_FUNCTION_DATA_HPP


#include <map>
#include <set>
#include <string>
#include <typeinfo>

#include "tl-object.hpp"
#include "tl-langconstruct.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"


namespace TL
{
	enum ParameterDirection
	{
		UNKNOWN_DIR=0,
		INPUT_DIR,
		OUTPUT_DIR,
		INOUT_DIR
	};
	
	class ParameterInfo
	{
		public:
			Symbol _symbol; // This is only used for tasks
			Type _definition_type;
			Type _declaration_type;
			Type _augmented_definition_type;
			Type _augmented_declaration_type;
			std::string _definition_locus;
			std::string _declaration_locus;
			std::string _augmented_definition_locus;
			std::string _augmented_declaration_locus;
			unsigned int _directionality_declaration_count;
			unsigned int _directionality_definition_count;
			ParameterDirection _direction;
			
			ParameterInfo()
				: _symbol(Symbol::invalid())
				, _definition_type(NULL), _declaration_type(NULL), _augmented_definition_type(NULL), _augmented_declaration_type(NULL)
				, _directionality_declaration_count(0), _directionality_definition_count(0)
				, _direction(UNKNOWN_DIR)
				{
				}
	};
	
	class FunctionInfo
	{
		public:
			std::string _name;
			std::set<std::string> _caller_functions;
			std::set<std::string> _called_functions;
			bool _is_task;
			bool _has_high_priority;
			bool _has_coherced_sides;
			bool _calls_to_taskside_coherced_function;
			bool _is_on_task_side;
			bool _is_on_non_task_side;
			ObjectList<ParameterInfo> _parameters;
			bool _has_ellipsis;
			bool _has_incomplete_prototype;
			Scope _definition_scope;
			Scope _declaration_scope;
			unsigned int _definition_count;
			unsigned int _declaration_count;
			unsigned int _task_definition_count;
			unsigned int _task_declaration_count;
			unsigned int _task_declarations_processed;
			std::string _definition_locus;
			std::string _declaration_locus;
			std::string _task_definition_locus;
			std::string _task_declaration_locus;
			bool _has_errors;
		
			FunctionInfo()
				: _name(), _caller_functions(), _called_functions(), _is_task(false), _has_high_priority(false), 
				_has_coherced_sides(false), _calls_to_taskside_coherced_function(false),
				_is_on_task_side(false), _is_on_non_task_side(false), _parameters(), _has_ellipsis(false),
				_has_incomplete_prototype(false),
				_definition_scope(), _declaration_scope(),
				_definition_count(0), _declaration_count(0), _task_definition_count(0), _task_declaration_count(0),
				_task_declarations_processed(0),
				_has_errors(false)
				{
				}
			
	};
	
	class FunctionMap : public Object
	{
		private:
			std::map<std::string, FunctionInfo> *_contents;
			
		public:
			typedef std::map<std::string, FunctionInfo>::iterator iterator;
			
			FunctionMap()
				: _contents(NULL)
			{
			}
			
			FunctionMap(std::map<std::string, FunctionInfo> *contents)
				: _contents(contents)
			{
			}
			
			FunctionMap(FunctionMap const & function_map)
				: Object(function_map), _contents(function_map._contents)
			{
			}
			
			FunctionMap(RefPtr<Object> object)
			{
				RefPtr<FunctionMap> function_map = RefPtr<FunctionMap>::cast_dynamic(object);
				if (function_map.get_pointer() != NULL)
				{
					_contents = function_map->_contents;
				}
				else
				{
					if (typeid(*object.get_pointer()) != typeid(Undefined))
					{
						std::cerr << "Bad initialization for FunctionMap" << std::endl;
					}
					_contents = NULL;
				}
			}
			
			void initialize()
			{
				_contents = new std::map<std::string, FunctionInfo>();
			}
			
			iterator begin()
			{
				return _contents->begin();
			}
			
			iterator end()
			{
				return _contents->end();
			}
			
			iterator find(std::string const &name)
			{
				return _contents->find(name);
			}
			
			FunctionInfo &operator[](std::string const &name)
			{
				return (*_contents)[name];
			}
			
			bool contains(std::string const &name)
			{
				return (_contents->find(name) != _contents->end());
			}
	};
}


#endif // TL_FUNCTION_DATA_HPP
