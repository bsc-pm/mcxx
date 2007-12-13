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


#include <string>

#include "tl-function-router.hpp"


namespace TL
{

	void TL::FunctionRouter::propagate_side_cohercion(RefPtr<FunctionMap> function_map, bool is_on_task_side, bool is_on_non_task_side, std::string const &task_side_function_name)
	{
		FunctionInfo &function_info = (*function_map.get_pointer())[task_side_function_name];
		
		if (function_info._has_coherced_sides)
		{
			// End of recursion
			return;
		}
		else
		{
			function_info._is_on_task_side = is_on_task_side;
			function_info._is_on_non_task_side = is_on_non_task_side;
			for (std::set<std::string>::iterator it = function_info._called_functions.begin(); it != function_info._called_functions.end(); it++)
			{
				std::string const &called_function = *it;
				propagate_side_cohercion(function_map, is_on_task_side, is_on_non_task_side, called_function);
			}
		}
	}
	
	
	void TL::FunctionRouter::mark_task_side_recursively(RefPtr<FunctionMap> function_map, std::string const &task_side_function_name)
	{
		FunctionInfo &function_info = (*function_map.get_pointer())[task_side_function_name];
		
		if (function_info._is_on_task_side || function_info._has_coherced_sides)
		{
			// End of recursion
			return;
		}
		else
		{
			function_info._is_on_task_side = true;
			for (std::set<std::string>::iterator it = function_info._called_functions.begin(); it != function_info._called_functions.end(); it++)
			{
				std::string const &called_function = *it;
				mark_task_side_recursively(function_map, called_function);
			}
		}
	}
	
	
	void TL::FunctionRouter::mark_non_task_side_recursively(RefPtr<FunctionMap> function_map, std::string const &non_task_side_function_name)
	{
		FunctionInfo &function_info = (*function_map.get_pointer())[non_task_side_function_name];
		
		if (function_info._is_task || function_info._is_on_non_task_side /* || function_info._has_coherced_sides */)
		{
			// End of recursion
			return;
		}
		else
		{
			function_info._is_on_non_task_side = true;
			for (std::set<std::string>::iterator it = function_info._called_functions.begin(); it != function_info._called_functions.end(); it++)
			{
				std::string const &called_function = *it;
				mark_non_task_side_recursively(function_map, called_function);
			}
		}
	}
	
	
	ObjectList<std::string> TL::FunctionRouter::get_coherced_side_function_names(RefPtr<FunctionMap> function_map) const
	{
		ObjectList<std::string> result;
		
		for (FunctionMap::iterator it = function_map->begin(); it != function_map->end(); it++)
		{
			FunctionInfo &function_info = it->second;
			if (function_info._has_coherced_sides)
			{
				result.push_back(function_info._name);
			}
		}
		
		return result;
	}
	
	
	ObjectList<std::string> TL::FunctionRouter::get_task_names(RefPtr<FunctionMap> function_map) const
	{
		ObjectList<std::string> result;
		
		for (FunctionMap::iterator it = function_map->begin(); it != function_map->end(); it++)
		{
			FunctionInfo &function_info = it->second;
			if (function_info._is_on_task_side)
			{
				result.push_back(function_info._name);
			}
		}
		
		return result;
	}
	
	
	ObjectList<std::string> TL::FunctionRouter::get_non_called_functions(RefPtr<FunctionMap> function_map) const
	{
		ObjectList<std::string> result;
		
		for (FunctionMap::iterator it = function_map->begin(); it != function_map->end(); it++)
		{
			FunctionInfo &function_info = it->second;
			if (function_info._caller_functions.empty())
			{
				result.push_back(function_info._name);
			}
		}
		
		return result;
	}
	
	
	void TL::FunctionRouter::run(DTO &dto)
	{
		RefPtr<FunctionMap> function_map = RefPtr<FunctionMap>::cast_dynamic(dto["superscalar_function_table"]);
		
		// Propagate task and non-task user specifiers from functions to their called functions
		ObjectList<std::string> coherced_side_function_names = get_coherced_side_function_names(function_map);
		for (ObjectList<std::string>::iterator it = coherced_side_function_names.begin(); it != coherced_side_function_names.end(); it++)
		{
			std::string const &function_name = *it;
			FunctionInfo &function_info = (*function_map.get_pointer())[function_name];
			for (std::set<std::string>::iterator it2 = function_info._called_functions.begin(); it2 != function_info._called_functions.end(); it2++)
			{
				std::string const &called_function_name = *it2;
				propagate_side_cohercion(function_map, function_info._is_on_task_side, function_info._is_on_non_task_side, called_function_name);
			}
		}
		
		// Mark task side
		ObjectList<std::string> task_names = get_task_names(function_map);
		for (ObjectList<std::string>::iterator it = task_names.begin(); it != task_names.end(); it++)
		{
			std::string const &task_name = *it;
			mark_task_side_recursively(function_map, task_name);
		}
		
		// Mark non task side from non called functions
		ObjectList<std::string> non_called_functions = get_non_called_functions(function_map);
		for (ObjectList<std::string>::iterator it = non_called_functions.begin(); it != non_called_functions.end(); it++)
		{
			std::string const &function_name = *it;
			mark_non_task_side_recursively(function_map, function_name);
		}
		
		// Mark the rest as non task side
		for (FunctionMap::iterator it = function_map->begin(); it != function_map->end(); it++)
		{
			FunctionInfo &function_info = it->second;
			if (!function_info._is_on_task_side && !function_info._is_on_non_task_side)
			{
				function_info._is_on_non_task_side = true;
			}
		}
	}

}


EXPORT_PHASE(TL::FunctionRouter);

