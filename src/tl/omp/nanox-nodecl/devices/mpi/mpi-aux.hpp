/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef MPI_AUX_HPP
#define MPI_AUX_HPP


//  using namespace TL;
//  using namespace TL::Nanox;
//  
//  
//  // Create a filter to search for function definitions
//  struct FilterFunctionDef : Predicate<AST_t>
//  {
//  private:
//  	Symbol _sym;
//  	ScopeLink _sl;
//  public:
//  	FilterFunctionDef(Symbol sym, ScopeLink sl)
//  	: _sym(sym), _sl(sl) { }
//  
//  	virtual bool do_(const AST_t& a) const
//  	{
//  		if (!FunctionDefinition::predicate(a))
//  			return false;
//  
//  		FunctionDefinition funct_def(a, _sl);
//  
//  		Symbol sym = funct_def.get_function_symbol();
//  		return _sym == sym;
//  	}
//  };
//  
//  
//  // Check if the given path file comes from CUDA installation directory
//  struct CheckIfInCudacompiler
//  {
//  	static bool check(const std::string& path)
//  	{
//  #ifdef CUDA_DIR
//  		std::string cudaPath(CUDA_DIR);
//  #else
//  		std::string cudaPath("???");
//  #endif
//  
//  		if (path.substr(0, cudaPath.size()) == cudaPath)
//  			return true;
//  		else
//  			return false;
//  	}
//  	static bool check_type(TL::Type t)
//  	{
//  		if (t.is_named())
//  		{
//  			return CheckIfInCudacompiler::check(t.get_symbol().get_filename());
//  		}
//  		else if (t.is_pointer())
//  		{
//  			return check_type(t.points_to());
//  		}
//  		else if (t.is_array())
//  		{
//  			return check_type(t.array_element());
//  		}
//  		else if (t.is_function())
//  		{
//  			TL::ObjectList<TL::Type> types = t.parameters();
//  			types.append(t.returns());
//  			for (TL::ObjectList<TL::Type>::iterator it = types.begin(); it != types.end(); it++)
//  			{
//  				if (!check_type(*it))
//  					return false;
//  			}
//  			return true;
//  		}
//  		else
//  		{
//  			return true;
//  		}
//  	}
//  };
//  
//  static std::string param_position_name(int position)
//  {
//  	// Do not modify __tmp_ name unless needed, it has to be the
//  	// same as 'std::stringstream var' in 'instantiate_function_task_info()'
//  	// from omp/fun-tasks/tl-omp-fun-tasks.cpp
//  	// Current value of 'var' in that file: var << "__tmp_" << i;
//  
//  	std::stringstream sstm;
//  	sstm << "__tmp_" << position;
//  	return sstm.str();
//  }
//  
//  /*
//   * Replaces every occurrence of 'from' with 'to' in 'str'
//   */
//  static void replaceAllString(std::string& str, const std::string& from, const std::string& to)
//  {
//  	size_t start_pos = str.find(from);
//  	while (start_pos != std::string::npos)
//  	{
//  		str.replace(start_pos, from.length(), to);
//  		start_pos = str.find(from, start_pos + to.length());
//  	}
//  }
//  
//  /*
//   * Replaces every occurrence of 'from' with 'to' in 'str'
//   * will not replace if previous or next character is alphanumeric or '_'
//   * because that means that it is not our var, but var with longer name than ours
//   */
//  static void replaceAllStringVar(std::string& str, const std::string& from, const std::string& to)
//  {
//  	size_t start_pos = str.find(from);
//  	while (start_pos != std::string::npos)
//  	{
//  		bool replace = true;
//  		const char* previous;
//  		if (start_pos > 0)
//  		{
//  			previous = str.substr(start_pos-1, start_pos).c_str();
//  			if ((previous[0] >= 'a' && previous[0] <= 'z') || (previous[0] >= 'A' && previous[0] <= 'Z')) replace = false;
//  			if ((previous[0] >= '0' && previous[0] <= '9')) replace = false;
//  			if ((previous[0] == '_')) replace = false;
//  		}
//  
//  		size_t end_pos = start_pos + from.length();
//  		if (end_pos < str.size() - 1)
//  		{
//  			previous = str.substr(end_pos, end_pos + 1).c_str();
//  			if ((previous[0] >= 'a' && previous[0] <= 'z') || (previous[0] >= 'A' && previous[0] <= 'Z')) replace = false;
//  			if ((previous[0] >= '0' && previous[0] <= '9')) replace = false;
//  			if ((previous[0] == '_')) replace = false;
//  		}
//  
//  		if (replace)
//  		{
//  			str.replace(start_pos, from.length(), to);
//  			start_pos = str.find(from, start_pos + to.length());
//  		}
//  		else
//  		{
//  			start_pos = str.find(from, start_pos + 1);
//  		}
//  	}
//  }
//  /*
//   * Replaces every occurrence of 'from' with 'to' in 'str'
//   */
//  static void replaceAllRegex(std::string& str, const std::string& from, const std::string& to)
//  {
//  	size_t start_pos = str.find(from);
//  	while (start_pos != std::string::npos)
//  	{
//  		str.replace(start_pos, from.length(), to);
//  		start_pos = str.find(from, start_pos + to.length());
//  	}
//  }
//  /*
//   * Replaces everything inside [ ] brackets, also (), *, &, and after -> and .
//   */
//  static void removePointerSymbolsString(std::string& str)
//  {
//  	replaceAllString(str, "*", "");
//  	replaceAllString(str, "&", "");
//  	replaceAllString(str, "(", "");
//  	replaceAllString(str, ")", "");
//  
//  	size_t start_pos = str.find("[");
//  	size_t end_pos = str.find("]");
//  	while (start_pos != std::string::npos && end_pos != std::string::npos)
//  	{
//  		str.replace(start_pos, end_pos + 1, "");
//  		start_pos = str.find("[");
//  		end_pos = str.find("]");
//  	}
//  
//  	start_pos = str.find("->");
//  	while (start_pos != std::string::npos)
//  	{
//  		str.replace(start_pos, str.length(), "");
//  		start_pos = str.find("->");
//  	}
//  
//  	start_pos = str.find(".");
//  	while (start_pos != std::string::npos)
//  	{
//  		str.replace(start_pos, str.length(), "");
//  		start_pos = str.find(".");
//  	}
//  }
//  
//  /*
//   * Replaces 'from' with 'to' in 'str' once
//   */
//  static void replaceString(std::string& str, const std::string& from, const std::string& to)
//  {
//      size_t start_pos = str.find(from);
//      if (start_pos != std::string::npos)
//      {
//          str.replace(start_pos, from.length(), to);
//      }
//  }




#endif
