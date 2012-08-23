/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center
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

#ifndef CUDA_AUX_HPP
#define CUDA_AUX_HPP


// Create a filter to search for function definitions
struct FilterFunctionDef : Predicate<AST_t>
{
private:
	Symbol _sym;
	ScopeLink _sl;
public:
	FilterFunctionDef(Symbol sym, ScopeLink sl)
	: _sym(sym), _sl(sl) { }

	virtual bool do_(const AST_t& a) const
	{
		if (!FunctionDefinition::predicate(a))
			return false;

		FunctionDefinition funct_def(a, _sl);

		Symbol sym = funct_def.get_function_symbol();
		return _sym == sym;
	}
};


// Check if the given path file comes from CUDA installation directory
struct CheckIfInCudacompiler
{
	static bool check(const std::string& path)
	{
#ifdef CUDA_DIR
		std::string cudaPath(CUDA_DIR);
#else
		std::string cudaPath("???");
#endif
		if (path.substr(0, cudaPath.size()) == cudaPath)
			return true;
		else
			return false;
	}
	static bool check_type(TL::Type t)
	{
		if (t.is_named())
		{
			return CheckIfInCudacompiler::check(t.get_symbol().get_filename());
		}
		else if (t.is_pointer())
		{
			return check_type(t.points_to());
		}
		else if (t.is_array())
		{
			return check_type(t.array_element());
		}
		else if (t.is_function())
		{
			TL::ObjectList<TL::Type> types = t.parameters();
			types.append(t.returns());
			for (TL::ObjectList<TL::Type>::iterator it = types.begin(); it != types.end(); it++)
			{
				if (!check_type(*it))
					return false;
			}
			return true;
		}
		else
		{
			return true;
		}
	}
};




#endif
