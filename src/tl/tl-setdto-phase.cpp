/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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



#include <iostream>
#include <tl-builtin.hpp>
#include <tl-setdto-phase.hpp>
#include <cstring>

namespace TL
{
	SetDTOCompilerPhase::SetDTOCompilerPhase()
		: _variable(), _type(), _value()
	{
	}
	
	// This phase only modifies the DTO in order to show some warnings only once if a phase is executed twice or more times
	void SetDTOCompilerPhase::run(TL::DTO& dto)
	{
		ObjectList<std::string>::iterator it_type = _type.begin();
		ObjectList<std::string>::iterator it_val = _value.begin();
		for (ObjectList<std::string>::iterator it_var = _variable.begin();
				it_var != _variable.end() && it_val != _value.end();
				it_var++, it_type++, it_val++)
		{
			if (*it_type=="bool")
            {
                dto.set_object(*it_var, std::shared_ptr<Bool>(new Bool(atoi((*it_val).c_str()))));
            }
			else if (*it_type=="int")
            {
                dto.set_object(*it_var, std::shared_ptr<Integer>(new Integer(atoi((*it_val).c_str()))));
            }
			else if (*it_type=="string")
            {
                dto.set_object(*it_var, std::shared_ptr<String>(new String(*it_val)));
            }
		}
	}

	// This function expect a data like variable:type:value
	// The only 'type's allowed are bool, int and string
	int SetDTOCompilerPhase::set_dto(const char* data)
	{
		char d[strlen(data)];
		strcpy (d,data);

		// get variable
		char *tokenized_data = strtok(d, ":");
		if (tokenized_data != NULL)
			_variable.append(tokenized_data);
		else return -1;

		// get type
		tokenized_data = strtok (NULL, ":");
		if (tokenized_data != NULL)
		{
			_type.append(tokenized_data);
		}			
		else return -1;

		// get value
		tokenized_data = strtok (NULL, ":");
		if (tokenized_data != NULL)
			_value.append(tokenized_data);
		else return -1;

		return 0;
	}
}
