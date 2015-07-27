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


#include "cxx-utils.h"
#include "tl-modules.hpp"
#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "fortran03-modules.h"
#include <algorithm>

namespace TL
{
    // Writer

    ModuleWriter::ModuleWriter(TL::Symbol module, const std::string domain)
        : _module(module), _domain(domain)
    {
        ERROR_CONDITION(!_module.is_fortran_module(), "This must be a Fortran module!", 0);
    }
    
    void ModuleWriter::builtin_write(unsigned int i)
    {
        _tl_values.append(tl_unsigned_integer(i));
    }
    
    void ModuleWriter::builtin_write(int i)
    {
        _tl_values.append(tl_integer(i));
    }

    void ModuleWriter::builtin_write(TL::Symbol s)
    {
        _tl_values.append(tl_symbol(s.get_internal_symbol()));
    }

    void ModuleWriter::builtin_write(TL::Type t)
    {
        _tl_values.append(tl_type(t.get_internal_type()));
    }

    void ModuleWriter::builtin_write(const std::string& str)
    {
        _tl_values.append(tl_string(uniquestr(str.c_str())));
    }

    void ModuleWriter::builtin_write(bool b)
    {
        _tl_values.append(tl_bool(b));
    }

    void ModuleWriter::builtin_write(Nodecl::NodeclBase node)
    {
        _tl_values.append( tl_nodecl(node.get_internal_nodecl()) );
    }

    void ModuleWriter::builtin_write(TL::Scope sc)
    {
        _tl_values.append( tl_decl_context(sc.get_decl_context()) );
    }

    void ModuleWriter::commit()
    {
        tl_type_t* tl_type_arr = new tl_type_t[_tl_values.size()];

        std::copy(_tl_values.begin(), _tl_values.end(), tl_type_arr);

        extend_module_info(_module.get_internal_symbol(), _domain.c_str(), 
                _tl_values.size(), tl_type_arr);

        delete[] tl_type_arr;
    }

    // Reader

    ModuleReader::ModuleReader(TL::Symbol module, const std::string& domain)
        : _data(NULL), _cursor(0)
    {
        ERROR_CONDITION(!module.is_fortran_module(), "This must be a Fortran module symbol!\n", 0);

        scope_entry_t* entry = module.get_internal_symbol();

        fortran_modules_data_set_t* extra_module_info = symbol_entity_specs_get_module_extra_info(entry);

        if (extra_module_info != NULL)
        {
            int i;
            for (i = 0; i < extra_module_info->num_data; i++)
            {
                if (std::string(extra_module_info->data[i]->name) == domain)
                {
                    _data = extra_module_info->data[i];
                }
            }
        }
    }

    void ModuleReader::builtin_read(unsigned int& i)
    {
        tl_type_t &t = read_item_from_module();
        
        ERROR_CONDITION(t.kind != TL_UNSIGNED_INTEGER, "Invalid read of unsigned integer", 0);
        i = t.data._unsigned_integer;
    }
    
    void ModuleReader::builtin_read(int& i)
    {
        tl_type_t &t = read_item_from_module();

        ERROR_CONDITION(t.kind != TL_INTEGER, "Invalid read of integer", 0);
        i = t.data._integer;
    }

    void ModuleReader::builtin_read(TL::Symbol& s)
    {
        tl_type_t &t = read_item_from_module();

        ERROR_CONDITION(t.kind != TL_SYMBOL, "Invalid read of symbol", 0);
        s = TL::Symbol(t.data._entry);
    }

    void ModuleReader::builtin_read(TL::Type& t)
    {
        tl_type_t &tl = read_item_from_module();

        ERROR_CONDITION(tl.kind != TL_TYPE, "Invalid read of type", 0);
        t = TL::Type(tl.data._type);
    }

    void ModuleReader::builtin_read(std::string& str)
    {
        tl_type_t &t = read_item_from_module();

        ERROR_CONDITION(t.kind != TL_STRING, "Invalid read of string", 0);
        if (t.data._string != NULL)
        {
            str = t.data._string;
        }
        else
        {
            str.clear();
        }
    }

    void ModuleReader::builtin_read(bool& b)
    {
        tl_type_t &t = read_item_from_module();

        ERROR_CONDITION(t.kind != TL_BOOL, "Invalid read of bool", 0);
        b = t.data._boolean;
    }

    void ModuleReader::builtin_read(Nodecl::NodeclBase& n)
    {
        tl_type_t &t = read_item_from_module();

        ERROR_CONDITION(t.kind != TL_NODECL, "Invalid read of nodecl", 0);
        n = Nodecl::NodeclBase(t.data._nodecl);
    }

    void ModuleReader::builtin_read(TL::Scope &sc)
    {
        tl_type_t &t = read_item_from_module();
        ERROR_CONDITION(t.kind != TL_DECL_CONTEXT, "Invalid read of const decl_context_t*", 0);
        sc = TL::Scope(t.data._decl_context);
    }

    tl_type_t& ModuleReader::read_item_from_module()
    {
        ERROR_CONDITION(_cursor >= _data->num_items, "Trying to read past the last item", 0);
        tl_type_t& result = _data->items[_cursor];
        _cursor++;
        return result;
    }

    bool ModuleReader::empty() const
    {
        return _data == NULL || (_cursor >= _data->num_items);
    }
}
