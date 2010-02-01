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

#ifndef TL_DATA_ENV_HPP
#define TL_DATA_ENV_HPP

#include "tl-omp.hpp"
#include "tl-symbol.hpp"
#include <string>

namespace TL
{
    namespace Nanox
    {
        class DataEnvironItem
        {
            private:
                Symbol _sym;
                Type _type;
                std::string _field_name;
                bool _is_pointer;
                bool _is_raw_buffer;

            public:
                DataEnvironItem() 
                    : _sym(NULL), 
                    _type(NULL),
                    _field_name(""), 
                    _is_pointer(false),
                    _is_raw_buffer(false)
                { }

                DataEnvironItem(Symbol sym, Type type, const std::string &field_name)
                    : _sym(sym), 
                    _type(type),
                    _field_name(field_name),
                    _is_pointer(false),
                    _is_raw_buffer(false)
                {
                }

                Symbol get_symbol() const
                {
                    return _sym;
                }

                Type get_type() const
                {
                    return _type;
                }

                std::string get_field_name() const
                {
                    return _field_name;
                }

                DataEnvironItem& set_is_pointer(bool b)
                {
                    _is_pointer = b;
                    return *this;
                }

                bool is_pointer() const
                {
                    return _is_pointer;
                }

                DataEnvironItem& set_is_raw_buffer(bool b)
                {
                    _is_raw_buffer = b;
                    return *this;
                }

                bool is_raw_buffer() const
                {
                    return _is_raw_buffer;
                }
        };

        class DataEnvironInfo
        {
            private:
                ObjectList<DataEnvironItem> _data_env_items;
            public:
                DataEnvironInfo() { }

                void add_item(const DataEnvironItem& item)
                {
                    _data_env_items.append(item);
                }

                void get_items(ObjectList<DataEnvironItem> &data_env_item) const
                {
                    data_env_item = _data_env_items;
                }

                DataEnvironItem get_data_of_symbol(Symbol sym)
                {
                    if (_data_env_items.contains(functor(&DataEnvironItem::get_symbol), sym))
                    {
                        ObjectList<DataEnvironItem> list = _data_env_items.find(functor(&DataEnvironItem::get_symbol), sym);
                        return list[0];
                    }

                    return DataEnvironItem();
                }

                std::string get_field_name_for_symbol(Symbol sym)
                {
                    int n = 0;

                    std::stringstream ss;

                    ss << sym.get_name() << "_" << n++;

                    while (_data_env_items.contains(functor(&DataEnvironItem::get_field_name), ss.str()))
                    {
                        ss.clear();
                        ss  << sym.get_name() << "_" << n++;
                    }

                    return ss.str();
                }
        };

        // This one is not to be exported
        void compute_data_environment(ObjectList<Symbol> value,
                ObjectList<Symbol> shared,
                ScopeLink scope_link,
                DataEnvironInfo &data_env_info);

        // This one is not to be exported
        void fill_data_environment_structure(
                Scope sc,
                const DataEnvironInfo &data_env_info,
                Source &struct_decl,
                Source &struct_fields,
                std::string& struct_arg_type_name,
                ObjectList<OpenMP::DependencyItem> dependencies);

        // This one is not to be exported
        void fill_data_args(const std::string& arg_var_accessor, 
                const DataEnvironInfo& data_env, 
                ObjectList<OpenMP::DependencyItem> dependencies,
                Source& result);

        // This one is not to be exported
        void do_outline_replacements(Statement body,
                const DataEnvironInfo& data_env_info,
                Source &replaced_outline,
                Source &initial_code);

    }
}

#endif // TL_DATA_ENV_HPP
