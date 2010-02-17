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
                bool _is_copy;
                bool _is_raw_buffer;
                bool _is_vla_type;

                ObjectList<Source> _vla_dim_list;

            public:
                DataEnvironItem() 
                    : _sym(NULL), 
                    _type(NULL),
                    _field_name(""), 
                    _is_copy(false),
                    _is_raw_buffer(false),
                    _is_vla_type(false),
                    _vla_dim_list()
                { }

                DataEnvironItem(Symbol sym, Type type, const std::string &field_name)
                    : _sym(sym), 
                    _type(type),
                    _field_name(field_name),
                    _is_copy(false),
                    _is_raw_buffer(false),
                    _is_vla_type(false),
                    _vla_dim_list()
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

                bool is_copy() const
                {
                    return _is_copy;
                }

                DataEnvironItem& set_is_copy(bool b)
                {
                    _is_copy = b;
                    return *this;
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

                bool is_vla_type() const
                {
                    return _is_vla_type;
                }

                DataEnvironItem& set_is_vla_type(bool b) 
                {
                    _is_vla_type = b;
                    return *this;
                }

                DataEnvironItem& set_vla_dimensions(ObjectList<Source> dim_list)
                {
                    _vla_dim_list = dim_list;
                    return *this;
                }

                ObjectList<Source> get_vla_dimensions() const
                {
                    return _vla_dim_list;
                }
        };

        class DataEnvironInfo
        {
            private:
                ObjectList<DataEnvironItem> _data_env_items;

                static bool data_env_item_has_sym(const DataEnvironItem &item)
                {
                    return item.get_symbol().is_valid();
                }
            public:
                DataEnvironInfo() { }

                void add_item(const DataEnvironItem& item)
                {
                    _data_env_items.append(item);
                }

                ObjectList<DataEnvironItem> get_items() const
                {
                    return _data_env_items.filter(predicate(data_env_item_has_sym));
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

                bool environment_is_runtime_sized() const
                {
                    ObjectList<DataEnvironItem> data_env_list = get_items();

                    for (ObjectList<DataEnvironItem>::iterator it = data_env_list.begin();
                            it != data_env_list.end();
                            it++)
                    {
                        if (it->is_vla_type())
                            return true;
                    }

                    return false;
                }

                Source sizeof_variable_part(Scope sc) const
                {
                    bool first = true;
                    Source result = Source("0");

                    ObjectList<DataEnvironItem> data_env_list = get_items();

                    for (ObjectList<DataEnvironItem>::iterator it = data_env_list.begin();
                            it != data_env_list.end();
                            it++)
                    {
                        if (it->is_vla_type())
                        {
                            Type base_type = it->get_symbol().get_type().basic_type();

                            if (first)
                            {
                                result = Source() << "sizeof(" << base_type.get_declaration(sc, "") << ") " 
                                    ;
                            }
                            else
                            {
                                result << "* sizeof(" << base_type.get_declaration(sc, "") << ") " 
                                    ;
                            }

                            ObjectList<Source> dim_list = it->get_vla_dimensions();

                            for (ObjectList<Source>::iterator it = dim_list.begin();
                                    it != dim_list.end();
                                    it++)
                            {
                                result << " * " << *it;
                            }
                        }
                    }

                    return result;
                }
        };

        // This one is not to be exported
        void compute_data_environment(ObjectList<Symbol> value,
                ObjectList<Symbol> shared,
                ScopeLink scope_link,
                DataEnvironInfo &data_env_info,
                ObjectList<Symbol>& converted_vlas);

        // This one is not to be exported
        void fill_data_environment_structure(
                Scope sc,
                const DataEnvironInfo &data_env_info,
                Source &struct_decl,
                Source &struct_fields,
                std::string& struct_arg_type_name,
                ObjectList<OpenMP::DependencyItem> dependencies);

        // This one is not to be exported
        void fill_data_args(
                const std::string& arg_var_name,
                const DataEnvironInfo& data_env, 
                ObjectList<OpenMP::DependencyItem> dependencies,
                bool is_pointer_struct,
                Source& result);

        // This one is not to be exported
        void do_outline_replacements(Statement body,
                const DataEnvironInfo& data_env_info,
                Source &replaced_outline,
                Source &initial_code);

    }
}

#endif // TL_DATA_ENV_HPP
