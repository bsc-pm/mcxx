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
        /*!
          Represents an environment data item
          */
        class DataEnvironItem
        {
            private:
                Symbol _sym;
                Type _type;
                std::string _field_name;
                bool _is_copy;
                bool _is_raw_buffer;
                bool _is_vla_type;
                bool _is_private;

                ObjectList<Source> _vla_dim_list;

            public:
                DataEnvironItem() 
                    : _sym(NULL), 
                    _type(NULL),
                    _field_name(""), 
                    _is_copy(false),
                    _is_raw_buffer(false),
                    _is_vla_type(false),
                    _is_private(false),
                    _vla_dim_list()
                { }

                //! Creates a data environment item after a symbol, type and name
                /*!
                  \param sym The symbol after this data environment item is created
                  \param type Type of the item, it may be different than the original type of sym
                              if the type of the symbol is not valid for a struct field
                  \param field_name Name of the field in the structur, holding this item
                  */
                DataEnvironItem(Symbol sym, Type type, const std::string &field_name)
                    : _sym(sym), 
                    _type(type),
                    _field_name(field_name),
                    _is_copy(false),
                    _is_raw_buffer(false),
                    _is_vla_type(false),
                    _is_private(false),
                    _vla_dim_list()
                {
                }

                //! Returns the symbol of this item
                Symbol get_symbol() const
                {
                    return _sym;
                }

                //! Returns the type of this item
                Type get_type() const
                {
                    return _type;
                }

                //! Returns the field name of this item
                std::string get_field_name() const
                {
                    return _field_name;
                }

                //! States if this item is to be copied
                bool is_copy() const
                {
                    return _is_copy;
                }

                //! Sets this item to be copied
                DataEnvironItem& set_is_copy(bool b)
                {
                    _is_copy = b;
                    return *this;
                }

                //! Sets this item to be a raw buffer
                DataEnvironItem& set_is_raw_buffer(bool b)
                {
                    _is_raw_buffer = b;
                    return *this;
                }

                //! States if this item is a raw buffer
                bool is_raw_buffer() const
                {
                    return _is_raw_buffer;
                }

                //! States if this item has a VLA type
                bool is_vla_type() const
                {
                    return _is_vla_type;
                }

                //! Sets this item to be a VLA type
                DataEnvironItem& set_is_vla_type(bool b) 
                {
                    _is_vla_type = b;
                    return *this;
                }

                //! Sets dimensions of a VLA type
                DataEnvironItem& set_vla_dimensions(ObjectList<Source> dim_list)
                {
                    _vla_dim_list = dim_list;
                    return *this;
                }

                //! Returns the dimensions of a VLA type
                ObjectList<Source> get_vla_dimensions() const
                {
                    return _vla_dim_list;
                }

                //! Sets the item as private
                void set_is_private(bool b)
                {
                    _is_private = b;
                }

                //! States if the item is private or not
                bool is_private() const
                {
                    return _is_private;
                }
        };

        class DataEnvironInfo
        {
            private:
                ObjectList<DataEnvironItem> _data_env_items;
                ObjectList<OpenMP::CopyItem> _copy_items;

                static bool data_env_item_has_sym(const DataEnvironItem &item)
                {
                    return item.get_symbol().is_valid();
                }
            public:
                DataEnvironInfo() { }

                //! Adds a data environment item to the data environment
                void add_item(const DataEnvironItem& item)
                {
                    _data_env_items.append(item);
                }

                //! Returns the data environment items
                ObjectList<DataEnvironItem> get_items() const
                {
                    return _data_env_items.filter(predicate(data_env_item_has_sym));
                }

                //! Returns the data environment item for a given symbol
                /*
                   \param sym Symbol whose DataEnvironItem is being requested
                 */
                DataEnvironItem get_data_of_symbol(Symbol sym) const
                {
                    if (_data_env_items.contains(functor(&DataEnvironItem::get_symbol), sym))
                    {
                        ObjectList<DataEnvironItem> list = _data_env_items.find(functor(&DataEnvironItem::get_symbol), sym);
                        return list[0];
                    }

                    return DataEnvironItem();
                }

                //! Returns the field name for a given symbol
                std::string get_field_name_for_symbol(Symbol sym) const
                {
                    int n = 0;

                    std::stringstream ss;

                    ss << sym.get_name() << "_" << n++;

                    while (_data_env_items.contains(functor(&DataEnvironItem::get_field_name), ss.str()))
                    {
                        ss.str("");
                        ss << sym.get_name() << "_" << n++;
                    }

                    return ss.str();
                }

                //! States if the environment size is defined at runtime
                bool environment_is_runtime_sized() const
                {
                    ObjectList<DataEnvironItem> data_env_list = get_items();

                    for (ObjectList<DataEnvironItem>::iterator it = data_env_list.begin();
                            it != data_env_list.end();
                            it++)
                    {
                        if (!it->is_private() 
                                && it->is_vla_type())
                            return true;
                    }

                    return false;
                }

                //! Returns an expression with the variable part
                /*!
                  This function is only meaningful if environment_is_runtime_sized returned true
                 */
                Source sizeof_variable_part(Scope sc) const
                {
                    bool first = true;
                    Source result = Source("0");

                    ObjectList<DataEnvironItem> data_env_list = get_items();

                    for (ObjectList<DataEnvironItem>::iterator it = data_env_list.begin();
                            it != data_env_list.end();
                            it++)
                    {
                        if (!it->is_private() 
                                && it->is_vla_type())
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

                void add_copy_item(OpenMP::CopyItem copy_item)
                {
                    _copy_items.append(copy_item);
                }

                ObjectList<OpenMP::CopyItem> get_copy_items() const
                {
                    return _copy_items;
                }
        };

        //! \cond NO_DOCUMENT

        // This one is not to be exported
        void compute_data_environment(
                OpenMP::DataSharingEnvironment& data_sharing,
                ScopeLink scope_link,
                DataEnvironInfo &data_env_info,
                ObjectList<Symbol>& converted_vlas);

        // This one is not to be exported
        void fill_data_environment_structure(
                Scope sc,
                DataEnvironInfo &data_env_info,
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

        //! \endcond
    }
}

#endif // TL_DATA_ENV_HPP
