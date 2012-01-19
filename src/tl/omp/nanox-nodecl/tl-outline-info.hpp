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



#ifndef TL_DATA_ENV_HPP
#define TL_DATA_ENV_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include <string>
#include <sstream>

namespace TL
{
    namespace Nanox
    {
        /*!
          Represents an environment data item
          */
        class OutlineDataItem
        {
            public:
                enum Sharing
                {
                    SHARING_UNDEFINED = 0,
                    SHARING_SHARED,
                    SHARING_CAPTURE,
                    SHARING_PRIVATE,
                };

                // -- FIXME -- Think this a bit more
                // This is similar to Transfer but it does not involve copies
                // between devices this is only useful for lastprivate and
                // reduction
                enum Flow
                {
                    FLOW_NONE = 0,
                    FLOW_LAST_VALUE,
                    FLOW_REDUCED_VALUE, 
                };

                enum Directionality
                {
                    DIRECTIONALITY_NONE = 0,
                    DIRECTIONALITY_INPUT,
                    DIRECTIONALITY_OUTPUT,
                    DIRECTIONALITY_INOUT
                };

                enum Transfer
                {
                    COPY_NONE = 0,
                    COPY_IN,
                    COPY_OUT,
                    COPY_INOUT
                };

                enum AllocationPolicyFlags
                {
                    ALLOCATION_POLICY_NONE = 0,
                    ALLOCATION_POLICY_OVERALLOCATED        = 1 << 1,
                    ALLOCATION_POLICY_TASK_MUST_DESTROY    = 1 << 2,
                    ALLOCATION_POLICY_TASK_MUST_DEALLOCATE = 1 << 3,
                };

                enum ValueKind
                {
                    VALUE_KIND_NORMAL = 0,
                    VALUE_KIND_CAST_IN_TASK
                };
            private:
                // Original symbol
                TL::Symbol _sym;

                // Name of the field
                std::string _field_name;
                TL::Type _field_type;

                TL::Type _in_outline_type;

                Sharing _sharing;

                // -- FIXME --
                // Reductions

                // -- FIXME ---
                // Nested data items (VLA and other crazy stuff)
                // ObjectList<OutlineDataItem> _nested_data;
                
                // -- FIXME ---
                // Dependences
                Directionality _directionality;
                
                // -- FIXME ---
                // Copies
                Transfer _transfer;

                AllocationPolicyFlags _allocation_policy_flags;
                
                ValueKind _value_kind;
            public:
                OutlineDataItem(TL::Symbol symbol, const std::string& field_name)
                    : _sym(symbol), 
                    _field_name(field_name), 
                    _field_type(_sym.get_type()),
                    _in_outline_type(NULL),
                    _sharing(),
                    _directionality(),
                    _transfer(),
                    _allocation_policy_flags(),
                    _value_kind()
                {
                }

                //! Returns the symbol of this item
                Symbol get_symbol() const
                {
                    return _sym;
                }

                //! Returns the field name of this item
                std::string get_field_name() const
                {
                    return _field_name;
                }

                // Returns the type used in the outline code
                // or the field type if not defined
                Type get_in_outline_type() const
                {
                    if (_in_outline_type.is_valid())
                        return _in_outline_type;
                    else
                        return _field_type;
                }

                // Sets a type to be used in the outline
                // It may be a different type to the field one
                void set_in_outline_type(Type t) 
                {
                    _in_outline_type = t;
                }

                // Returns the type used in the structure
                Type get_field_type() const
                {
                    return _field_type;
                }

                // Sets the type used in the structure
                void set_field_type(Type t)
                {
                    _field_type = t;
                }

                bool is_capture() const
                {
                    return _sharing == SHARING_CAPTURE;
                }

                bool is_shared() const
                {
                    return _sharing == SHARING_SHARED;
                }

                bool is_private() const
                {
                    return _sharing == SHARING_PRIVATE;
                }

                void set_sharing(Sharing s)
                {
                    _sharing = s;
                }

                Sharing get_sharing() const
                {
                    return _sharing;
                }

                void set_allocation_policy(AllocationPolicyFlags allocation_policy_flags)
                {
                    _allocation_policy_flags = allocation_policy_flags;
                }

                AllocationPolicyFlags get_allocation_policy() const
                {
                    return _allocation_policy_flags;
                }

                ValueKind get_value_kind() const
                {
                    return _value_kind;
                }

                void set_value_kind(ValueKind value_kind)
                {
                    _value_kind = value_kind;
                }
        };

        class OutlineInfo
        {
            private:
                ObjectList<OutlineDataItem> _data_env_items;
            
                // -- FIXME --
                // Devices!

                std::string get_field_name(std::string name);
            public:
                OutlineDataItem& get_entity_for_symbol(TL::Symbol sym);

                ObjectList<OutlineDataItem>& get_data_items()
                {
                    return _data_env_items;
                }

                const ObjectList<OutlineDataItem>& get_data_items() const
                {
                    return _data_env_items;
                }

                OutlineInfo(Nodecl::NodeclBase environment);
        };
    }
}

#endif // TL_DATA_ENV_HPP
