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
            private:
                // Original symbol
                TL::Symbol _sym;

                // Name of the field
                std::string _field_name;
                TL::Type _field_type;

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
            public:
                // OutlineDataItem() 
                //     : _sym(NULL), 
                //     _field_name(""), 
                //     _field_type(NULL),
                //     _is_capture(false)
                // { }

                OutlineDataItem(TL::Symbol symbol, const std::string& field_name)
                    : _sym(symbol), 
                    _field_name(field_name), 
                    _field_type(_sym.get_type())
                {
                }

                //! Returns the symbol of this item
                Symbol get_symbol() const
                {
                    return _sym;
                }

                //! Returns the original type of this item
                Type get_original_type() const
                {
                    return _sym.get_type();
                }

                //! Returns the field name of this item
                std::string get_field_name() const
                {
                    return _field_name;
                }

                //! Returns the field type
                Type get_field_type() const
                {
                    return _field_type;
                }

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

                ObjectList<OutlineDataItem> get_data_items() const
                {
                    return _data_env_items;
                }

                OutlineInfo(Nodecl::NodeclBase environment);
        };
    }
}

#endif // TL_DATA_ENV_HPP
