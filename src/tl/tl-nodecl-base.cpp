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

#include "tl-nodecl-base.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-utils.h"
#include "cxx-codegen.h"
#include "fortran03-codegen.h"
#include <algorithm>

namespace Nodecl
{
    Nodecl::NodeclBase NodeclBase::no_conv() const
    {
        if (is_null())
            return *this;
        Nodecl::NodeclBase result = *this;
        while (result.is<Nodecl::Conversion>() || result.is<Nodecl::VectorConversion>())
        {
            result = result.as<Nodecl::Conversion>().get_nest();
        }
        return result;
    }

    std::string NodeclBase::prettyprint() const
    {
        const char* result = NULL;
        result = codegen_to_str(_n, nodecl_retrieve_context(_n));

        if (result == NULL)
        {
            return "<<<null codegen>>>";
        }
        else
        {
            return result;
        }
    }

    bool NodeclBase::is_in_list() const
    {
        return Utils::is_in_list(*this);
    }

    void NodeclBase::append_sibling(Nodecl::NodeclBase items) const
    {
        Utils::append_items_after(*this, items);
    }

    void NodeclBase::prepend_sibling(Nodecl::NodeclBase items) const
    {
        Utils::prepend_items_before(*this, items);
    }

    void NodeclBase::replace(Nodecl::NodeclBase new_node) const
    {
        Utils::replace(*this, new_node);
    }

    void* NodeclBase::get_internal_tree_address()
    {
        return & (this->_n.tree);
    }

    static nodecl_t make_list_helper(const TL::ObjectList<NodeclBase>::const_iterator& first, 
            const TL::ObjectList<NodeclBase>::const_iterator& last)
    {
        if (first == last)
        {
            if (first->is<Nodecl::List>())
            {
                return first->get_internal_nodecl();
            }
            else
            {
                return nodecl_make_list_1(first->get_internal_nodecl());
            }
        }
        else
        {
            nodecl_t previous_list =  make_list_helper(first, last - 1);

            if (last->is<Nodecl::List>())
            {
                return nodecl_concat_lists(previous_list, 
                        last->get_internal_nodecl());
            }
            else
            {
                return nodecl_append_to_list(previous_list, 
                        last->get_internal_nodecl());
            }
        }
    }

    TL::ObjectList<NodeclBase> List::to_object_list() const
    {
        TL::ObjectList<NodeclBase> result;
        for (List::const_iterator it = this->begin(); it != this->end(); ++it)
        {
            result.append(*it);
        }
        return result;
    }

    List List::make(const TL::ObjectList<NodeclBase>& list)
    {
        if (list.empty())
            return nodecl_null();
        return make_list_helper(list.begin(), list.end() - 1);
    }

    List List::make(const NodeclBase& item_1)
    {
        return nodecl_make_list_1(item_1.get_internal_nodecl());
    }

    List List::make(const NodeclBase& item_1, const NodeclBase& item_2)
    {
        return nodecl_make_list_2(item_1.get_internal_nodecl(), item_2.get_internal_nodecl());
    }

    List List::make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3)
    {
        return nodecl_make_list_3(item_1.get_internal_nodecl(), item_2.get_internal_nodecl(), item_3.get_internal_nodecl());
    }

    List List::make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3, const NodeclBase& item_4)
    {
        return nodecl_make_list_4(item_1.get_internal_nodecl(), item_2.get_internal_nodecl(), item_3.get_internal_nodecl(), item_4.get_internal_nodecl());
    }

    List List::make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3, const NodeclBase& item_4, const NodeclBase& item_5)
    {
        return nodecl_make_list_5(item_1.get_internal_nodecl(), item_2.get_internal_nodecl(), item_3.get_internal_nodecl(), item_4.get_internal_nodecl(), item_5.get_internal_nodecl());
    }

    List List::make(const NodeclBase& item_1, const NodeclBase& item_2, const NodeclBase& item_3, const NodeclBase& item_4, const NodeclBase& item_5, const NodeclBase& item_6)
    {
        return nodecl_make_list_6(item_1.get_internal_nodecl(), item_2.get_internal_nodecl(), item_3.get_internal_nodecl(), item_4.get_internal_nodecl(), item_5.get_internal_nodecl(), item_6.get_internal_nodecl());
    }
}
