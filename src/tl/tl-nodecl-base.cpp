#include "tl-nodecl-base.hpp"
#include "tl-nodecl-alg.hpp"
#include "cxx-utils.h"
#include "cxx-codegen.h"
#include "fortran03-codegen.h"
#include <algorithm>

namespace Nodecl
{
    NodeclBase::NodeclBase(TL::RefPtr<TL::Object> obj)
         : _n(nodecl_null())
    {
        TL::RefPtr<Nodecl::NodeclBase> pint = TL::RefPtr<Nodecl::NodeclBase>::cast_dynamic(obj);
        if (pint.get_pointer() != NULL)
        {
            this->_n = pint->_n;
        }
        else
        {
            if (typeid(*obj.get_pointer()) != typeid(TL::Undefined))
            {
                std::cerr << "Bad initialization of Nodecl" << std::endl;
            }
        }
    }

    std::string NodeclBase::prettyprint()
    {
        const char* result = NULL;
        result = codegen_to_str(_n);

        if (result == NULL)
        {
            return "<<<null codegen>>>";
        }
        else
        {
            return result;
        }
    }

    void NodeclBase::integrate(Nodecl::NodeclBase new_node) const
    {
        if (!this->is<Nodecl::List>())
        {
            if (!new_node.is<Nodecl::List>())
            {
                // If none is a list, this is a plain replacement
                this->replace(new_node);
            }
            else
            {
                Nodecl::NodeclBase parent = this->get_parent();
                if (!parent.is<Nodecl::List>())
                {
                    internal_error("Cannot integrate a list into a node if the parent of the latter is not a list", 0);
                }
                Nodecl::List enclosing_list = Utils::get_all_list_from_list_node(parent.as<Nodecl::List>());

                Nodecl::List::iterator it = std::find(enclosing_list.begin(), enclosing_list.end(), *this);
                ERROR_CONDITION(it == enclosing_list.end(), "Node not found in its list!", 0);

                Nodecl::List new_list = new_node.as<Nodecl::List>();

                for (Nodecl::List::iterator new_it = new_list.begin();
                        new_it != new_list.end();
                        new_it++)
                {
                    enclosing_list.insert(it, *new_it);
                }

                Utils::remove_from_enclosing_list(*it);
            }
        }
        else
        {
            if (new_node.is<Nodecl::List>())
            {
                // If both are lists, this is a plain replacement
                this->replace(new_node);
            }
            else
            {
                // Replace using a singleton list
                TL::ObjectList<Nodecl::NodeclBase> single;
                single.append(new_node);

                Nodecl::List new_list = Nodecl::List::make(new_node);

                this->replace(new_list);
            }
        }
    }

    void NodeclBase::replace(Nodecl::NodeclBase new_node) const
    {
        nodecl_exchange(this->_n, new_node._n);
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
            return nodecl_make_list_1(first->get_internal_nodecl());
        }
        else
        {
            nodecl_t previous_list =  make_list_helper(first, last - 1);

            return nodecl_append_to_list(previous_list, 
                    last->get_internal_nodecl());
        }
    }

    List List::make(const TL::ObjectList<NodeclBase>& list)
    {
        if (list.empty())
            return nodecl_null();
        return make_list_helper(list.begin(), list.end() - 1);
    }

    List List::make(const NodeclBase& list)
    {
        return nodecl_make_list_1(list.get_internal_nodecl());
    }

}
