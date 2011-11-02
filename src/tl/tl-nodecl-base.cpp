#include "tl-nodecl-base.hpp"
#include "cxx-utils.h"
#include "cxx-codegen.h"
#ifdef FORTRAN_SUPPORT
#include "fortran03-codegen.h"
#endif

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

    void NodeclBase::replace(Nodecl::NodeclBase new_node)
    {
        nodecl_exchange(this->_n, new_node._n);
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
}
