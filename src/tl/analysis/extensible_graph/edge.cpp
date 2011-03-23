#include "edge.hpp"

namespace TL
{
    Edge::Edge(Node *source, Node *target, Edge_type type, std::string label)
        :_source(source), _target(target)
    {
        set_data <Edge_type> ("type", type);
        set_data <std::string> ("label", label);
    }

    Node* Edge::get_source()
    {
        return _source;
    }

    Node* Edge::get_target()
    {
        return _target;
    }

    Edge_type Edge::get_type()
    {
        if (has_key("type"))
        {    
            return get_data <Edge_type> ((const std::string) "type");
        }
        else
        {    
            return UNCLASSIFIED_EDGE;
        }
    }

    std::string Edge::get_label()
    {
        if (has_key("type") && 
            get_data <Edge_type> ((const std::string) "type") != UNCLASSIFIED_EDGE)
        {
            Edge_type etype = get_data<Edge_type>((const std::string) "type");
            if (etype == TRUE_EDGE)
            {    
                return "True";
            }
            else if (etype == FALSE_EDGE)
            {    
                return "False";
            }
            else if (etype == ALWAYS_EDGE)
            {    
                return "";
            }
            else if (etype == CASE_EDGE || etype == CATCH_EDGE)
            {    
                return get_data<std::string>(std::string("label"));
            }
            else if (etype == PARALLEL_EDGE)
            {    
                return "Parallel";
            }
            else if (etype == CONFLICT_EDGE)
            {    
                return "Conflict";
            }
            else
            {
                std::cout << "Unexpected type of edge while getting the Edge label: '" << etype << "'" << std::endl;
            }
                
        }
        else
        {    
            return "";
        }
    }
}