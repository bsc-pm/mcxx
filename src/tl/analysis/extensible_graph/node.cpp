#include "node.hpp"

namespace TL
{
    Node::Node()
    : _id(-1), _entry_edges(), _exit_edges(), _visited(false)
    {}
    
    Node::Node(int& id, Node_type ntype, Node* outer_graph)
            : _id(++id), _entry_edges(), _exit_edges(), _visited(false)
    {
        set_data("type", ntype);
        if (outer_graph != NULL)
        {
            set_data("outer_graph", outer_graph);
        }
        if (ntype == GRAPH_NODE)
        {
            set_data("entry", new Node(id, BASIC_ENTRY_NODE, NULL));
            int a = -1; set_data("exit", new Node(a, BASIC_EXIT_NODE, NULL));
        }
    }
    
    void Node::erase_entry_edge(Node* source)
    {
        for (ObjectList<Edge*>::iterator it = _entry_edges.begin();
                it != _entry_edges.end();
                it++)
        {
            if ((*it)->get_source() == source)
            {
                std::cout << "Deleting edge between nodes " << source->_id << " and " << _id << std::endl;
                _entry_edges.erase(it);
                break;
            }
        }
        std::cerr << "Trying to delete an non-existent edge " 
                  << "between nodes " << source->_id << " and " << _id << std::endl;
    }
    
    void Node::erase_exit_edge(Node* target)
    {
        for (ObjectList<Edge*>::iterator it = _exit_edges.begin();
                it != _exit_edges.end();
                it++)
        {
            if ((*it)->get_target() == target)
            {
                std::cout << "Deleting edge between nodes " << _id << " and " << target->_id << std::endl;
                _exit_edges.erase(it);
                break;
            }
        }
        std::cerr << "Trying to delete an non-existent edge " 
                  << "between nodes " << _id << " and " << target->_id << std::endl;
    }
    
    int Node::get_id()
    {
        return _id;
    }
    
    void Node::set_id(int id)
    {
        _id = id;
    }
    
    bool Node::is_visited()
    {
        return _visited;
    }
    
    void Node::set_visited(bool visited)
    {
        _visited = visited;
    }
    
    ObjectList<Edge*> Node::get_entry_edges()
    {
        return _entry_edges;
    }
    
    void Node::set_entry_edge(Edge *entry_edge)
    {
        _entry_edges.append(entry_edge);
    }
    
    ObjectList<Edge*> Node::get_exit_edges()
    {
        return _exit_edges;
    }
    
    void Node::set_exit_edge(Edge *exit_edge)
    {
        _exit_edges.append(exit_edge);
    }
    
    Node_type Node::get_node_type()
    {
        if (has_key("type"))
            return get_data <Node_type> ((const std::string) "type");
        else
            return UNCLASSIFIED_NODE;
    }
    
    bool Node::is_basic_node()
    {
        if (has_key("type"))
        {
            Node_type nt = get_data<Node_type>((const std::string) "type");
            if (nt == BASIC_ENTRY_NODE || nt == BASIC_EXIT_NODE ||
                nt == BASIC_NORMAL_NODE || nt == BASIC_LABELED_NODE ||
                nt == BASIC_GOTO_NODE || nt == BASIC_FUNCTION_CALL_NODE ||
                nt == BASIC_PRAGMA_DIRECTIVE_NODE || nt == FLUSH_NODE)
                return true;
            else
                return false;
        }
        else
            return false;
    }
}