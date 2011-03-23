#ifndef NODE_HPP
#define NODE_HPP

#include "edge.hpp"
#include "tl-ast.hpp"
#include "tl-builtin.hpp"
#include "tl-objectlist.hpp"
#include "tl-statement.hpp"

namespace TL 
{
    enum Node_type {
        // BASIC (CFG / PCFG)
        BASIC_ENTRY_NODE,
        BASIC_EXIT_NODE,
        BASIC_NORMAL_NODE,
        BASIC_LABELED_NODE,
        BASIC_GOTO_NODE,
        BASIC_FUNCTION_CALL_NODE,
        BASIC_PRAGMA_DIRECTIVE_NODE,
        // BASIC (PCFG)
        FLUSH_NODE,
        // CFG
        GRAPH_NODE,
        // PCFG
        BARRIER_NODE,
        COMPOSITE_NODE,
        SUPER_NODE,
        // Unclassified nodes
        UNCLASSIFIED_NODE
    };

    class Edge;
    
    class LIBTL_CLASS Node : public LinkData {
        private:
            int _id;
            ObjectList<Edge*> _entry_edges;
            ObjectList<Edge*> _exit_edges;
            bool _visited;
            
        public:
            // *** Constructors *** //
            Node();
            Node(int& id, Node_type type, Node* outer_graph);

            // *** Modifiers *** //
            void erase_entry_edge(Node* source);
            void erase_exit_edge(Node* target);
            
            // *** Getters and setters *** //
            int get_id();
            void set_id(int id);
            
            bool is_visited();
            void set_visited(bool visited);
            
            ObjectList<Edge*> get_entry_edges();
            void set_entry_edge(Edge *entry_edge);
            
            ObjectList<Edge*> get_exit_edges();
            void set_exit_edge(Edge *exit_edge);
            
            Node_type get_node_type();
            bool is_basic_node();
    };
}

#endif // NODE_HPP