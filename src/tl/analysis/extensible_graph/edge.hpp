#ifndef EDGE_HPP
#define EDGE_HPP

#include "node.hpp"
#include "tl-builtin.hpp"

namespace TL
{
    enum Edge_type
    {
        // PCFG
        PARALLEL_EDGE = 5,
        CONFLICT_EDGE = 6,
        // CFG: All are SEQUENTIAL_EDGE in a PCFG
        TRUE_EDGE = 1,
        FALSE_EDGE = 0,
        ALWAYS_EDGE = 2,
        CASE_EDGE = 3,
        CATCH_EDGE = 4,
        // Unclassified edges
        UNCLASSIFIED_EDGE = -1
    };
    
    class Node;
    
    class LIBTL_CLASS Edge : public LinkData {
        private:
            Node* _source;
            Node* _target;
            
        public:
            // *** Constructors *** //
            //!*
            //!*
            Edge(Node *source, Node *target, Edge_type type, std::string label="");
            
            
            // *** Getters and Setters *** //
            Node* get_source();
            Node* get_target();
            
            Edge_type get_type();
            std::string get_label();
            
            friend class ExtensibleGraph;
    };
}

#endif // EDGE_HPP