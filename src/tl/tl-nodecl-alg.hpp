#ifndef TL_NODECL_ALG_HPP
#define TL_NODECL_ALG_HPP

#include "tl-nodecl.hpp"

#include <tr1/unordered_map>

namespace Nodecl
{
    namespace Utils
    {
        TL::ObjectList<TL::Symbol> get_all_symbols(Nodecl::NodeclBase);
        TL::ObjectList<TL::Symbol> get_nonlocal_symbols(Nodecl::NodeclBase);
        TL::ObjectList<TL::Symbol> get_local_symbols(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_occurrences(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_first_occurrence(Nodecl::NodeclBase);
       
        bool equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2);
        struct Nodecl_hash {
            size_t operator() (const Nodecl::NodeclBase& n) const;
        };
        struct Nodecl_comp {
            bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const;
        };
        
        NodeclBase reduce_expression(Nodecl::NodeclBase n);
        NodeclBase algebraic_simplification(Nodecl::NodeclBase n);
    }
}

namespace TL
{
    struct ForStatement : Nodecl::ForStatement
    {
        public:
            ForStatement(const Nodecl::ForStatement n)
                : Nodecl::ForStatement(n) { }

            bool is_regular_loop() const;
            Symbol get_induction_variable() const;
    };
}

#endif // TL_NODECL_ALG_HPP
