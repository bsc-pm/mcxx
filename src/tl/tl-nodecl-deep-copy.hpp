#ifndef TL_NODECL_DEEP_COPY_HPP
#define TL_NODECL_DEEP_COPY_HPP

#include "tl-symbol.hpp"
#include "tl-source.hpp"
#include "tl-nodecl-copy-visitor.hpp"
#include <map>

namespace Nodecl {
    struct SimpleSymbolMap : SymbolMap
    {
        private:
            typedef std::map<TL::Symbol, TL::Symbol> sym_map_t;
            sym_map_t _map;
        public:
            virtual void add_map(TL::Symbol source, TL::Symbol target)
            {
                _map[source] = target;
            }

            virtual TL::Symbol map(TL::Symbol source)
            {
                sym_map_t::iterator it = _map.find(source);
                if (it != _map.end())
                {
                    return it->second;
                }
                else
                {
                    return source;
                }
            }
    };

    Nodecl::NodeclBase deep_copy(Nodecl::NodeclBase, const TL::ReferenceScope&, SymbolMap &map_symbol);
}

#endif // TL_NODECL_DEEP_COPY_HPP
