#ifndef TL_PRETRANSFORM_HPP
#define TL_PRETRANSFORM_HPP

#include "tl-omp.hpp"
#include <utility>

namespace TL
{
    namespace Nanos4
    {
        class OpenMP_PreTransform : public OpenMP::OpenMPPhase
        {
            private:
                typedef std::pair<Symbol, ObjectList<Symbol> > function_symbols_pair_t;
                typedef ObjectList<function_symbols_pair_t> function_sym_list_t;
                function_sym_list_t _function_sym_list;

                void add_local_symbol(function_sym_list_t& sym_list, Symbol function, Symbol local);

                ObjectList<Symbol> get_all_functions(const function_sym_list_t& sym_list);
                ObjectList<Symbol> get_symbols_of_function(const function_sym_list_t& sym_list, 
                        Symbol function_sym);

                void remove_symbol_declaration(Symbol sym);

                ScopeLink _scope_link;

                int _function_num;
            public:
                OpenMP_PreTransform();

                void handle_threadprivate(OpenMP::ThreadPrivateDirective);

                void purge_local_threadprivates();

                virtual void init(DTO& dto);
        };
    }
}

#endif // TL_PRETRANSFORM_HPP
