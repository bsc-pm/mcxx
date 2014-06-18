#include "tl-cache-rtl-calls.hpp"
#include "tl-counters.hpp"
#include "tl-source.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { namespace Intel {

    class FindRTLCacheableCalls : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            TL::ObjectList<TL::Symbol> _cacheable_set;
        public:
            TL::ObjectList<TL::Symbol> functions_found;
            std::map<TL::Symbol, TL::ObjectList<Nodecl::NodeclBase> > occurrences;

            FindRTLCacheableCalls(const TL::ObjectList<TL::Symbol> &cacheable_set)
                : _cacheable_set(cacheable_set) { }

            virtual void visit(const Nodecl::FunctionCall& n)
            {
                walk(n.get_arguments());

                TL::Symbol called_sym = n.get_called().get_symbol();
                if (called_sym.is_valid()
                        && _cacheable_set.contains(n.get_called().get_symbol()))
                {
                    functions_found.insert(called_sym);
                    occurrences[called_sym].append(n);
                }
            }
    };

    CacheRTLCalls::CacheRTLCalls(Lowering* lowering)
        : _lowering(lowering) { }

    CacheRTLCalls::~CacheRTLCalls()
    {
    }

    void CacheRTLCalls::add_cacheable_function(
            TL::ObjectList<TL::Symbol>& cacheable_set,
            const std::string& str,
            std::map<TL::Symbol, CacheRTLCallsHandler>& cacheable_handler_set,
            CacheRTLCallsHandler do_cache_call)
    {
        TL::Scope sc = Scope::get_global_scope();

        TL::Symbol sym = sc.get_symbol_from_name(str);

        if (!sym.is_valid())
            return;

        cacheable_set.insert(sym);
        cacheable_handler_set[sym] = do_cache_call;
    }

    void CacheRTLCalls::cache_kmpc_global_thread(
            TL::Symbol sym,
            Nodecl::NodeclBase function_code,
            TL::ObjectList<Nodecl::NodeclBase>& occurrences)
    {
        ERROR_CONDITION(occurrences.empty(), "Invalid set of occurrences", 0);

        Nodecl::NodeclBase context = function_code.as<Nodecl::FunctionCode>().get_statements();

        TL::Counter &cached_num = TL::CounterManager::get_counter("intel-omp-cached-values");

        std::stringstream cached_name;
        cached_name << "cached_gtid_value_" << (int)cached_num;
        cached_num++;

        Source src_decl;
        // We will cache the first occurrence
        src_decl << "kmp_int32 " << cached_name.str() << " = "
            << as_expression(occurrences[0].shallow_copy())
            << ";"
            ;
        Nodecl::NodeclBase new_decl = src_decl.parse_statement(context);

        ERROR_CONDITION(IS_FORTRAN_LANGUAGE, "Fortran not supported", 0);
        Nodecl::List statement_list = context.as<Nodecl::Context>().get_in_context().as<Nodecl::List>();
        Nodecl::CompoundStatement compound = statement_list[0].as<Nodecl::CompoundStatement>();
        Nodecl::List statements = compound.get_statements().as<Nodecl::List>();

        Nodecl::Utils::prepend_items_before(statements[0], new_decl);

        Source src_cached_expr;
        src_cached_expr << cached_name.str();

        Nodecl::NodeclBase cached_expr = src_cached_expr.parse_expression(context);

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = occurrences.begin();
                it != occurrences.end();
                it++)
        {
            it->replace( cached_expr.shallow_copy() );
        }
    }

    void CacheRTLCalls::visit(const Nodecl::FunctionCode& function_code)
    {
        TL::ObjectList<TL::Symbol> cacheable_set;
        std::map<TL::Symbol, CacheRTLCallsHandler> cacheable_handler_set;

        add_cacheable_function(cacheable_set,
                "__kmpc_global_thread_num",
                cacheable_handler_set,
                &CacheRTLCalls::cache_kmpc_global_thread);

        FindRTLCacheableCalls find_rtl_cacheable_calls(cacheable_set);
        find_rtl_cacheable_calls.walk(function_code);

        for (TL::ObjectList<TL::Symbol>::iterator
                it = find_rtl_cacheable_calls.functions_found.begin();
                it != find_rtl_cacheable_calls.functions_found.end();
                it++)
        {
            CacheRTLCallsHandler handler = cacheable_handler_set[*it];

            (this->*handler)(*it,
                    function_code,
                    find_rtl_cacheable_calls.occurrences[*it]);
        }
    }

} }
