#ifndef TL_CACHE_RTL_CALLS
#define TL_CACHE_RTL_CALLS

#include "tl-nodecl-visitor.hpp"
#include "tl-omp-intel.hpp"

namespace TL { namespace Intel {

class CacheRTLCalls : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        CacheRTLCalls(Lowering*);
        ~CacheRTLCalls();

        virtual void visit(const Nodecl::FunctionCode& function_code);

        typedef void (CacheRTLCalls::* CacheRTLCallsHandler)(TL::Symbol sym,
                Nodecl::NodeclBase function_code,
                TL::ObjectList<Nodecl::NodeclBase>& ocurrences);
    private:
        Lowering* _lowering;

        void add_cacheable_function(
                TL::ObjectList<TL::Symbol>& cacheable_set,
                const std::string& str,
                std::map<TL::Symbol, CacheRTLCallsHandler>& cacheable_handler_set,
                CacheRTLCallsHandler do_cache_call);

        void cache_kmpc_global_thread(
                TL::Symbol sym,
                Nodecl::NodeclBase function_code,
                TL::ObjectList<Nodecl::NodeclBase>& occurrences);
};

} }

#endif
