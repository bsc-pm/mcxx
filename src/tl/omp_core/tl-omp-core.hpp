#ifndef TL_OMP_CORE_HPP
#define TL_OMP_CORE_HPP

#include "tl-compilerphase.hpp"
#include "tl-pragmasupport.hpp"

#include "tl-omp.hpp"

namespace TL
{
    namespace OpenMP
    {
        class Core : public TL::PragmaCustomCompilerPhase
        {
            private:
                void register_omp_constructs();

                // Handler functions
#define OMP_DIRECTIVE(_directive, _name) \
                void _name##_handler_pre(PragmaCustomConstruct); \
                void _name##_handler_post(PragmaCustomConstruct);
#define OMP_CONSTRUCT(_directive, _name) \
                OMP_DIRECTIVE(_directive, _name)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_DIRECTIVE

                RefPtr<OpenMP::Info> _openmp_info;

                void get_clause_symbols(PragmaCustomClause clause, ObjectList<Symbol>& sym_list);
                void get_reduction_symbols(PragmaCustomClause clause, ObjectList<ReductionSymbol>& sym_list);
                void get_data_explicit_attributes(PragmaCustomConstruct construct, 
                        DataSharing& data_sharing);
                void get_data_implicit_attributes(PragmaCustomConstruct construct, 
                        DataAttribute default_data_attr, 
                        DataSharing& data_sharing);
                void get_data_implicit_attributes_task(PragmaCustomConstruct construct,
                        DataSharing& data_sharing,
                        DataAttribute default_data_attr);

                DataAttribute get_default_data_sharing(PragmaCustomConstruct construct,
                        DataAttribute fallback_data_sharing);

                void common_parallel_handler(PragmaCustomConstruct ctr, DataSharing& data_sharing);
                void common_for_handler(PragmaCustomConstruct ctr, DataSharing& data_sharing);
                void common_workshare_handler(PragmaCustomConstruct construct, DataSharing& data_sharing);

            public:
                Core();

                virtual void run(TL::DTO& dto);
                virtual void pre_run(TL::DTO& dto);

                virtual ~Core() { }
        };

    }
}

EXPORT_PHASE(TL::OpenMP::Core)

#endif // TL_OMP_CORE_HPP
