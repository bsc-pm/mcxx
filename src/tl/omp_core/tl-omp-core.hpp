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
                virtual void register_omp_constructs();

                // Handler functions
#define DIRECTIVE_INFO(_name) \
                void _name##_handler(PragmaCustomConstruct);
#define CONSTRUCT_INFO(_name) DIRECTIVE_INFO(_name)
#include "tl-omp-core.def"
#undef DIRECTIVE_INFO
#undef CONSTRUCT_INFO

                RefPtr<OpenMP::Info> _openmp_info;

                void get_clause_symbols(PragmaCustomClause clause, ObjectList<Symbol>& sym_list);
                void get_reduction_symbols(PragmaCustomClause clause, ObjectList<ReductionSymbol>& sym_list);
                void get_data_explicit_attributes(PragmaCustomConstruct construct, 
                        DataSharing& data_sharing);
                void get_data_implicit_attributes(PragmaCustomConstruct construct, 
                        DataAttribute default_data_attr, 
                        DataSharing& data_sharing);
                DataAttribute get_default_data_sharing(PragmaCustomConstruct construct,
                        DataAttribute fallback_data_sharing);
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
