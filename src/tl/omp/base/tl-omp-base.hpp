#ifndef TL_OMP_BASE_HPP
#define TL_OMP_BASE_HPP

#include "tl-pragmasupport.hpp"

namespace TL
{
    namespace OpenMP
    {
        //! This class transforms OpenMP pragmas to the Nodecl representation of parallelism
        class Base : public TL::PragmaCustomCompilerPhase
        {
            public:
                Base();

                virtual void run(TL::DTO& dto);
                virtual void pre_run(TL::DTO& dto);

                virtual ~Base() { }

            private:
                void register_omp_constructs();

                static bool _already_registered;

                // Handler functions
#define OMP_DIRECTIVE(_directive, _name) \
                void _name##_handler_pre(TL::PragmaCustomDirective); \
                void _name##_handler_post(TL::PragmaCustomDirective);
#define OMP_CONSTRUCT(_directive, _name) \
                void _name##_handler_pre(TL::PragmaCustomStatement); \
                void _name##_handler_post(TL::PragmaCustomStatement); \
                void _name##_handler_pre(TL::PragmaCustomDeclaration); \
                void _name##_handler_post(TL::PragmaCustomDeclaration); 
#define OMP_CONSTRUCT_NOEND(_directive, _name) \
                OMP_CONSTRUCT(_directive, _name)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
#undef OMP_DIRECTIVE
                
                Nodecl::List compute_execution_environment(PragmaCustomLine, Nodecl::NodeclBase region, bool is_task);
        };
    }
}

#endif // TL_OMP_BASE_HPP
