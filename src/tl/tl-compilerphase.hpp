#ifndef TL_COMPILER_PHASE_HPP
#define TL_COMPILER_PHASE_HPP

#include "tl-object.hpp"
#include "tl-dto.hpp"

namespace TL
{
    class CompilerPhase : Object
    {
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
            {
                return NULL;
            }
        public:
            virtual void run(DTO& data_flow) = 0;
            virtual ~CompilerPhase() { }
    };
}

#define EXPORT_PHASE(ClassName) \
extern "C"  \
{ \
    extern TL::CompilerPhase* give_compiler_phase_object(void) \
    { \
        return new ClassName(); \
    } \
}

#endif // TL_COMPILER_PHASE_HPP
