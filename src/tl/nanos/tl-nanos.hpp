#ifndef TL_NANOS_HPP
#define TL_NANOS_HPP

#include <string>

#include "tl-pragmasupport.hpp"

namespace TL
{
    namespace Nanos4
    {
        class Version
        {
            public:
                const static int DEFAULT_VERSION;
                const static char* DEFAULT_FAMILY;

                static int version;
                static std::string family;

                static bool is_family(const std::string &family);
                static bool is_version(int version);
                static bool is_interface(const std::string &family, int version);
        };
        
        class Interface : public PragmaCustomCompilerPhase
        {
            public:
                Interface();
                void interface_preorder(PragmaCustomConstruct);
                void interface_postorder(PragmaCustomConstruct);

                virtual void run(TL::DTO& dto);

                ~Interface() { }
        };
    }
}

#endif // TL_NANOS_HPP
