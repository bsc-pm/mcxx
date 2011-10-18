#ifndef CODEGEN_COMMON_HPP
#define CODEGEN_COMMON_HPP

#include "tl-nodecl-visitor.hpp"
#include <string>
#include <cstdio>
#include <fstream>

namespace Codegen
{
    class CodegenVisitor : public Nodecl::NodeclVisitor<void>
    {
        virtual std::string codegen(const Nodecl::NodeclBase&) = 0;

        void codegen_to_file(const Nodecl::NodeclBase& n, FILE* f)
        {
            std::string str ( this->codegen(n) );
            fprintf(f, "%s", str.c_str());
        }

        std::ostream& codegen_to_ostream(const Nodecl::NodeclBase& n, std::ostream& os)
        {
            std::string str ( this->codegen(n) );
            os << str;

            return os;
        }
    };
}

#endif // CODEGEN_COMMON_HPP
