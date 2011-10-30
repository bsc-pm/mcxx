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
        private:
            bool _is_file_output;
        protected:
            virtual std::string codegen(const Nodecl::NodeclBase&) = 0;
        public:
            CodegenVisitor();

            bool is_file_output() const;

            std::string codegen_to_str(const Nodecl::NodeclBase& n);

            void codegen_top_level(const Nodecl::NodeclBase& n, FILE* f);
    };
}

#endif // CODEGEN_COMMON_HPP
