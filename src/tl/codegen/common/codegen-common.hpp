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
            CodegenVisitor()
                : _is_file_output(false)
            {
            }

            bool is_file_output() const
            {
                return _is_file_output;
            }

            std::string codegen_to_str(const Nodecl::NodeclBase& n)
            {
                this->_is_file_output = false;
                return this->codegen(n);
            }

            void codegen_top_level(const Nodecl::NodeclBase& n, FILE* f)
            {
                this->_is_file_output = true;
                std::string str ( this->codegen(n) );
                fprintf(f, "%s", str.c_str());
            }
    };
}

#endif // CODEGEN_COMMON_HPP
