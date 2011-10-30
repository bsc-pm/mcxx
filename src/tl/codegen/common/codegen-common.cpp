#include "codegen-common.hpp"

namespace Codegen
{

CodegenVisitor::CodegenVisitor()
: _is_file_output(false)
{
}

bool CodegenVisitor::is_file_output() const
{
    return _is_file_output;
}

std::string CodegenVisitor::codegen_to_str(const Nodecl::NodeclBase& n)
{
    bool old_is_file_output = this->_is_file_output;
    this->_is_file_output = false;

    std::string result = this->codegen(n);

    this->_is_file_output = old_is_file_output;
    return result;
}

void CodegenVisitor::codegen_top_level(const Nodecl::NodeclBase& n, FILE* f)
{
    bool old_is_file_output = this->_is_file_output;
    this->_is_file_output = true;

    std::string str ( this->codegen(n) );
    fprintf(f, "%s", str.c_str());

    this->_is_file_output = old_is_file_output;
}

}
