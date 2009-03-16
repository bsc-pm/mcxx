#ifndef HLT_EXCEPTION_HPP
#define HLT_EXCEPTION_HPP

#include "tl-ast.hpp"
#include "tl-langconstruct.hpp"
#include <iostream>

struct HLTException
{
    public:
        HLTException(TL::LangConstruct place, const std::string& message)
            : _ast(place.get_ast()), _message(message)
        {
        }
        HLTException(TL::AST_t place, const std::string& message)
            : _ast(place), _message(message)
        {
        }
    private:
        TL::AST_t _ast;
        std::string _message;

        friend std::ostream& operator<<(std::ostream &o, const HLTException&);
};

std::ostream& operator<<(std::ostream &o, const HLTException& e)
{
    return (o << e._ast.get_locus() << ": error: " << e._message);
}

#endif // HLT_EXCEPTION_HPP
