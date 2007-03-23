#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    Type Symbol::get_type() const
    {
        Type result(_symbol->type_information);
        return result;
    }

    std::string Symbol::get_name() const
    {
        return (_symbol->symbol_name != NULL) ? 
            std::string(_symbol->symbol_name) : 
            std::string("");
    }

    std::string Symbol::get_qualified_name() const
    {
        if (_symbol->symbol_name == NULL)
        {
            return std::string("");
        }
        else
        {
            // FIXME -> the scope should be the occurrence one
            int max_level = 0;
            char is_dependent = 0;
            char* qualified_name = get_fully_qualified_symbol_name(_symbol, _symbol->scope, 
                    &is_dependent, &max_level);
            return std::string(qualified_name);
        }
    }

    bool Symbol::operator<(Symbol s) const
    {
        return this->_symbol < s._symbol;
    }

    Scope Symbol::get_scope() const
    {
        Scope result(_symbol->scope);

        return result;
    }

    Symbol& Symbol::operator=(Symbol s)
    {
        this->_symbol = s._symbol;
        return (*this);
    }

    bool Symbol::operator==(Symbol s) const
    {
        return (this->_symbol == s._symbol);
    }

    bool Symbol::operator!=(Symbol s) const
    {
        return !(this->operator==(s));
    }

    const Symbol Symbol::invalid()
    {
        return Symbol(NULL);
    }

    bool Symbol::is_invalid() const
    {
        return (*this == invalid());
    }

    bool Symbol::is_valid() const
    {
        return !is_invalid();
    }

    // This should be subclassed since it is C/C++ specific
    bool Symbol::is_variable() const
    {
        return (this->_symbol->kind == SK_VARIABLE);
    }

    bool Symbol::is_function() const
    {
        return (this->_symbol->kind == SK_FUNCTION
                || this->_symbol->kind == SK_TEMPLATE_FUNCTION);
    }

    bool Symbol::is_template_function() const
    {
        return (this->_symbol->kind == SK_TEMPLATE_FUNCTION);
    }

    bool Symbol::is_typename() const
    {
        return (this->_symbol->kind == SK_TYPEDEF
                || this->_symbol->kind == SK_ENUM
                || this->_symbol->kind == SK_CLASS
                || this->_symbol->kind == SK_TEMPLATE_PRIMARY_CLASS
                || this->_symbol->kind == SK_TEMPLATE_SPECIALIZED_CLASS
                || this->_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER);
    }

    bool Symbol::is_member() const
    {
        return _symbol->is_member;
    }

    Type Symbol::get_class_type() const
    {
        return Type(_symbol->class_type);
    }

    AST_t Symbol::get_point_of_declaration() const
    {
        return AST_t(_symbol->point_of_declaration);
    }

    bool Symbol::is_parameter() const
    {
        if (_symbol != NULL)
        {
            return (_symbol->scope->kind == PROTOTYPE_SCOPE);
        }
        else
        {
            return false;
        }
    }
}
