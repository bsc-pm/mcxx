#include "tl-scopelink.hpp"
namespace TL
{

Scope ScopeLink::get_scope(AST_t ast) const
{
    AST _ast = ast._ast;
    scope_t* st = scope_link_get_scope(_scope_link, _ast);

    Scope result(st);
    return result;
}

ScopeLink& ScopeLink::operator=(ScopeLink sl)
{
    this->_scope_link = sl._scope_link;
    return (*this);
}

bool ScopeLink::operator==(ScopeLink sl)
{
    return this->_scope_link == sl._scope_link;
}

bool ScopeLink::operator!=(ScopeLink sl)
{
    return !(this->operator==(sl));
}

}
