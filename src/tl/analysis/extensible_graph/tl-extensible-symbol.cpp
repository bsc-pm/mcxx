/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#include <algorithm>


#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-extensible-symbol.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-cfg-renaming-visitor.hpp"


namespace TL
{
    namespace Analysis
    {
        ExtensibleSymbol::ExtensibleSymbol()
            : _n(Nodecl::NodeclBase::null())
        {}
        
        ExtensibleSymbol::ExtensibleSymbol(Nodecl::NodeclBase n)
            : _n(n)
        {
            if(n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::Mul>() 
                || n.is<Nodecl::Minus>() || n.is<Nodecl::Add>())
            {
                std::cerr << "wat de foo: " << n.prettyprint() << std::endl;
            }
        }
        
        void ExtensibleSymbol::propagate_constant_values(std::map<Symbol, Nodecl::NodeclBase> values_map)
        {
            CfgRenamingVisitor renaming_v(values_map, _n.get_filename().c_str(), _n.get_line());
            ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(_n);
            if (!renamed.empty())
            {
                if (renamed.size() == 1)
                {
                    _n = renamed[0];
                }
                else
                {
                    internal_error("More than one nodecl returned while renaming constant values", 0);
                }
            }
        }
        
        ObjectList<Nodecl::NodeclBase> ExtensibleSymbol::get_nodecls_base(Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>() || n.is<Nodecl::ObjectInit>() || n.is<Nodecl::FunctionCall>())
            {
                return ObjectList<Nodecl::NodeclBase>(1, n);
            }
            else if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                     || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>())
            {
                return ObjectList<Nodecl::NodeclBase>();
            }
            else if (n.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
                return get_nodecls_base(aux.get_lhs());
            }
            else if (n.is<Nodecl::ArraySubscript>())
            {
                Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
                return get_nodecls_base(aux.get_subscripted());
            }
            else if (n.is<Nodecl::Reference>())
            {
                Nodecl::Reference aux = n.as<Nodecl::Reference>();
                return get_nodecls_base(aux.get_rhs());
            }            
            else if (n.is<Nodecl::Derreference>())
            {
                Nodecl::Derreference aux = n.as<Nodecl::Derreference>();
                return get_nodecls_base(aux.get_rhs());
            }
            else if (n.is<Nodecl::Conversion>())
            {
                Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
                return get_nodecls_base(aux.get_nest());
            }
            else if (n.is<Nodecl::Cast>())
            {
                Nodecl::Cast aux = n.as<Nodecl::Cast>();
                return get_nodecls_base(aux.get_rhs());
            }
            /*!
             * We can have (pre- post-) in- de-crements and other arithmetic operations
             * Example:
             * T *curr_high = ...;
             * *curr_high-- = l;
             * "*curr_high--" is a _KILLED_VAR
             */
            else if (n.is<Nodecl::Predecrement>())
            {   
                Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
                return get_nodecls_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Postdecrement>())
            {   
                Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
                return get_nodecls_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Preincrement>())
            {   
                Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
                return get_nodecls_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Postincrement>())
            {   
                Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
                return get_nodecls_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Add>())
            {   
                Nodecl::Add aux = n.as<Nodecl::Add>();
                ObjectList<Nodecl::NodeclBase> rhs = get_nodecls_base(aux.get_rhs());
                ObjectList<Nodecl::NodeclBase> lhs = get_nodecls_base(aux.get_lhs());
                return rhs.append(lhs);               
            }            
            else
            {
                std::cerr << "Necesito una linea más" << std::endl;
                internal_error("Unexpected type of nodecl '%s' contained in an ExtensibleSymbol '%s'", 
                            ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
            }            
        }
        
        ObjectList<Symbol> ExtensibleSymbol::get_nodecl_symbols(Nodecl::NodeclBase n) const
        {
            ObjectList<Symbol> syms;
            ObjectList<Nodecl::NodeclBase> base_nodecls = get_nodecls_base(n);
            for (ObjectList<Nodecl::NodeclBase>::iterator it = base_nodecls.begin(); it != base_nodecls.end(); ++it)
            {
                if (!it->is<Nodecl::FunctionCall>())
                {
                    syms.insert(it->get_symbol());
                }
            }
            return syms;
        }
    
        ObjectList<Symbol> ExtensibleSymbol::get_symbols() const
        {
            return get_nodecl_symbols(_n);
        }    

        Nodecl::NodeclBase ExtensibleSymbol::get_nodecl_base(Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>() || n.is<Nodecl::ObjectInit>() || n.is<Nodecl::FunctionCall>())
            {
                return n;
            }
            else if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                     || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>())
            {
                return Nodecl::NodeclBase::null();
            }
            else if (n.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
                return get_nodecl_base(aux.get_lhs());
            }
            else if (n.is<Nodecl::ArraySubscript>())
            {
                Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
                return get_nodecl_base(aux.get_subscripted());
            }
            else if (n.is<Nodecl::Reference>())
            {
                Nodecl::Reference aux = n.as<Nodecl::Reference>();
                Nodecl::NodeclBase ref = get_nodecl_base(aux.get_rhs());
                if (ref.is_null())
                    return n;
                else
                    return ref;
            }            
            else if (n.is<Nodecl::Derreference>())
            {
                Nodecl::Derreference aux = n.as<Nodecl::Derreference>();
                Nodecl::NodeclBase derref = get_nodecl_base(aux.get_rhs());
                if (derref.is_null())
                    return n;
                else
                    return derref;
            }
            else if (n.is<Nodecl::Conversion>())
            {
                Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
                return get_nodecl_base(aux.get_nest());
            }
            else if (n.is<Nodecl::Cast>())
            {
                Nodecl::Cast aux = n.as<Nodecl::Cast>();
                return get_nodecl_base(aux.get_rhs());
            }
            /*!
             * We can have (pre- post-) in- de-crements and other arithmetic operations
             * Example:
             * T *curr_high = ...;
             * *curr_high-- = l;
             * "*curr_high--" is a _KILLED_VAR
             */
            else if (n.is<Nodecl::Predecrement>())
            {   
                Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
                return get_nodecl_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Postdecrement>())
            {   
                Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
                return get_nodecl_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Preincrement>())
            {   
                Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
                return get_nodecl_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Postincrement>())
            {   
                Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
                return get_nodecl_base(aux.get_rhs());                
            }
            else if (n.is<Nodecl::Add>())
            {   
                return Nodecl::NodeclBase::null();               
            }            
            else
            {
                internal_error("Unexpected type of nodecl '%s' contained in an ExtensibleSymbol '%s'", 
                            ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
            }            
        }
        
        Symbol ExtensibleSymbol::get_nodecl_symbol(Nodecl::NodeclBase n) const
        {
            Nodecl::NodeclBase base_nodecl = get_nodecl_base(n);
            if (!n.is<Nodecl::FunctionCall>())
            {
                return n.get_symbol();
            }
            else
            {
                return Symbol();
            }
        }

        Symbol ExtensibleSymbol::get_symbol() const
        {
            return get_nodecl_symbol(_n);
        } 

        std::string ExtensibleSymbol::get_name() const
        {
            std::string name = "";
            
            ObjectList<Symbol> syms = get_nodecl_symbols(_n);
            for (ObjectList<Symbol>::iterator it = syms.begin(); it != syms.end(); ++it)
            {
                if (it->is_valid())
                    name += (it->get_name() + "  ");
            }
            
            return name;
        }
        
        Type ExtensibleSymbol::get_type() const
        {
            Type res(NULL);
            
            ObjectList<Symbol> syms = get_nodecl_symbols(_n);
            for (ObjectList<Symbol>::iterator it = syms.begin(); it != syms.end(); ++it)
            {
                if (it->is_valid())
                {    
                    res = it->get_type();
                    break;
                }
            }
            
            return res;
        }
        
        Nodecl::NodeclBase ExtensibleSymbol::get_nodecl() const
        {
            return _n;
        }
        
        bool ExtensibleSymbol::is_simple_symbol() const
        {
            return (_n.is<Nodecl::Symbol>());
        }

        bool ExtensibleSymbol::is_array() const
        {
            if (_n.is<Nodecl::ArraySubscript>())
                return true;
            else if (_n.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess member_acc = _n.as<Nodecl::ClassMemberAccess>();
                ExtensibleSymbol class_(member_acc.get_lhs());
                ExtensibleSymbol memb_(member_acc.get_member());
                return ( class_.is_array() || memb_.is_array() );
            }
            else if (_n.is<Nodecl::Reference>())
            {
                Nodecl::Reference ref = _n.as<Nodecl::Reference>();
                ExtensibleSymbol ref_(ref.get_rhs());
                return ref_.is_array();
            }
            else if (_n.is<Nodecl::Derreference>())
            {
                Nodecl::Derreference ref = _n.as<Nodecl::Derreference>();
                ExtensibleSymbol ref_(ref.get_rhs());
                return ref_.is_array();
            }            
            else
                return false;
        }

        bool ExtensibleSymbol::operator==(const ExtensibleSymbol& es) const
        {
            bool equals = Nodecl::Utils::equal_nodecls(_n, es._n);
//             Nodecl::NodeclBase a = _n, b = es._n;
//             std::cerr << "  - " << a.prettyprint() << " == " << b.prettyprint() << "? " << (equals?"TRUE":"FALSE") << std::endl;
            return equals;
        }
        
        bool ExtensibleSymbol::operator<(const ExtensibleSymbol& es) const
        {
            // a < b -> ¬ (b > a)
            // a == b <=> ¬(a < b) /\ ¬(b < a)
            if (Nodecl::Utils::equal_nodecls(_n, es._n))
            {   
                return false;
            }
            else
            {
                AST this_ast = nodecl_get_ast(_n.get_internal_nodecl());
                AST es_ast = nodecl_get_ast(es._n.get_internal_nodecl());
                bool less_than = this_ast < es_ast;
            
                return  less_than;
            }
        }
    }
}
