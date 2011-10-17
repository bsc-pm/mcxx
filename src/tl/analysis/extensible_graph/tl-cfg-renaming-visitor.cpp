/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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


#include "cxx-codegen.h"

#include "tl-cfg-renaming-visitor.hpp"

namespace TL
{
    CfgRenamingVisitor::CfgRenamingVisitor(std::map<Symbol, Nodecl::NodeclBase> rename_map, const char* filename, int line)
        : _rename_map(rename_map), _filename(filename), _line(line), _s(NULL)
    {}
    
    CfgRenamingVisitor::CfgRenamingVisitor(const CfgRenamingVisitor& rename_v)
    {
        _rename_map = rename_v._rename_map;
        _filename = rename_v._filename;
        _line = rename_v._line;
        _s = rename_v._s;
    }
    
    Symbol CfgRenamingVisitor::get_matching_symbol() const
    {
        return _s;
    }
    
    nodecl_t CfgRenamingVisitor::create_nodecl_list(ObjectList<Nodecl::NodeclBase> list)
    {
        nodecl_t nodecl_l = nodecl_null();
        for (ObjectList<Nodecl::NodeclBase>::iterator it = list.begin();
            it != list.end();
            ++it)
        {
            nodecl_l = nodecl_append_to_list(nodecl_l, it->get_internal_nodecl());
        }
        
        return nodecl_l;
    }        
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        std::cerr << "Unhandled node while inline renaming '" << c_cxx_codegen_to_str(n.get_internal_nodecl())
                  << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
        return Ret();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Symbol& n)
    {
        if (_rename_map.find(n.get_symbol()) != _rename_map.end())
        {
            Nodecl::NodeclBase mapped_value = _rename_map[n.get_symbol()];
            _s = n.get_symbol();
            return ObjectList<Nodecl::NodeclBase>(1, mapped_value);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }

    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        Nodecl::NodeclBase subscripted = n.get_subscripted();
        Nodecl::NodeclBase subscripts = n.get_subscripts();
        
        ObjectList<Nodecl::NodeclBase> renamed_subscripted = walk(subscripted);
        ObjectList<Nodecl::NodeclBase> renamed_subscripts = walk(subscripts);
        
        if (!renamed_subscripted.empty() || !renamed_subscripts.empty())
        {
            if (!renamed_subscripted.empty())
            {
                subscripted = renamed_subscripted[0];
            }
            if (!renamed_subscripts.empty())
            {
                subscripts = Nodecl::NodeclBase(create_nodecl_list(renamed_subscripts));
            }
            
            Nodecl::NodeclBase renamed = Nodecl::ArraySubscript::make(subscripted, subscripts, n.get_type(), _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Range& n)
    {
        Nodecl::NodeclBase lower = n.get_lower();
        Nodecl::NodeclBase upper = n.get_upper();
        Nodecl::NodeclBase stride = n.get_stride();
        
        ObjectList<Nodecl::NodeclBase> renamed_lower = walk(lower);
        ObjectList<Nodecl::NodeclBase> renamed_upper = walk(upper);
        ObjectList<Nodecl::NodeclBase> renamed_stride = walk(stride);

        if (!renamed_lower.empty() || !renamed_upper.empty() || !renamed_stride.empty())
        {
            if (!renamed_lower.empty())
            {
                lower = renamed_lower[0];
            }
            if (!renamed_upper.empty())
            {
                upper = renamed_upper[0];
            }
            if (!renamed_stride.empty())
            {
                stride = renamed_stride[0];
            }
            
            Nodecl::NodeclBase renamed = Nodecl::Range::make(lower, upper, stride, n.get_type(), _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed);
        }    
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase member = n.get_member();
        
        ObjectList<Nodecl::NodeclBase> renamed_lhs = walk(lhs);
        ObjectList<Nodecl::NodeclBase> renamed_member = walk(member);
        
        if (!renamed_lhs.empty() || !renamed_member.empty())
        {
            if (!renamed_lhs.empty())
            {
                lhs = renamed_lhs[0];
            }
            if (!renamed_member.empty())
            {
                member = renamed_member[0];
            }

            Nodecl::NodeclBase renamed = Nodecl::ClassMemberAccess::make(lhs, member, n.get_type(), _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }

    template <typename T>
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit_binary(const T& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        
        ObjectList<Nodecl::NodeclBase> renamed_lhs = walk(lhs);
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(rhs);
        
        if (!renamed_lhs.empty() || !renamed_rhs.empty())
        {
            if (!renamed_lhs.empty())
            {
                lhs = renamed_lhs[0];
            }
            if (!renamed_rhs.empty())
            {
                rhs = renamed_rhs[0];
            }
            
            Nodecl::NodeclBase renamed;
            if (n.template is<Nodecl::Add>())
            {
                renamed = Nodecl::Add::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Minus>())
            {
                renamed = Nodecl::Minus::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Mul>())
            {
                renamed = Nodecl::Mul::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Div>())
            {
                renamed = Nodecl::Div::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Mod>())
            {
                renamed = Nodecl::Mod::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Power>())
            {
                renamed = Nodecl::Power::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::LogicalAnd>())
            {
                renamed = Nodecl::LogicalAnd::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::LogicalOr>())
            {
                renamed = Nodecl::LogicalOr::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseAnd>())
            {
                renamed = Nodecl::BitwiseAnd::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseOr>())
            {
                renamed = Nodecl::BitwiseOr::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseXor>())
            {
                renamed = Nodecl::BitwiseXor::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Shr>())
            {
                renamed = Nodecl::Shr::make(lhs, rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Shl>())
            {
                renamed = Nodecl::Shl::make(lhs, rhs, n.get_type(), _filename, _line);
            }            
            
            return ObjectList<Nodecl::NodeclBase>(1, renamed);            
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }

    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Add& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Minus& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Mul& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Div& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Mod& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Power& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LogicalOr& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseAnd& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseOr& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseXor& n)
    {
        return visit_binary(n);
    }
       
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Shr& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Shl& n)
    {
        return visit_binary(n);
    }
        
    template <typename T>
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit_unary(const T& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(rhs);
        
        if (!renamed_rhs.empty())
        {
            rhs = renamed_rhs[0];
            
            Nodecl::NodeclBase renamed;
            if (n.template is<Nodecl::Predecrement>())
            {
                renamed = Nodecl::Predecrement::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Postdecrement>())
            {
                renamed = Nodecl::Postdecrement::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Preincrement>())
            {
                renamed = Nodecl::Preincrement::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Postincrement>())
            {
                renamed = Nodecl::Postincrement::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Plus>())
            {
                renamed = Nodecl::Plus::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Neg>())
            {
                renamed = Nodecl::Neg::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseNot>())
            {
                renamed = Nodecl::BitwiseNot::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::LogicalNot>())
            {
                renamed = Nodecl::LogicalNot::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Derreference>())
            {
                renamed = Nodecl::Derreference::make(rhs, n.get_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Reference>())
            {
                renamed = Nodecl::Reference::make(rhs, n.get_type(), _filename, _line);
            }
            
            return ObjectList<Nodecl::NodeclBase>(1, renamed);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Predecrement& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Postdecrement& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Preincrement& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Postincrement& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Plus& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Neg& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseNot& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LogicalNot& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Derreference& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Reference& n)
    {
        return visit_unary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Conversion& n)
    {
        Nodecl::NodeclBase nest = n.get_nest();
        ObjectList<Nodecl::NodeclBase> renamed_nest = walk(nest);
        
        if (!renamed_nest.empty())
        {
            nest = Nodecl::NodeclBase(create_nodecl_list(renamed_nest));
            Nodecl::NodeclBase renamed = Nodecl::Conversion::make(nest, n.get_type(), _filename, _line);
            
            return ObjectList<Nodecl::NodeclBase>(1, renamed);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::IntegerLiteral& n)
    {
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::FloatingLiteral& n)
    {
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ComplexLiteral& n)
    {
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BooleanLiteral& n)
    {
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::StringLiteral& n)
    {
        return ObjectList<Nodecl::NodeclBase>();
    }
}