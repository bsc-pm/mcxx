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
    CfgRenamingVisitor::CfgRenamingVisitor(std::map<Symbol, Nodecl::NodeclBase> rename_map,
                                           const char* filename, int line)
        : _rename_map(rename_map), _filename(filename), _line(line)
    {}
    
    CfgRenamingVisitor::CfgRenamingVisitor(const CfgRenamingVisitor& sym_v)
    {
        _rename_map = sym_v._rename_map;
        _filename = sym_v._filename;
        _line = sym_v._line;
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        std::cerr << "Unhandled node during CFG construction '" << c_cxx_codegen_to_str(n.get_internal_nodecl())
                  << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
        return Ret();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Symbol& n)
    {
        std::map<Symbol, Nodecl::NodeclBase>::iterator it = _rename_map.find(n.get_symbol());
        
        if (it != _rename_map.end())
        {
            Nodecl::NodeclBase renamed_sym = _rename_map[n.get_symbol()];
            return ObjectList<Nodecl::NodeclBase>(1, renamed_sym);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        return walk(n.get_nest());
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        return walk(n.get_nest());
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ObjectInit& n)
    {
        // The symbol of an ObjectInit will never be renamed!!
        ObjectList<Nodecl::NodeclBase> init_expr = walk(n.get_symbol().get_initialization());
        if (!init_expr.empty())
        {
            // TODO Make new Symbol with the new init_expr
            nodecl_t renamed_object_init = nodecl_make_object_init(n.get_symbol().get_internal_symbol(),
                                                                   _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_object_init);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }   
   
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Throw& n)
    {
        ObjectList<Nodecl::NodeclBase> rhs = walk(n.get_rhs());
        if (!rhs.empty())
        {
            nodecl_t renamed_throw = nodecl_make_throw(rhs[0].get_internal_nodecl(), 
                                                       n.get_type().get_internal_type(), 
                                                       _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_throw);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }    
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_subscripted = walk(n.get_subscripted());
        ObjectList<Nodecl::NodeclBase> renamed_subscripts = walk(n.get_subscripts());
        
        if (!renamed_subscripted.empty() || !renamed_subscripts.empty())
        {
            Nodecl::NodeclBase subscripted = renamed_subscripted[0];
            if (renamed_subscripted.empty())
            {
                subscripted = n.get_subscripted();
            }
            
            Nodecl::NodeclBase subscripts;
            if (renamed_subscripts.empty())
            {
                subscripted = n.get_subscripts();
            }
            else
            {
                nodecl_t subscripts_l = nodecl_null();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = renamed_subscripts.begin();
                    it != renamed_subscripts.end();
                    ++it)
                {
                    subscripts_l = nodecl_append_to_list(subscripts_l, it->get_internal_nodecl());
                }
                subscripts = Nodecl::NodeclBase(subscripts_l);
            }
            
            nodecl_t renamed_array_subscript = nodecl_make_array_subscript(subscripted.get_internal_nodecl(), 
                                                                           subscripts.get_internal_nodecl(),
                                                                           n.get_type().get_internal_type(), 
                                                                           _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_array_subscript);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ArraySection& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_subscripted = walk(n.get_subscripted());
        ObjectList<Nodecl::NodeclBase> renamed_lower = walk(n.get_lower());
        ObjectList<Nodecl::NodeclBase> renamed_upper = walk(n.get_upper());
        
        if (!renamed_subscripted.empty() || !renamed_lower.empty() || !renamed_upper.empty())
        {
            Nodecl::NodeclBase subscripted = renamed_subscripted[0];
            if (renamed_subscripted.empty())
            {
                subscripted = n.get_subscripted();
            }
            
            Nodecl::NodeclBase lower = renamed_lower[0];
            if (renamed_lower.empty())
            {
                lower = n.get_lower();
            }

            Nodecl::NodeclBase upper = renamed_upper[0];
            if (renamed_upper.empty())
            {
                upper = n.get_upper();
            }
            
            nodecl_t renamed_array_section = nodecl_make_array_section(subscripted.get_internal_nodecl(), 
                                                                       lower.get_internal_nodecl(), upper.get_internal_nodecl(),
                                                                       n.get_type().get_internal_type(), 
                                                                       _filename, _line);       
            return ObjectList<Nodecl::NodeclBase>(1, renamed_array_section);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_lhs = walk(n.get_lhs());
        ObjectList<Nodecl::NodeclBase> renamed_member = walk(n.get_member());
        
        if (!renamed_lhs.empty() || !renamed_member.empty())
        {
            Nodecl::NodeclBase lhs = renamed_lhs[0];
            if (renamed_lhs.empty())
            {
                lhs = n.get_lhs();
            }

            Nodecl::NodeclBase member = renamed_member[0];
            if (renamed_member.empty())
            {
                member = n.get_member();
            }

            nodecl_t renamed_class_member = nodecl_make_class_member_access(lhs.get_internal_nodecl(), member.get_internal_nodecl(), 
                                                                            n.get_type().get_internal_type(),
                                                                            _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_class_member);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
   
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Concat& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::New& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Delete& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::DeleteArray& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Sizeof& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Type& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Typeid& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Cast& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Offset& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::StringLiteral& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BooleanLiteral& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::IntegerLiteral& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ComplexLiteral& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::FloatingLiteral& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::StructuredValue& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::EmptyStatement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ReturnStatement& n);  
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::GotoStatement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LabeledStatement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ContinueStatement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BreakStatement& n);        
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Assignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::AddAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::SubAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::DivAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::MulAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ModAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseAndAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseOrAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseXorAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ShrAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ShlAssignment& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Add& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Minus& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Mul& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Div& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Mod& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Power& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LogicalAnd& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LogicalOr& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseAnd& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseOr& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseXor& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Equal& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Different& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LowerThan& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::GreaterThan& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LowerOrEqualThan& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::GreaterOrEqualThan& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Shr& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Shl& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Predecrement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Postdecrement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Preincrement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Postincrement& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Plus& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Neg& n);     
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseNot& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LogicalNot& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Derreference& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Reference& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Text& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Comma& n);
//     CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Conversion& n);
}