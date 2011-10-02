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
                                           const char* filename, int line, int i)
        : _rename_map(rename_map), _filename(filename), _line(line), _i(i)
    {}
    
    CfgRenamingVisitor::CfgRenamingVisitor(const CfgRenamingVisitor& sym_v)
    {
        _rename_map = sym_v._rename_map;
        _filename = sym_v._filename;
        _line = sym_v._line;
        _i = sym_v._i;
    }
    
    template <typename T>
    scope_entry_t* CfgRenamingVisitor::create_symbol(const T& n, Nodecl::NodeclBase init_expr)
    {
        std::stringstream ss; ss << _i; ++_i;
        std::string sym_name = "_tmp_" + ss.str();
        decl_context_t sym_context = n.retrieve_context().get_decl_context();
        scope_entry_t* new_sym = new_symbol(sym_context, sym_context.current_scope, sym_name.c_str());
            new_sym->kind = SK_VARIABLE;
        scope_entry_t* called_symbol_ = n.get_symbol().get_internal_symbol();
        scope_entry_t* param = called_symbol_->entity_specs.related_symbols[_i];
        new_sym->type_information = param->type_information;
            new_sym->value = init_expr.get_internal_nodecl();
            
        return new_sym;
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
        std::map<Symbol, Nodecl::NodeclBase>::iterator it = _rename_map.find(n.get_symbol());
        
        if (it != _rename_map.end())
        {
            Nodecl::NodeclBase renamed_sym = _rename_map[n.get_symbol()];
            Nodecl::Symbol s = renamed_sym.as<Nodecl::Symbol>();
            std::cerr << "Renaming symbol from " << n.get_symbol().get_name() << " to " << s.get_symbol().get_name() << std::endl;
            return ObjectList<Nodecl::NodeclBase>(1, renamed_sym);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
   
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_nest = walk(n.get_nest());
        
        if (!renamed_nest.empty())
        {
            nodecl_t nest = create_nodecl_list(renamed_nest);
            nodecl_t renamed_expr_stmt = nodecl_make_expression_statement(nest, _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_expr_stmt);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
   
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_nest = walk(n.get_nest());
        
        if (!renamed_nest.empty())
        {
            nodecl_t nest = create_nodecl_list(renamed_nest);
            nodecl_t renamed_parenth_expr = nodecl_make_parenthesized_expression(nest, n.get_type().get_internal_type(),
                                                                                _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_parenth_expr);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Conversion& n)
    {
        ObjectList<Nodecl::NodeclBase> nest = walk(n.get_nest());
        
        
        ObjectList<Nodecl::NodeclBase> renamed_nest = walk(n.get_nest());
        
        if (!renamed_nest.empty())
        {
            nodecl_t nest = create_nodecl_list(renamed_nest);
            nodecl_t renamed_conversion = nodecl_make_conversion(nest, n.get_type().get_internal_type(),
                                                                 _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_conversion);
        }
        
        return ObjectList<Nodecl::NodeclBase>();        
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ObjectInit& n)
    {
        // The symbol of an ObjectInit will never be renamed!!
        ObjectList<Nodecl::NodeclBase> init_expr = walk(n.get_symbol().get_initialization());
        if (!init_expr.empty())
        {
            nodecl_t renamed_object_init = nodecl_make_object_init(create_symbol(n, init_expr[0]), _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_object_init);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }   
   
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::FunctionCall& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_called = walk(n.get_called());
        ObjectList<Nodecl::NodeclBase> renamed_args = walk(n.get_arguments());
        
//         if (!renamed_called.empty() || !renamed_args.empty())
//         {
//             Nodecl::NodeclBase called = n.get_called();
//             if (!renamed_called.empty())
//             {
//                 called = renamed_called[0];
//             }
// 
//             nodecl_t subscripts;
//             if (renamed_subscripts.empty())
//             {
//                 subscripts = n.get_subscripts().get_internal_nodecl();
//             }
//             else
//             {
//                 subscripts = create_nodecl_list(renamed_subscripts);
//             }
// 
//             nodecl_t args;
//             if (renamed_args.empty())
//             {
//                 args = n.get_arguments().get_internal_nodecl();
//             }
//             else
//             {
//                 args = create_nodecl_list(renamed_args);
//             }
//             
//             nodecl_t renamed_func_call = nodecl_make_function_code(nodecl_t context_0, nodecl_t mem_init_seq_opt_0, nodecl_t
//                                                                    function_code_seq_opt_0, scope_entry_t* symbol, 
//                                                                    _filename, _line);
//             
//             return ObjectList<Nodecl::NodeclBase>(1, renamed_func_call);
//         }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
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
            Nodecl::NodeclBase subscripted = n.get_subscripted();
            if (!renamed_subscripted.empty())
            {
                subscripted = renamed_subscripted[0];
            }
            
            nodecl_t subscripts;
            if (renamed_subscripts.empty())
            {
                subscripts = n.get_subscripts().get_internal_nodecl();
            }
            else
            {
                subscripts = create_nodecl_list(renamed_subscripts);
            }
            
            nodecl_t renamed_array_subscript = nodecl_make_array_subscript(subscripted.get_internal_nodecl(), 
                                                                           subscripts,
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
            Nodecl::NodeclBase subscripted = n.get_subscripted();
            if (renamed_subscripted.empty())
            {
                subscripted = renamed_subscripted[0];
            }
            
            Nodecl::NodeclBase lower = n.get_lower();
            if (renamed_lower.empty())
            {
                lower = renamed_lower[0];
            }

            Nodecl::NodeclBase upper = n.get_upper();
            if (renamed_upper.empty())
            {
                upper = renamed_upper[0];
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
   
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Concat& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_lhs = walk(n.get_lhs());
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(n.get_rhs());
        
        if (!renamed_lhs.empty() || !renamed_rhs.empty())
        {
            Nodecl::NodeclBase lhs = renamed_lhs[0];
            if (renamed_lhs.empty())
            {
                lhs = n.get_lhs();
            }
            
            Nodecl::NodeclBase rhs = renamed_rhs[0];
            if (renamed_rhs.empty())
            {
                rhs = n.get_rhs();
            }
            
            nodecl_t renamed_concat = nodecl_make_concat(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                         n.get_type().get_internal_type(),
                                                         _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_concat);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::New& n)
    {
        // TODO Cal mirar aquí si és un array, i en aquest cas, la mida amb que s'inicialitza
        // int* n = new int[i];
        unhandled_node(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Delete& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(n.get_rhs());
        
        if (!renamed_rhs.empty())
        {
            nodecl_t renamed_delete = nodecl_make_delete(renamed_rhs[0].get_internal_nodecl(), 
                                                         n.get_type().get_internal_type(), 
                                                         _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_delete);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::DeleteArray& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(n.get_rhs());
        
        if (!renamed_rhs.empty())
        {
            nodecl_t renamed_delete_array = nodecl_make_delete_array(renamed_rhs[0].get_internal_nodecl(), 
                                                                     n.get_type().get_internal_type(), 
                                                                     _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_delete_array);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    template <typename T>
    CfgRenamingVisitor::Ret CfgRenamingVisitor::empty_visit(const T& n)
    {   // nothing to do
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Offsetof& n)
    {
        return empty_visit(n);
    }    
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Sizeof& n)
    {   // No expression has arrived here, the type has already been synthesized
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Type& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Typeid& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Cast& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Offset& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::StringLiteral& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BooleanLiteral& n)
    {   
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::IntegerLiteral& n)
    {   
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ComplexLiteral& n)
    {   
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::FloatingLiteral& n)
    {
        empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::StructuredValue& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_items = walk(n.get_items());
       
        if (!renamed_items.empty())
        {
            nodecl_t items = create_nodecl_list(renamed_items);
            nodecl_t renamed_structured_value = nodecl_make_structured_value(items, n.get_type().get_internal_type(), 
                                                                             _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_structured_value);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::EmptyStatement& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_value = walk(n.get_value());
        
        if (!renamed_value.empty())
        {
            nodecl_t renamed_return = nodecl_make_return_statement(renamed_value[0].get_internal_nodecl(), 
                                                                   _filename, _line);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::GotoStatement& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_stmt = walk(n.get_statement());
        
        if (!renamed_stmt.empty())
        {
            nodecl_t renamed_labeled_stmt = nodecl_make_labeled_statement(create_nodecl_list(renamed_stmt), 
                                                                          create_symbol(n, Nodecl::NodeclBase::null()),
                                                                          _filename, _line);
            return ObjectList<Nodecl::NodeclBase>(1, renamed_labeled_stmt);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }
   
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BreakStatement& n)
    {
        return empty_visit(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Text& n)
    {
        return empty_visit(n);
    }
    
    template <typename T>
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit_binary(const T& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_lhs = walk(n.get_lhs());
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(n.get_rhs());
        
        if (!renamed_lhs.empty() || !renamed_rhs.empty())
        {
            Nodecl::NodeclBase lhs = renamed_lhs[0];
            if (renamed_lhs.empty())
            {
                lhs = n.get_lhs();
            }

            Nodecl::NodeclBase rhs = renamed_rhs[0];
            if (renamed_rhs.empty())
            {
                rhs = n.get_rhs();
            }
            
            nodecl_t renamed_binary;
            if (n.template is<Nodecl::Assignment>())
            {
                renamed_binary = nodecl_make_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                        n.get_type().get_internal_type(), 
                                                        _filename, _line);
            }
            else if (n.template is<Nodecl::AddAssignment>())
            {
                renamed_binary = nodecl_make_add_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }
            else if (n.template is<Nodecl::SubAssignment>())
            {
                renamed_binary = nodecl_make_sub_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }
            else if (n.template is<Nodecl::DivAssignment>())
            {
                renamed_binary = nodecl_make_div_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }            
            else if (n.template is<Nodecl::MulAssignment>())
            {
                renamed_binary = nodecl_make_mul_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }
            else if (n.template is<Nodecl::ModAssignment>())
            {
                renamed_binary = nodecl_make_mod_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseAndAssignment>())
            {
                renamed_binary = nodecl_make_bitwise_and_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                                    n.get_type().get_internal_type(), 
                                                                    _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseOrAssignment>())
            {
                renamed_binary = nodecl_make_bitwise_or_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                                   n.get_type().get_internal_type(), 
                                                                   _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseXorAssignment>())
            {
                renamed_binary = nodecl_make_bitwise_xor_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                                    n.get_type().get_internal_type(), 
                                                                    _filename, _line);
            }
            else if (n.template is<Nodecl::ShrAssignment>())
            {
                renamed_binary = nodecl_make_shr_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }
            else if (n.template is<Nodecl::ShlAssignment>())
            {
                renamed_binary = nodecl_make_shl_assignment(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                            n.get_type().get_internal_type(), 
                                                            _filename, _line);
            }
            else if (n.template is<Nodecl::Add>())
            {
                renamed_binary = nodecl_make_add(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                 n.get_type().get_internal_type(), 
                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::Minus>())
            {
                renamed_binary = nodecl_make_minus(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                   n.get_type().get_internal_type(), 
                                                   _filename, _line);
            }
            else if (n.template is<Nodecl::Mul>())
            {
                renamed_binary = nodecl_make_mul(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                 n.get_type().get_internal_type(), 
                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::Div>())
            {
                renamed_binary = nodecl_make_div(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                 n.get_type().get_internal_type(), 
                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::Mod>())
            {
                renamed_binary = nodecl_make_mod(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                 n.get_type().get_internal_type(), 
                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::Power>())
            {
                renamed_binary = nodecl_make_power(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                   n.get_type().get_internal_type(), 
                                                   _filename, _line);
            }
            else if (n.template is<Nodecl::LogicalAnd>())
            {
                renamed_binary = nodecl_make_logical_and(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                         n.get_type().get_internal_type(), 
                                                         _filename, _line);
            }
            else if (n.template is<Nodecl::LogicalOr>())
            {
                renamed_binary = nodecl_make_logical_or(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                        n.get_type().get_internal_type(), 
                                                        _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseAnd>())
            {
                renamed_binary = nodecl_make_bitwise_and(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                         n.get_type().get_internal_type(), 
                                                         _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseOr>())
            {
                renamed_binary = nodecl_make_bitwise_or(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                        n.get_type().get_internal_type(), 
                                                        _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseXor>())
            {
                renamed_binary = nodecl_make_bitwise_xor(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                         n.get_type().get_internal_type(), 
                                                         _filename, _line);
            }
            else if (n.template is<Nodecl::Equal>())
            {
                renamed_binary = nodecl_make_equal(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                   n.get_type().get_internal_type(), 
                                                   _filename, _line);
            }
            else if (n.template is<Nodecl::Different>())
            {
                renamed_binary = nodecl_make_different(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                       n.get_type().get_internal_type(), 
                                                       _filename, _line);
            }
            else if (n.template is<Nodecl::LowerThan>())
            {
                renamed_binary = nodecl_make_lower_than(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                        n.get_type().get_internal_type(), 
                                                        _filename, _line);
            }
            else if (n.template is<Nodecl::GreaterThan>())
            {
                std::cerr << "Making new Nodecl for Greater Than " << std::endl;
                renamed_binary = nodecl_make_greater_than(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                          n.get_type().get_internal_type(), 
                                                          _filename, _line);
            }
            else if (n.template is<Nodecl::LowerOrEqualThan>())
            {
                renamed_binary = nodecl_make_lower_or_equal_than(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                                 n.get_type().get_internal_type(), 
                                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::GreaterOrEqualThan>())
            {
                renamed_binary = nodecl_make_greater_or_equal_than(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                                   n.get_type().get_internal_type(), 
                                                                   _filename, _line);
            }
            else if (n.template is<Nodecl::Shr>())
            {
                renamed_binary = nodecl_make_shr(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                 n.get_type().get_internal_type(), 
                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::Shl>())
            {
                renamed_binary = nodecl_make_shl(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                 n.get_type().get_internal_type(), 
                                                 _filename, _line);
            }
            else if (n.template is<Nodecl::Comma>())
            {
                renamed_binary = nodecl_make_comma(lhs.get_internal_nodecl(), rhs.get_internal_nodecl(), 
                                                   n.get_type().get_internal_type(), 
                                                   _filename, _line);
            }
            
            return ObjectList<Nodecl::NodeclBase>(1, renamed_binary);
        }
        
        return ObjectList<Nodecl::NodeclBase>();
    }

    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Assignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::AddAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::SubAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::DivAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::MulAssignment& n)
    
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ModAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        return visit_binary(n);
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
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Equal& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Different& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LowerThan& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::GreaterThan& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        return visit_binary(n);
    }
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
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
    
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit(const Nodecl::Comma& n)
    {
        return visit_binary(n);
    }
        
    template <typename T>
    CfgRenamingVisitor::Ret CfgRenamingVisitor::visit_unary(const T& n)
    {
        ObjectList<Nodecl::NodeclBase> renamed_rhs = walk(n.get_rhs());
        
        if (!renamed_rhs.empty())
        {
            nodecl_t rhs = renamed_rhs[0].get_internal_nodecl();
            
            nodecl_t renamed_unary;
            if (n.template is<Nodecl::Predecrement>())
            {
                renamed_unary = nodecl_make_predecrement(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Postdecrement>())
            {
                renamed_unary = nodecl_make_postdecrement(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Preincrement>())
            {
                renamed_unary = nodecl_make_preincrement(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Postincrement>())
            {
                renamed_unary = nodecl_make_postincrement(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Plus>())
            {
                renamed_unary = nodecl_make_plus(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Neg>())
            {
                renamed_unary = nodecl_make_neg(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::BitwiseNot>())
            {
                renamed_unary = nodecl_make_bitwise_not(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::LogicalNot>())
            {
                renamed_unary = nodecl_make_logical_not(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Derreference>())
            {
                renamed_unary = nodecl_make_derreference(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Reference>())
            {
                renamed_unary = nodecl_make_reference(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            else if (n.template is<Nodecl::Derreference>())
            {
                renamed_unary = nodecl_make_derreference(rhs, n.get_type().get_internal_type(), _filename, _line);
            }
            
            return ObjectList<Nodecl::NodeclBase>(1, renamed_unary);
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
}