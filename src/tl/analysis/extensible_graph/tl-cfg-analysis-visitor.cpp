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
#include "cxx-process.h"

#include "tl-cfg-analysis-visitor.hpp"

namespace TL
{
    CfgAnalysisVisitor::CfgAnalysisVisitor(Node* n)
        : _node(n), _define(false), _actual_nodecl(Nodecl::NodeclBase::null())
    {}

    CfgAnalysisVisitor::CfgAnalysisVisitor(const CfgAnalysisVisitor& v)
        : _node(v._node), _define(v._define), _actual_nodecl(v._actual_nodecl)
    {}

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        std::cerr << "Unhandled node during CFG Analysis'" << c_cxx_codegen_to_str(n.get_internal_nodecl())
                  << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Symbol& n)
    {
        _node->fill_use_def_sets(n.get_symbol(), _define, _actual_nodecl);
        _actual_nodecl = Nodecl::NodeclBase::null();
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Text& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ContinueStatement& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BreakStatement& n)
    {   // do nothing
    }    
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::literal_visit(const T& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::StringLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BooleanLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::IntegerLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ComplexLiteral& n)
    {
        literal_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::FloatingLiteral& n)
    {
        literal_visit(n);
    }

    // TODO
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::func_call(const T& n)
    {
        
//         CfgArgumentVisitor new_visitor();
//         new_visitor.walk(n);
        
        /*Type t = n.get_type();
        
        bool has_ellipsis;
        ObjectList<Type> parameters_types = t.parameters(has_ellipsis);
        Nodecl::List arguments(n.get_internal_nodecl());*/        
        
//         ObjectList<Nodecl::NodeclBase>::iterator ita = arguments.begin();
//         for (ObjectList<Type>::iterator itp = parameters_types.begin();
//             itp != parameters_types.end();
//             ++itp, ++ita)
//         {   // While we can use the parameters (arguments that are not ellipsis), we do
//             // FIXME Escape analysis??
//             if (itp->is_reference() || itp->is_pointer())
//             {
//                 ObjectList<Symbol> syms = (*ita).get_symbols();
//                 if (!syms.empty())
//                 {
//                     for (ObjectList<Symbol>::iterator its = syms.begin();
//                         it != syms.end();
//                         ++it)
//                     {
//                         _node->fill_use_def_sets(ita.get_symbol(), false);
//                         _node->fill_use_def_sets(ita.get_symbol(), true);
//                     }
//                 }
//             }
//         }
        
//         if (has_ellipsis)
//         {
//             for(; ita != arguments.end(); ++ita)
//             {
//                 // FIXME get_type will not work if Fortran when argument is 'named-pair' or 'alt-return'
//                 if (ita->is_reference() || ita->is_pointer())
//                 {
//                     _node->fill_use_def_sets(ita.get_symbol(), false);
//                     _node->fill_use_def_sets(ita.get_symbol(), true);
//                 }
//             }
//         }
             
        //         else if (e.is_function_call())
//         {
//             Expression called_expression = e.get_called_expression();
//             Type type = called_expression.get_type();
//             ObjectList<Type> parameter_types = type.parameters(has_ellipsis);
//             
//             ObjectList<Expression> args = e.get_argument_list();
//             ObjectList<Type>::iterator itp = parameter_types.begin();
//             ObjectList<Expression>::iterator ita = args.begin();
//             for(; itp != parameter_types.end(); ita++, itp++)
//             {
//                 // Regarding the parameter, if possible (arguments that are not in ellipsis)
//                 if ((itp->is_pointer() && !itp->points_to().is_const()) || 
//                     (itp->is_reference() && !itp->references_to().is_const()))
//                 {
// //                     std::cerr << "Parameter " << ita->prettyprint() 
// //                               << " is pointer and not const" << std::endl;
//                     // Assuming that the argument is used and defined, in this order
//                     set_live_initial_expression_information(*ita, /* Defined */ false);
//                     set_live_initial_expression_information(*ita, /* Defined */ true);
//                 }
//                 else
//                 {
// //                     std::cerr << "Parameter " << ita->prettyprint() 
// //                               << " is not pointer or const" << std::endl;
//                     set_live_initial_expression_information(*ita, /* Defined */ false);
//                 }
//             }
//             if (has_ellipsis)
//             {
//                 // There are more arguments than parameters, so we must regard the argument
//                 for(; ita != args.end(); ita++)
//                 {
//                     if ((ita->get_type().is_pointer() && !ita->get_type().points_to().is_const()) ||
//                         (ita->get_type().is_reference() && 
//                              !ita->get_type().references_to().is_const()))
//                     {
// //                         std::cerr << "Argument " << ita->prettyprint() 
// //                                   << " is pointer and not const" << std::endl;
//                         // Assuming that the argument is used and defined, in this order
//                         set_live_initial_expression_information(*ita, /* Defined */ false);
//                         set_live_initial_expression_information(*ita, /* Defined */ true);
//                     }
//                     else
//                     {    
// //                         std::cerr << "Argument " << ita->prettyprint() 
// //                                   << " is not pointer or const" << std::endl;
//                         set_live_initial_expression_information(*ita, /* Defined */ false);
//                     }
//                 }
//             }
//         }        
        
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        func_call(n);
    }
   
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::FunctionCall& n)
    {
        func_call(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ObjectInit& n)
    {
        _node->fill_use_def_sets(n.get_symbol(), true);
        walk(n.get_symbol().get_initialization());
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Assignment& n)
    {
        walk(n.get_rhs());
        _define = true;
        walk(n.get_lhs());
        _define = false;
    }
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::binary_assignment(const T& n)
    {
        walk(n.get_rhs());        
        walk(n.get_lhs());
        _define = true;
        walk(n.get_lhs());
        _define = false;
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::AddAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::SubAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::DivAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::MulAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ModAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        binary_assignment(n);
    }
        
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Throw& n)
    {
        walk(n.get_rhs());
    }
    
    template<typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::nested_visit(const T& n)
    {
        walk(n.get_nest());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ExpressionStatement& n)
    {
        nested_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ParenthesizedExpression& n)
    {
        unhandled_node(n);
//         nested_visit(n);
    }

    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::binary_visit(const T& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Comma& n)
    {
        binary_visit(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Concat& n)
    {
        binary_visit(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Add& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Minus& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Mul& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Div& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Mod& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Power& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LogicalOr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseAnd& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseOr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseXor& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Equal& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Different& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LowerThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::GreaterThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Shr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Shl& n)
    {
        binary_visit(n);
    }
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::unary_visit(const T& n)
    {
        walk(n.get_rhs());
    }    
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Predecrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Postdecrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Preincrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Postincrement& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Plus& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Neg& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseNot& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LogicalNot& n)
    {
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Derreference& n)
    {
        if (_actual_nodecl.is_null())
        {    
            _actual_nodecl = n;
        }
        
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Reference& n)
    {
        if (_actual_nodecl.is_null())
        {    
            _actual_nodecl = n;
        }
        
        unary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        walk(n.get_subscripted());
        walk(n.get_subscripts());
    }
   
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ArraySection& n)
    {
        unhandled_node(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        if (_actual_nodecl.is_null())
        {    
            _actual_nodecl = n;
        }
        
        // walk(n.get_lhs());  // In a member access, the use/definition is always of the member, not the base
        walk(n.get_member());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::New& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Delete& n)
    {   // FIXME We should specify the object destruction
        // walk(n.get_rhs());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::DeleteArray& n)
    {   // FIXME We should specify the object destruction
        // walk(n.get_rhs());
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Offsetof& n)
    {   // do nothing
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Sizeof& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Type& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Typeid& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Cast& n)
    {
        unhandled_node(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Offset& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::StructuredValue& n)
    {
        walk(n.get_items());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::EmptyStatement& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        walk(n.get_value());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::GotoStatement& n)
    {   // do nothing
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::LabeledStatement& n)
    {
        walk(n.get_statement());
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Conversion& n)
    {
        walk(n.get_nest());
    }
}
