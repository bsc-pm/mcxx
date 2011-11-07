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

#include "cxx-cexpr.h"
#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-cfg-analysis-visitor.hpp"
#include "tl-nodecl-calc.hpp"


namespace TL
{
    static ObjectList<Symbol> get_symbols(Nodecl::NodeclBase n)
    {
        if (n.get_symbol().is_valid())
        {
            return ObjectList<Symbol>(1, n.get_symbol());
        }
        
        ObjectList<Symbol> result;
        ObjectList<Nodecl::NodeclBase> children = n.children();
        for(ObjectList<Nodecl::NodeclBase>::iterator it = children.begin(); it != children.end(); ++it)
        {
            result.append(get_symbols(*it));
        }
        
        return result;
    }

    static Nodecl::NodeclBase compute_init_expr(Nodecl::NodeclBase n, Nodecl::NodeclBase stride, int op)
    {
        Nodecl::NodeclBase val;
        switch (op)
        {
            case 0:     val = Nodecl::Add::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 1:     val = Nodecl::Minus::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 2:     val = Nodecl::Div::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 3:     val = Nodecl::Mul::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 4:     val = Nodecl::Mod::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 5:     val = Nodecl::BitwiseAnd::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 6:     val = Nodecl::BitwiseOr::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 7:     val = Nodecl::BitwiseXor::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 8:     val = Nodecl::Shl::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 9:     val = Nodecl::Shr::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            default:    internal_error("Unexpected type of operation '%d' while computing initial expression", op);
        }
        
        Nodecl::Calculator calc;
        const_value_t* const_val = calc.compute_const_value(val);
        Nodecl::NodeclBase result;
        if (const_val != NULL)
        {
            if (n.is<Nodecl::IntegerLiteral>())
            {
                result = Nodecl::IntegerLiteral::make(n.get_type(), const_val, n.get_filename(), n.get_line());
            }
            else if (n.is<Nodecl::FloatingLiteral>())
            {
                result = Nodecl::FloatingLiteral::make(n.get_type(), const_val, n.get_filename(), n.get_line());
            }
            else
            {
                internal_error("Unexpected node type '%s' while computing initial value in a constant expression", 
                               ast_print_node_type(n.get_kind()));
            }
        }
        else
        {
            result = val;
        }
        
        return result;
    }

    CfgAnalysisVisitor::CfgAnalysisVisitor(Node* n)
        : _node(n), _define(false), _actual_nodecl(Nodecl::NodeclBase::null())
    {}

    CfgAnalysisVisitor::CfgAnalysisVisitor(const CfgAnalysisVisitor& v)
        : _node(v._node), _define(v._define), _actual_nodecl(v._actual_nodecl)
    {} 

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        std::cerr << "Unhandled node during CFG Analysis'" << codegen_to_str(n.get_internal_nodecl())
                  << "' of type '" << ast_print_node_type(n.get_kind()) << "'" << std::endl;
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Symbol& n)
    {
        Nodecl::NodeclBase defined_var = n;
        
        if (_actual_nodecl.is_null())
        {   
            _node->fill_use_def_sets(n, _define);
        }
        else
        {
            defined_var = _actual_nodecl;
            _node->fill_use_def_sets(_actual_nodecl, _define);
            _actual_nodecl = Nodecl::NodeclBase::null();
        }
        
        if (!_init_expression.is_null())
        {
//             _node->set_reaching_definition(defined_var, _init_expression);
            _init_expression = Nodecl::NodeclBase::null();
        }
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ObjectInit& n)
    {
        Symbol s = n.get_symbol();
       
        Nodecl::Symbol sym_node = Nodecl::Symbol::make(s, n.get_filename(), n.get_line());
        _node->fill_use_def_sets(sym_node, true);
       
        Nodecl::NodeclBase init = s.get_initialization();
        
        if (!init.is_null())
        {
//             _node->set_reaching_definition(sym_node, init);
            walk(init);
        }
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        if (_actual_nodecl.is_null())
        {
            _actual_nodecl = n;
        }
        
        walk(n.get_subscripted());
        _define = false;        // We may come form a LHS walk and subscripts not defined!
        walk(n.get_subscripts());
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

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Assignment& n)
    {
        Nodecl::NodeclBase assig = n;
        walk(n.get_rhs());
        _init_expression = n.get_rhs();
        _define = true;
        walk(n.get_lhs());
        _define = false;
    }
    
    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::binary_assignment(const T& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase lhs = n.get_lhs();
        walk(rhs);
        walk(lhs);
        _define = true;
        if (n.template is<Nodecl::AddAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 0);
        }
        else if (n.template is<Nodecl::SubAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 1);
        }
        else if (n.template is<Nodecl::DivAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 2);
        }
        else if (n.template is<Nodecl::MulAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 3);
        }
        else if (n.template is<Nodecl::ModAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 4);
        }
        else if (n.template is<Nodecl::BitwiseAndAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 5);
        }
        else if (n.template is<Nodecl::BitwiseOrAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 6);
        }
        else if (n.template is <Nodecl::BitwiseXorAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 7);
        }
        else if (n.template is <Nodecl::ShlAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 8);
        }
        else if (n.template is <Nodecl::ShrAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 9);
        }
        else
        {
            Nodecl::NodeclBase node = n;
            internal_error("Non add or sub assignment not yet implemented in CFG analysis. Founded '%s'", 
                           node.prettyprint().c_str());
        }
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
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Shr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Shl& n)
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
   
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Predecrement& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        walk(rhs);
        _define = true;
        nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
        _init_expression = compute_init_expr(rhs, Nodecl::NodeclBase(one), 1);
        walk(rhs);
        _define = false;
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Postdecrement& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        walk(rhs);
        _define = true;
        nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
        _init_expression = compute_init_expr(rhs, Nodecl::NodeclBase(one), 1);
        walk(rhs);
        _define = false;
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Preincrement& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        walk(rhs);
        _define = true;
        nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
        _init_expression = compute_init_expr(rhs, Nodecl::NodeclBase(one), 0);
        walk(rhs);
        _define = false;
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Postincrement& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        walk(rhs);
        _define = true;
        nodecl_t one = const_value_to_nodecl(const_value_get_one(/* bytes */ 4, /* signed*/ 1));
        _init_expression = compute_init_expr(rhs, Nodecl::NodeclBase(one), 0);
        walk(rhs);
        _define = false;
    }

    template <typename T>
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::unary_visit(const T& n)
    {
        walk(n.get_rhs());
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
    
    /// *** GLOBAL VARIABLES VISITOR *** //
    
    CfgRecursiveAnalysisVisitor::CfgRecursiveAnalysisVisitor(Scope sc)
        : _sc(sc), _global_vars(), _parameters(), _defining(false), _reference_params_to_args()
    {}
    
    void CfgRecursiveAnalysisVisitor::set_param_to_args_mapping(std::map<Symbol, Nodecl::NodeclBase> params_to_args)
    {
        _reference_params_to_args = params_to_args;
    }
    
    bool CfgRecursiveAnalysisVisitor::usage_list_contains_sym(Nodecl::Symbol n, char list)
    {
        if (list == '0')
        {
            for (ObjectList<struct var_usage_t*>::iterator it = _global_vars.begin(); it != _global_vars.end(); ++it)
            {
                if (Nodecl::Utils::equal_nodecls((*it)->get_nodecl(), n))
                {
                    return true;
                }
            }
        }
        else
        {
            for (ObjectList<struct var_usage_t*>::iterator it = _parameters.begin(); it != _parameters.end(); ++it)
            {
                if (Nodecl::Utils::equal_nodecls((*it)->get_nodecl(), n))
                {
                    return true;
                }
            }
        }
        return false;
    }
    
    struct var_usage_t* CfgRecursiveAnalysisVisitor::get_global_variable_in_list(Nodecl::Symbol n)
    {
        for (ObjectList<struct var_usage_t*>::iterator it = _global_vars.begin(); it != _global_vars.end(); ++it)
        {
            if (Nodecl::Utils::equal_nodecls((*it)->get_nodecl(), n))
            {
                return *it;
            }
        }
        
        internal_error("No symbol '%s' founded in global variable list", n.get_symbol().get_name().c_str());
    }
    
    struct var_usage_t* CfgRecursiveAnalysisVisitor::get_parameter_in_list(Nodecl::Symbol n)
    {
        for (ObjectList<struct var_usage_t*>::iterator it = _parameters.begin(); it != _parameters.end(); ++it)
        {
            if (Nodecl::Utils::equal_nodecls((*it)->get_nodecl(), n))
            {
                return *it;
            }
        }
        
        internal_error("No symbol '%s' founded in parameters list", n.get_symbol().get_name().c_str());
    }
    
    ObjectList<struct var_usage_t*> CfgRecursiveAnalysisVisitor::get_global_variables_usage() const
    {
        return _global_vars;
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::Symbol& n)
    {
        Symbol s = n.get_symbol();
        Scope s_sc = s.get_scope();
      
        if (!s_sc.scope_is_enclosed_by(_sc))
        {   // The symbol is a global variable
            if (usage_list_contains_sym(n, /*global variable*/ '0'))
            {
                struct var_usage_t* global_var = get_global_variable_in_list(n);
                char usage = global_var->get_usage();
                if (usage == '0' || usage == '2')
                {   // nothing to do: It doesn't matters what happens with an already Killed variable
                }
                else if (usage == '1')
                {
                    if (_defining)
                    {   // Set to 2 the usage value
                        global_var->set_usage('2');
                    }
                    else {} // nothing to do, the variable was already used
                }
            }
            else
            {
                char usage;
                if (_defining) usage = '0';
                else usage = '1';
                
                struct var_usage_t* new_global_var_usage = new var_usage_t(n, usage);
                _global_vars.insert(new_global_var_usage);
            }
        }
        else
        {   
            if (!_reference_params_to_args.empty())
            {
                if (_reference_params_to_args.find(n.get_symbol()) != _reference_params_to_args.end())
                {   // This symbol is a parameter
                    if (usage_list_contains_sym(n, /*parameter*/ '1'))
                    {
                        struct var_usage_t* parameter = get_parameter_in_list(n);
                        char usage = parameter->get_usage();
                        if (usage == '0' || usage == '2')
                        {   // nothing to do: It doesn't matters what happens with an already Killed variable
                        }
                        else if (usage == '1')
                        {
                            if (_defining)
                            {   // Set to 2 the usage value
                                parameter->set_usage('2');
                            }
                            else {} // nothing to do, the variable was already used
                        }
                    }
                    else
                    {
                        char usage;
                        if (_defining) usage = '0';
                        else usage = '1';
                        
                        struct var_usage_t* new_parameter_usage = new var_usage_t(n, usage);
                        _parameters.insert(new_parameter_usage);
                    }
                }
            }
        }
    }
    
    template <typename T>
    void CfgRecursiveAnalysisVisitor::op_assignment_visit(const T& n)
    {
        walk(n.get_lhs());
        _defining = true;
        walk(n.get_lhs());
        _defining = false;
        walk(n.get_rhs());        
    }

    template <typename T>
    void CfgRecursiveAnalysisVisitor::unary_visit(const T& n)
    {
        walk(n.get_rhs());
        _defining = true;
        walk(n.get_rhs());
        _defining = false;        
    }

    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::Assignment& n)
    {
        _defining = true;
        walk(n.get_lhs());
        _defining = false;
        walk(n.get_rhs());  
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::AddAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::SubAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::DivAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::MulAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::ModAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::Predecrement& n)
    {
        unary_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::Postdecrement& n)
    {
        unary_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::Preincrement& n)
    {
        unary_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::Postincrement& n)
    {
        unary_visit(n);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::FunctionCall& n)
    {
        //TODO
        internal_error("Not yet implemented", 0);
    }
    
    CfgRecursiveAnalysisVisitor::Ret CfgRecursiveAnalysisVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        //TODO
        internal_error("Not yet implemented", 0);
    }
    
}
