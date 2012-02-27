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


#include "cxx-cexpr.h"
#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-cfg-analysis-visitor.hpp"
#include "tl-nodecl-calc.hpp"


namespace TL
{
namespace Analysis
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
    
    static void get_use_def_variables(Node* actual, int id_target_node, ext_sym_set &ue_vars, ext_sym_set &killed_vars)
    {
        ObjectList<Node*> children = actual->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
        {
            if ((*it)->get_id() != id_target_node)
            {
                ue_vars.append((*it)->get_ue_vars());
                killed_vars.append((*it)->get_killed_vars());
                get_use_def_variables(*it, id_target_node, ue_vars, killed_vars);
            }
        }
    }
   
    /*!
     * We reach this point when a function call is founded inside another expression such as "b + f(a)" or "return g(b, c)".
     * The Use-Def for the node FUNCTION_CALL_NODE has been already computed in > tl-static-analysis > set_live_initial_information.
     * FIXME May be we want to move the computation of the FUNCTION_CALL_NODE to this class.
     * 
     * This method only propagates the previously computed usage information of the function to the actual "split-node".
     */
    template <typename T>
    void CfgAnalysisVisitor::function_visit(const T& n)
    {
        Node* outer_node = _node->get_outer_node();
        ext_sym_set ue_vars, killed_vars;
        get_use_def_variables(outer_node->get_graph_entry_node(), _node->get_id(), ue_vars, killed_vars);
        _node->set_ue_var(ue_vars);
        _node->set_killed_var(killed_vars);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::FunctionCall& n)
    {
        function_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        function_visit(n);
    }
        
    
    /// *** GLOBAL VARIABLES & FUNCTION PARAMETERS VISITOR *** //
    
    CfgIPAVisitor::CfgIPAVisitor(ExtensibleGraph* cfg, ObjectList<ExtensibleGraph*> cfgs,
                                 ObjectList<var_usage_t*> glob_vars, ObjectList<Symbol> reference_params, 
                                 std::map<Symbol, Nodecl::NodeclBase> params_to_args)
        : _cfg(cfg), _cfgs(cfgs), _global_vars(glob_vars), _ref_params(), _usage(), _defining(false), 
          _params_to_args(params_to_args), _visited_functions()
    {
        Symbol s = cfg->get_function_symbol();
        if (s.is_valid())
        {
            _visited_functions.append(s);
        }
        else
        {
            internal_error("You shouldn't perform IPA in a graph which do not contain a whole function.", 0);
        }
    }
    
    void CfgIPAVisitor::compute_usage_rec(Node* node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            Node_type ntype = node->get_type();
            if (ntype == GRAPH_NODE)
            {
                compute_usage_rec(node->get_graph_entry_node());
            }
            else if (node->has_key(_NODE_STMTS))
            {
                ObjectList<Nodecl::NodeclBase> stmts = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    walk(*it);
                }
            }
            
            ObjectList<Node*> children = node->get_children();
            for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                compute_usage_rec(*it);
            }
        }
    }

    void CfgIPAVisitor::fill_graph_usage_info()
    {
        ObjectList<struct var_usage_t*> cfg_ref_params;
        for(ObjectList<struct var_usage_t*>::iterator it = _usage.begin(); it != _usage.end(); ++it)
        {
            Nodecl::NodeclBase var = (*it)->get_nodecl();
            if (var.is<Nodecl::Symbol>())
            {
                Nodecl::Symbol s = var.as<Nodecl::Symbol>();
                if (usage_list_contains_sym(s, _global_vars))
                {
                    struct var_usage_t* ipa_var = get_var_in_list(s, _usage);
                    struct var_usage_t* global_var = get_var_in_list(s, _global_vars);
                    global_var->set_usage(ipa_var->get_usage());
                }
                else if (_ref_params.contains(s.get_symbol()))
                {
                    struct var_usage_t* ipa_var = get_var_in_list(s, _usage);
                    cfg_ref_params.insert(ipa_var);
                }
                else
                {
                    // It can be a global var used in a called function within the current graph
                    
                    internal_error("Computed IPA in graph '%s' for the variable '%s' which is not in the global variables list "\
                                   "nor in the reference parameters list", _cfg->get_name().c_str(), var.prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Computed usage for an invalid symbol. It shouldn't happen", 0);
            }
        }
    }

    void CfgIPAVisitor::compute_usage()
    {
        Node* graph_node = _cfg->get_graph();
        
        compute_usage_rec(graph_node);
        
        fill_graph_usage_info();
        
        ExtensibleGraph::clear_visits(graph_node);
    }
  
    struct var_usage_t* CfgIPAVisitor::get_var_in_list(Nodecl::Symbol n, ObjectList<struct var_usage_t*> list)
    {
        for (ObjectList<struct var_usage_t*>::iterator it = list.begin(); it != list.end(); ++it)
        {
            if (Nodecl::Utils::equal_nodecls((*it)->get_nodecl(), n))
            {
                return *it;
            }
        }
        
        internal_error("No symbol '%s' founded in usage list", n.get_symbol().get_name().c_str());
    }
    
    struct var_usage_t* CfgIPAVisitor::get_var_in_list(Symbol n, ObjectList<struct var_usage_t*> list)
    {
        for (ObjectList<struct var_usage_t*>::iterator it = list.begin(); it != list.end(); ++it)
        {
            if ((*it)->get_nodecl().get_symbol() == n)
            {
                return *it;
            }
        }
        
        internal_error("No symbol '%s' founded in usage list", n.get_name().c_str());
    }
    
    ObjectList<struct var_usage_t*> CfgIPAVisitor::get_usage() const
    {
        return _usage;
    }
    
    void CfgIPAVisitor::set_up_symbol_usage(Nodecl::Symbol s)
    {
        if (usage_list_contains_sym(s, _usage))
        {   // The variable was already inserted in the result
            struct var_usage_t* ipa_var = get_var_in_list(s, _usage);
            char usage = ipa_var->get_usage();
            if (usage == '0' || usage == '2' || usage == '3')
            {   // nothing to do: It doesn't matters what happens with an already Killed variable or the value is undefined at this point
            }
            else if (usage == '1')
            {
                if (_defining)
                {   // Set to 2 the usage value
                    ipa_var->set_usage('2');
                }
                else {} // nothing to do, the variable was already used
            }
        }
        else
        {   // Is the first usage of this variable. We insert it in the result
            char usage;
            if (_defining) usage = '0';
            else usage = '1';
            struct var_usage_t* new_ipa_var = new var_usage_t(s, usage);
            _usage.insert(new_ipa_var);
        }
    }
    
    void CfgIPAVisitor::set_up_argument_usage(Nodecl::Symbol arg)
    {
        if (usage_list_contains_sym(arg, _usage))
        {
            struct var_usage_t* ipa_var = get_var_in_list(arg, _usage);
            char ipa_usage = ipa_var->get_usage();
            if (ipa_usage == '1')
                ipa_var->set_usage('3');
            else {} // If it was killed or undefined, we don't care now
        }
        else
        {
            struct var_usage_t* new_ipa_var = new var_usage_t(arg, '3');
            _usage.insert(new_ipa_var);
        }
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Symbol& n)
    {
        if (_ref_params.contains(n.get_symbol()))
        {
            Symbol param = n.get_symbol();
            Nodecl::Symbol arg = _params_to_args[param].as<Nodecl::Symbol>();
            set_up_symbol_usage(arg);
        }
        else if (TL::Analysis::usage_list_contains_sym(n, _global_vars))
        {
            set_up_symbol_usage(n);
        }
    }
    
    template <typename T>
    void CfgIPAVisitor::op_assignment_visit(const T& n)
    {
        walk(n.get_lhs());
        _defining = true;
        walk(n.get_lhs());
        _defining = false;
        walk(n.get_rhs());        
    }

    template <typename T>
    void CfgIPAVisitor::unary_visit(const T& n)
    {
        walk(n.get_rhs());
        _defining = true;
        walk(n.get_rhs());
        _defining = false;        
    }

    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Assignment& n)
    {
        _defining = true;
        walk(n.get_lhs());
        _defining = false;
        walk(n.get_rhs());  
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::AddAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::SubAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::DivAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::MulAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::ModAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::ShrAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::ShlAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Predecrement& n)
    {
        unary_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Postdecrement& n)
    {
        unary_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Preincrement& n)
    {
        unary_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Postincrement& n)
    {
        unary_visit(n);
    }
    
    std::map<Symbol, Nodecl::NodeclBase> CfgIPAVisitor::compute_nested_param_args(Nodecl::NodeclBase n, ExtensibleGraph* called_func_graph)
    {
        // Useless lists at that point, but they are a reference parameters to compute the mapping between parameters and arguments
        ObjectList<Symbol> params; Nodecl::List args;
        std::map<Symbol, Nodecl::NodeclBase> nested_params_to_args = map_params_to_args(n, called_func_graph, params, args);
        
        for(std::map<Symbol, Nodecl::NodeclBase>::iterator it = nested_params_to_args.begin(); 
            it != nested_params_to_args.end(); ++it)
        {
            Symbol param = it->first;
            Nodecl::NodeclBase arg = it->second;
            if (arg.is<Nodecl::Symbol>())
            {
                Symbol arg_sym = arg.get_symbol();
                if (_params_to_args.find(arg_sym) != _params_to_args.end())
                {
                    nested_params_to_args[param] = _params_to_args[arg_sym];
                }
                else
                {   // It is a reference parameter but the argument does not come from a reference parameter of the firs call
                    // We don't care about this parameter
                    nested_params_to_args.erase(param);
                }
            }
            else
            {}  // We don't care. If the argument is not a symbol, it cannot be a parameter by reference.
                // If argument expression contains a global variable, we take care of the usage in the called function analysis
                // Any other case does not matter for the analysis we are performing here
        }
        
        return nested_params_to_args;
    }
    
    template <typename T>
    void CfgIPAVisitor::function_visit(const T& n)
    {
        // Hem de calcular IPA per la crida a la funció i afegir tota la informació que obtinguem a la informació actual.
        Nodecl::FunctionCall call = n.template as<Nodecl::FunctionCall>();
        
        ExtensibleGraph* called_func_graph = find_function_for_ipa(n.get_called().get_symbol(), _cfgs);
        if (called_func_graph != NULL)
        {
            Symbol s = called_func_graph->get_function_symbol();
            if (s.is_valid())
            {
                if (!_visited_functions.contains(s))
                {
                    _visited_functions.append(s);
                    ObjectList<struct var_usage_t*> nested_global_vars = called_func_graph->get_global_variables();
                    for (ObjectList<struct var_usage_t*>::iterator it = nested_global_vars.begin(); it != nested_global_vars.end(); ++it)
                    {
                        if (!usage_list_contains_sym((*it)->get_nodecl(), _global_vars))
                        {
                            _global_vars.insert(*it);
                        }
                    }
                        
                    ObjectList<var_usage_t*> glob_vars = called_func_graph->get_global_variables();
                    ObjectList<Symbol> params = called_func_graph->get_function_parameters();
                    ObjectList<Symbol> reference_params;
                    for(ObjectList<Symbol>::iterator it = params.begin(); it != params.end(); ++it)
                    {
                        Type t = it->get_type();
                        if (t.is_any_reference() || t.is_pointer())
                        {
                            reference_params.append(*it);
                        }
                    }
                    if (!glob_vars.empty() || !reference_params.empty())
                    {   // Compute liveness for global variables and reference parameters
                        std::map<Symbol, Nodecl::NodeclBase> nested_params_to_args = compute_nested_param_args(n, called_func_graph);
                            
                        // Reconvert those parameters form the original call
                        CfgIPAVisitor ipa_visitor(called_func_graph, _cfgs, glob_vars, reference_params, nested_params_to_args);
                        ipa_visitor.compute_usage();
                        
                        // Propagate this information to the current graph analysis
                        ObjectList<struct var_usage_t*> ipa_usage = ipa_visitor.get_usage();
                        for (ObjectList<struct var_usage_t*>::iterator it = ipa_usage.begin(); it != ipa_usage.end(); ++it)
                        {
                            Nodecl::Symbol current_var = (*it)->get_nodecl();
                            if (usage_list_contains_sym(current_var, _usage))
                            {
                                struct var_usage_t* ipa_var = get_var_in_list(current_var, _usage);
                                char ipa_usage = ipa_var->get_usage();
                                if (ipa_usage == '1')
                                    ipa_var->set_usage('3');
                                else {} // If it was killed or undefined, we don't care now
                            }
                            else
                            {
                                _usage.insert(*it);
                            }
                        }
                    }
                }
                else
                {}  // Nothing to do, the analysis of this function is already included in the actual usage computation
            }
        }
        else
        {   // All global variables and reference parameters has an undefined behaviour from this point
            // If the variable was already killed, we don't care what happens from now on
            
            // Global variables
            for(ObjectList<struct var_usage_t*>::iterator it = _global_vars.begin(); it != _global_vars.end(); ++it)
            {
                Nodecl::Symbol current_var = (*it)->get_nodecl();
                set_up_argument_usage(current_var);
            }
            
            // Reference parameters of the current function used in the current function call
            ObjectList<Symbol> params = called_func_graph->get_function_parameters();
            Nodecl::List args;
            if (n.template is<Nodecl::FunctionCall>())
            {
                Nodecl::FunctionCall func_call_nodecl = n.template as<Nodecl::FunctionCall>();
                args = func_call_nodecl.get_arguments().as<Nodecl::List>();
            }
            else
            {   // is VirtualFunctionCall
                Nodecl::VirtualFunctionCall func_call_nodecl = n.template as<Nodecl::VirtualFunctionCall>();
                args = func_call_nodecl.get_arguments().as<Nodecl::List>();
            }
            ObjectList<Symbol>::iterator itp = params.begin();
            Nodecl::List::iterator ita = args.begin();
            for(; itp != params.end() && ita != args.end(); ++itp, ++ita)
            {
                Type t = itp->get_type();
                if (t.is_any_reference() || t.is_pointer())
                {   // A parameter in the function call is reference
                    if (_ref_params.contains(*itp))
                    {   // It was a reference in the current function
                        Nodecl::Symbol current_arg = ita->template as<Nodecl::Symbol>();
                        set_up_argument_usage(current_arg);
                    }
                }
            }
        }
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::FunctionCall& n)
    {
        function_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        function_visit(n);
    }
}
}
