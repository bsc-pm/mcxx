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
            case 8:     val = Nodecl::BitwiseShl::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 9:     val = Nodecl::BitwiseShr::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
                        break;
            case 10:    val = Nodecl::ArithmeticShr::make(n, stride, n.get_type(), n.get_filename(), n.get_line());
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
        std::cerr << "Unhandled node during CFG Analysis'" << codegen_to_str(n.get_internal_nodecl(), 
                nodecl_retrieve_context(n.get_internal_nodecl()))
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
            _node->set_reaching_definition(defined_var, _init_expression);
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
            _node->set_reaching_definition(sym_node, init);
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
        else if (n.template is<Nodecl::MinusAssignment>())
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
        else if (n.template is <Nodecl::BitwiseShlAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 8);
        }
        else if (n.template is <Nodecl::BitwiseShrAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 9);
        }
        else if (n.template is <Nodecl::ArithmeticShrAssignment>())
        {
            _init_expression = compute_init_expr(rhs, lhs, 10);
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
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::MinusAssignment& n)
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
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ArithmeticShrAssignment& n)
    {
        binary_assignment(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseShrAssignment& n)
    {
        binary_assignment(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseShlAssignment& n)
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
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseShr& n)
    {
        binary_visit(n);
    }

    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::ArithmeticShr& n)
    {
        binary_visit(n);
    }
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::BitwiseShl& n)
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
    
    CfgAnalysisVisitor::Ret CfgAnalysisVisitor::visit(const Nodecl::Dereference& n)
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
    
    static void get_use_def_variables(Node* actual, int id_target_node, ext_sym_set &ue_vars, ext_sym_set &killed_vars, ext_sym_set &undef_vars)
    {
        ObjectList<Node*> children = actual->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
        {
            if ((*it)->get_id() != id_target_node)
            {
                ue_vars.insert((*it)->get_ue_vars());
                killed_vars.insert((*it)->get_killed_vars());
                undef_vars.insert((*it)->get_undefined_behaviour_vars());
                
                get_use_def_variables(*it, id_target_node, ue_vars, killed_vars, undef_vars);
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
        ext_sym_set ue_vars, killed_vars, undef_vars;
        get_use_def_variables(outer_node->get_graph_entry_node(), _node->get_id(), ue_vars, killed_vars, undef_vars);
        _node->set_ue_var(ue_vars);
        _node->set_killed_var(killed_vars);
        _node->set_undefined_behaviour_var(undef_vars);
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
                                 ObjectList<var_usage_t*> glob_vars, ObjectList<Symbol> parameters, 
                                 std::map<Symbol, Nodecl::NodeclBase> params_to_args)
        : _cfg(cfg), _cfgs(cfgs), _global_vars(glob_vars), _params(parameters), _usage(), 
          _defining(false), _last_nodecl(Nodecl::NodeclBase::null()),
          _params_to_args(params_to_args), _visited_functions()
    {
        Symbol s = cfg->get_function_symbol();
        if (s.is_valid())
        {
            _visited_functions.insert(s);
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

    void CfgIPAVisitor::compute_usage()
    {
        Node* graph_node = _cfg->get_graph();
        ExtensibleGraph::clear_visits(graph_node);
        
        compute_usage_rec(graph_node);
       
        ExtensibleGraph::clear_visits(graph_node);
    }
    
    ObjectList<struct var_usage_t*> CfgIPAVisitor::get_usage() const
    {
        return _usage;
    }
    
    void CfgIPAVisitor::set_up_usage(Nodecl::NodeclBase n)
    {
        if (usage_list_contains_nodecl(n, _usage))
        {   // Some use in the nodecl was already registered. We upload it if necessary
            struct var_usage_t* ipa_var = get_var_in_list(n, _usage);
            char usage = ipa_var->get_usage();
            if (usage == ' ')
            {
                if (_defining)
                    ipa_var->set_usage('0');
                else
                    ipa_var->set_usage('1');
            }
            if (usage == '1')
            {
                if (_defining)  // Set to '2' (=> UE + KILLED) the usage value
                    ipa_var->set_usage('2');
                else 
                {}  // Nothing to do, the variable was already used
            }
            else
            {}  // nothing to do: It doesn't matters what happens with an already Killed or Undefined behaviour variable
        }
        else
        {   // Is the first time we use this nodecl
            if (!usage_list_contains_englobing_nodecl(n, _usage))
            {
                if (usage_list_contains_englobed_nodecl(n, _usage))
                {   // delete the englobed part
                    delete_englobed_var_in_usage_list(n, _usage);
                }
                char usage = '1';
                if (_defining) usage = '0';
                struct var_usage_t* new_ipa_var = new var_usage_t(ExtendedSymbol(n), usage);
                _usage.insert(new_ipa_var);
            }
        }
    }
    
    void CfgIPAVisitor::set_up_undefined_usage(Nodecl::NodeclBase arg)
    {
        if (usage_list_contains_nodecl(arg, _usage))
        {
            struct var_usage_t* ipa_var = get_var_in_list(arg, _usage);
            char ipa_usage = ipa_var->get_usage();
            if (ipa_usage == '1')
                ipa_var->set_usage('3');
            else {} // If it was killed or undefined, we don't care now
        }
        else
        {
            struct var_usage_t* new_ipa_var = new var_usage_t(ExtendedSymbol(arg), '3');
            _usage.insert(new_ipa_var);
        }
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Symbol& n)
    {
        Symbol s = n.get_symbol();
        if (usage_list_contains_sym(s, _global_vars))
        {   // Usage of a global variable
            Nodecl::NodeclBase real_n = n;
            if (!_last_nodecl.is_null())
                real_n = _last_nodecl;
            
            // Set up the usage
            set_up_usage(real_n);
        }
        else if (_params.contains(s))
        {   // Usage of a parameter: We need to transform the usage of the parameter into an usage of the corresponding argument
            // Get the corresponding argument
            Nodecl::NodeclBase arg = _params_to_args[s];
           
            if (!_last_nodecl.is_null())
            {
                // Translate the parameter usage into an argument usage
                if (_last_nodecl.is<Nodecl::Reference>())
                {
                    Nodecl::Reference aux = Nodecl::Reference::make(arg, arg.get_type(), arg.get_filename(), arg.get_line());
                    arg = aux;
                }
                else if (_last_nodecl.is<Nodecl::Dereference>())
                {
                    Nodecl::Dereference aux = Nodecl::Dereference::make(arg, arg.get_type(), arg.get_filename(), arg.get_line());
                    arg = aux;                    
                }
                else
                {
                    internal_error("Usage of node type '%s' not yet implemented", ast_print_node_type(_last_nodecl.get_kind()));
                }
                
                // Set the usage into the list
                set_up_usage(arg);
            }
            else
            {
                if (arg.is<Nodecl::Symbol>())
                {   // Set the usage into the list
                    set_up_usage(arg);
                }
                else
                {   // The symbols in the argument can only be used
                    if (!_defining)
                    {
                        ExtendedSymbolVisitor sv;
                        sv.walk(arg);
                        ObjectList<Nodecl::NodeclBase> arg_syms = sv.get_extensible_symbols();
                        for (ObjectList<Nodecl::NodeclBase>::iterator it = arg_syms.begin(); it != arg_syms.end(); ++it)
                            set_up_usage(*it);
                    }
                }
            }
        }
        else
        {}  // The symbol is a local variable

        _last_nodecl = Nodecl::NodeclBase::null();
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
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::MinusAssignment& n)
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
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::BitwiseShrAssignment& n)
    {
        op_assignment_visit(n);
    }

    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::ArithmeticShrAssignment& n)
    {
        op_assignment_visit(n);
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::BitwiseShlAssignment& n)
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
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Reference& n)
    {
        _last_nodecl = n;
        walk(n.get_rhs());
    }
    
    CfgIPAVisitor::Ret CfgIPAVisitor::visit(const Nodecl::Dereference& n)
    {
        _last_nodecl = n;
        walk(n.get_rhs());
    }
    
    static void print_params_to_args(std::map<Symbol, Nodecl::NodeclBase> params_to_args)
    {
        std::cerr << "Map between parameters and arguments: " << std::endl;
        for (std::map<Symbol, Nodecl::NodeclBase>::iterator it = params_to_args.begin(); it != params_to_args.end(); ++it)
        {
            std::cerr << "  - " << it->first.get_name() << "  ->  " << it->second.prettyprint() << std::endl;
        }
    }
    
    template <typename T>
    void CfgIPAVisitor::function_visit(const T& n)
    {
        // Compute IPA for the current function call and propagate the info to the current node
        Nodecl::FunctionCall call = n.template as<Nodecl::FunctionCall>();
        
        ExtensibleGraph* called_func_graph = find_function_for_ipa(n.get_called().get_symbol(), _cfgs);
        if (called_func_graph != NULL)
        {
            Symbol s = called_func_graph->get_function_symbol();
            if (s.is_valid())
            {
                if (!_visited_functions.contains(s))
                {
                    _visited_functions.insert(s);
                    ObjectList<struct var_usage_t*> nested_global_vars = called_func_graph->get_global_variables();
                    ObjectList<Symbol> nested_params = called_func_graph->get_function_parameters();
                    if (!nested_global_vars.empty() || !nested_params.empty())
                    {
                        // Compute liveness for global variables and parameters
                        std::map<Symbol, Nodecl::NodeclBase> nested_params_to_args = map_reference_params_to_args(n, called_func_graph);
                        CfgIPAVisitor ipa_visitor(called_func_graph, _cfgs, nested_global_vars, nested_params, nested_params_to_args);
                        ipa_visitor.compute_usage();
                        
                        // Propagate this information to the current graph analysis
                        ObjectList<struct var_usage_t*> nested_usage = ipa_visitor.get_usage();
                        for (ObjectList<struct var_usage_t*>::iterator it = nested_usage.begin(); it != nested_usage.end(); ++it)
                        {
                            Nodecl::NodeclBase current_var = (*it)->get_nodecl();
                            if (usage_list_contains_nodecl(current_var, _usage))
                            {   // The variable was already used in the current graph
                                struct var_usage_t* ipa_var_usage = get_var_in_list(current_var, _usage);
                                char ipa_usage = ipa_var_usage->get_usage();
                                if (ipa_usage == '1')
                                {
                                    if ((*it)->get_usage() == '0' || (*it)->get_usage() == '2')
                                        ipa_var_usage->set_usage('2');
                                }
                                else {} // If it was already killed or undefined, we don't care now
                            }
                            else
                            {   // Is the first use of the variable in the current graph
                                struct var_usage_t* new_ipa_var = *it;
                                _usage.insert(new_ipa_var);
                            }
                        }
                    }
                }
                else
                {}  // Nothing to do, the analysis of this function is already included in the actual usage computation
            }
        }
        else
        {   // All global variables and parameters in the current function call that comes from parameters in the previous call (current _cfg)
            // have an undefined behaviour from this point.
            // If a variable was already killed, we don't care what happens from now on
            
            // Global variables
            for(ObjectList<struct var_usage_t*>::iterator it = _global_vars.begin(); it != _global_vars.end(); ++it)
            {
                Nodecl::NodeclBase current_var = (*it)->get_nodecl();
                set_up_undefined_usage(current_var);
            }
            
            // Reference parameters of the current function used in the current function call
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
           
            for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
            {
                if ( (it->is<Nodecl::Symbol>() || it->is<Nodecl::ClassMemberAccess>() || it->is<Nodecl::ArraySubscript>()
                    || it->is<Nodecl::Reference>() || it->is<Nodecl::Dereference>() ) && !it->is_constant())
                {   // Since they can be passed by reference, they can be modified
                    set_up_undefined_usage(*it);
                }
                else if (it->is<Nodecl::FunctionCall>() || it->is<Nodecl::VirtualFunctionCall>())
                {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                else
                {   // Expressions cannot be passed by reference, so this arguments are only used
                    // FIXME We can define a variable here passing as argument "(n = 3)"
                    ExtendedSymbolVisitor sv;
                    sv.walk(*it);
                    ObjectList<Nodecl::NodeclBase> syms_in_arg = sv.get_extensible_symbols();
                    for (ObjectList<Nodecl::NodeclBase>::iterator it = syms_in_arg.begin(); it != syms_in_arg.end(); ++it)
                    {   // The variable is used
                        set_up_usage(*it);
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
    
    // *** Symbols Visitor *** //
    
    ExtendedSymbolVisitor::ExtendedSymbolVisitor()
        : _symbols()
    {}
            
    ExtendedSymbolVisitor::Ret ExtendedSymbolVisitor::visit(const Nodecl::Symbol& n)
    {
        _symbols.insert(n);
    }
    
    ExtendedSymbolVisitor::Ret ExtendedSymbolVisitor::visit(const Nodecl::Reference& n)
    {
        // FIXME We should see here if we have some symbol inside the expression
        _symbols.insert(n);
    }
    
    ExtendedSymbolVisitor::Ret ExtendedSymbolVisitor::visit(const Nodecl::Dereference& n)
    {
        // FIXME We should see here if we have some symbol inside the expression
        _symbols.insert(n);
    }
    
    ObjectList<Nodecl::NodeclBase> ExtendedSymbolVisitor::get_extensible_symbols()
    {
        return _symbols;
    }
}
}
