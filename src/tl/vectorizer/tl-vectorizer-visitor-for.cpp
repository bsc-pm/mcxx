/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-vectorizer.hpp"
#include "tl-vectorizer-visitor-for.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-vectorizer-visitor-expression.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorFor::VectorizerVisitorFor(VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        bool VectorizerVisitorFor::join_list(ObjectList<bool>& list)
        {
            /*
            for(ObjectList<bool>::iterator it = list.begin( ); 
                    it != list.end( );  
                    ++it)
            {
                running_error("VectorizerVisitorFor: Error join_list");
            }
            */

            running_error("VectorizerVisitorFor: Error join_list");
        }

        bool VectorizerVisitorFor::visit(const Nodecl::ForStatement& for_statement)
        {
            bool needs_epilog = false;
    
            // Set up enviroment
            _environment._external_scope =
                for_statement.retrieve_context();
            _environment._local_scope_list.push_back(
                    for_statement.get_statement().as<Nodecl::List>().front().retrieve_context());

            // Get analysis info
            Nodecl::NodeclBase enclosing_func = 
                Nodecl::Utils::get_enclosing_function(for_statement).get_function_code();

            if ((Vectorizer::_analysis_info == 0) || 
                (Vectorizer::_analysis_info->get_nodecl_origin() != enclosing_func))
            {
                if (Vectorizer::_analysis_info != 0)
                    delete Vectorizer::_analysis_info;

                Vectorizer::_analysis_info = new Analysis::AnalysisStaticInfo(
                        enclosing_func.as<Nodecl::FunctionCode>(),
                        Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS |
                        Analysis::WhichAnalysis::CONSTANTS_ANALYSIS ,
                        Analysis::WhereAnalysis::NESTED_ALL_STATIC_INFO, /* nesting level */ 100);
            }

            // Push ForStatement as scope for analysis
            _environment._analysis_scopes.push_back(for_statement);

            // TODO: ???
            analyze_loop(for_statement);

            if (_remain_iterations)
            {
                needs_epilog = true;
            }

            // Vectorize Loop Header
            VectorizerVisitorLoopHeader visitor_loop_header(_environment);
            visitor_loop_header.walk(for_statement.get_loop_header().as<Nodecl::LoopControl>());

            // Vectorize Loop Body
            VectorizerVisitorStatement visitor_stmt(_environment);
            visitor_stmt.walk(for_statement.get_statement());

            _environment._analysis_scopes.pop_back();

            return needs_epilog;
        }

        void VectorizerVisitorFor::analyze_loop(const Nodecl::ForStatement& for_statement)
        {
            //TODO
            _remain_iterations = 2;
        }

        Nodecl::NodeclVisitor<bool>::Ret VectorizerVisitorFor::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "For Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus_str()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopHeader::VectorizerVisitorLoopHeader(const VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorLoopHeader::visit(const Nodecl::LoopControl& loop_control)
        {
            // Init
            VectorizerVisitorLoopInit visitor_loop_init;
            visitor_loop_init.walk(loop_control.get_init());

            // Cond
            VectorizerVisitorLoopCond visitor_loop_cond(_environment);
            visitor_loop_cond.walk(loop_control.get_cond());

            // Next
            VectorizerVisitorLoopNext visitor_loop_next(_environment);
            visitor_loop_next.walk(loop_control.get_next());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopHeader::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Header Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus_str()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopInit::VectorizerVisitorLoopInit(void)
        {
        }

        void VectorizerVisitorLoopInit::visit(const Nodecl::ObjectInit& node)
        {
            running_error("Vectorizer (%s): Declaration of new variables is not supported yet "\
                          "in LoopControl. Please, declare them outside of the loop.",
                          node.get_locus_str().c_str());
        }

        void VectorizerVisitorLoopInit::visit(const Nodecl::Assignment& node)
        {
            /*
            ERROR_CONDITION(!_for_analysis_info.is_induction_variable(_for_statement, node.get_lhs()),
                    "Vectorizer: Induction variable was expected in LoopControl initialization.", 0);
            */
        }

        void VectorizerVisitorLoopInit::visit(const Nodecl::Comma& node)
        {
            walk(node.get_lhs());
            walk(node.get_rhs());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopInit::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Init Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus_str()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopCond::VectorizerVisitorLoopCond(const VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::Equal& node)
        {
            visit_condition(node);
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::LowerThan& node)
        {
            visit_condition(node);
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::LowerOrEqualThan& node)
        {
            visit_condition(node);
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::GreaterThan& node)
        {
            visit_condition(node);
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::GreaterOrEqualThan& node)
        {
            visit_condition(node);
        }

        void VectorizerVisitorLoopCond::visit_condition(const Nodecl::NodeclBase& node)
        {
            Nodecl::Equal condition = node.as<Nodecl::Equal>();
            Nodecl::NodeclBase lhs = condition.get_lhs();
            Nodecl::NodeclBase rhs = condition.get_rhs();

            bool lhs_const_flag = Vectorizer::_analysis_info->is_constant(
                    _environment._analysis_scopes.back(), lhs);
            bool rhs_const_flag = Vectorizer::_analysis_info->is_constant(
                    _environment._analysis_scopes.back(), rhs);

            if (!lhs_const_flag && rhs_const_flag)
            {
                TL::Type rhs_type = rhs.get_type();

                // Step
                // TODO: Not only the trivial case
                Nodecl::NodeclBase step;
                Nodecl::ParenthesizedExpression new_step;

                if (Vectorizer::_analysis_info->is_induction_variable(
                            _environment._analysis_scopes.back(),
                            lhs))
                {
                    step = const_value_to_nodecl(
                        Vectorizer::_analysis_info->get_induction_variable_increment(
                            _environment._analysis_scopes.back(),
                            lhs));

                    new_step = Nodecl::ParenthesizedExpression::make(
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    step,
                                    step.get_type(),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_int_type(),
                                    const_value_get_signed_int(_environment._unroll_factor),
                                    node.get_locus()),
                                TL::Type::get_int_type(),
                                node.get_locus()),
                            step.get_type(),
                            node.get_locus());
                }
                else
                {
                    running_error("Vectorizer (%s): Induction variable cannot be found in LoopCondition.",
                            node.get_locus_str().c_str());
                }


                // rhs = (rhs-(step-1))
                const Nodecl::ParenthesizedExpression new_node =
                    Nodecl::ParenthesizedExpression::make(
                            Nodecl::Minus::make(
                                rhs.shallow_copy(),
                                Nodecl::ParenthesizedExpression::make(
                                    Nodecl::Minus::make(
                                        new_step,
                                        Nodecl::IntegerLiteral::make(
                                            TL::Type::get_int_type(),
                                            const_value_get_signed_int(1),
                                            node.get_locus()),
                                        rhs_type,
                                        node.get_locus()),
                                    rhs_type,
                                    node.get_locus()),
                                rhs_type,
                                node.get_locus()),
                            rhs_type,
                            node.get_locus());

                rhs.replace(new_node);
            }
            else if (lhs_const_flag && !rhs_const_flag)
            {
                TL::Type lhs_type = lhs.get_type();

                // Step
                // TODO: Not only the trivial case
                Nodecl::NodeclBase step;
                Nodecl::ParenthesizedExpression new_step;

                if (Vectorizer::_analysis_info->is_induction_variable(
                            _environment._analysis_scopes.back(),
                            rhs))
                {
                    step = const_value_to_nodecl(
                        Vectorizer::_analysis_info->get_induction_variable_increment(
                            _environment._analysis_scopes.back(),
                            rhs));

                    new_step = Nodecl::ParenthesizedExpression::make(
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    step,
                                    step.get_type(),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_int_type(),
                                    const_value_get_signed_int(_environment._unroll_factor),
                                    node.get_locus()),
                                step.get_type(),
                                node.get_locus()),
                            step.get_type(),
                            node.get_locus());
                }
                else
                {
                    running_error("Vectorizer (%s): Induction variable cannot be found in LoopCondition.",
                            node.get_locus_str().c_str());
                }


                // lhs = (lhs-(step-1))
                const Nodecl::ParenthesizedExpression new_node =
                    Nodecl::ParenthesizedExpression::make(
                            Nodecl::Minus::make(
                                lhs.shallow_copy(),
                                Nodecl::ParenthesizedExpression::make(
                                    Nodecl::Minus::make(
                                        new_step,
                                        Nodecl::IntegerLiteral::make(
                                            TL::Type::get_int_type(),
                                            const_value_get_signed_int(1),
                                            node.get_locus()),
                                        lhs_type,
                                        node.get_locus()),
                                    lhs_type,
                                    node.get_locus()),
                                lhs_type,
                                node.get_locus()),
                            lhs_type,
                            node.get_locus());

                lhs.replace(new_node);
            }
            else if (lhs_const_flag && rhs_const_flag)
            {
                running_error("Vectorizer (%s): Loop is not vectorizable because of the loop "
                        "condition. BOTH expressions are CONSTANT.", locus_to_str(node.get_locus()));
            }
            else
            {
                running_error("Vectorizer (%s): Loop is not vectorizable because of the loop "
                        "condition. BOTH expressions are NOT CONSTANT.", locus_to_str(node.get_locus()));
            }
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopCond::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Cond Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus_str()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopNext::VectorizerVisitorLoopNext(const VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Preincrement& node)
        {
            visit_increment(node, node.get_rhs());
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Postincrement& node)
        {
            visit_increment(node, node.get_rhs());
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::AddAssignment& node)
        {
            visit_increment(node, node.get_lhs());
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Comma& node)
        {
            walk(node.get_lhs());
            walk(node.get_rhs());
        }

        void VectorizerVisitorLoopNext::visit_increment(const Nodecl::NodeclBase& node, const Nodecl::NodeclBase& lhs)
        {
            if (Vectorizer::_analysis_info->is_induction_variable(
                        _environment._analysis_scopes.back(),
                        lhs))
            {
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            lhs.shallow_copy(),
                            const_value_to_nodecl(
                                const_value_mul(const_value_get_signed_int(_environment._unroll_factor), 
                                    Vectorizer::_analysis_info->get_induction_variable_increment(
                                        _environment._analysis_scopes.back(),
                                        lhs))),
                            node.get_type(),
                            node.get_locus());


/*
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            lhs.shallow_copy(),
                            Nodecl::Mul::make(
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_int_type(),
                                    const_value_get_signed_int(_environment._unroll_factor),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    node.get_type(),
                                    Vectorizer::_analysis_info->get_induction_variable_increment(
                                        _environment._analysis_scopes.back(),
                                        lhs),
                                    node.get_locus()),
                                node.get_type(),
                                node.get_locus()),
                            node.get_type(),
                            node.get_locus());
*/
                node.replace(new_node);
            }
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopNext::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Next Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus_str()
                << std::endl;

            return Ret();
        }


        VectorizerVisitorForEpilog::VectorizerVisitorForEpilog(VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        void VectorizerVisitorForEpilog::visit(const Nodecl::ForStatement& for_statement)
        {
            if(_environment._support_masking)
            {
                visit_vector_epilog(for_statement);
            }
            else
            {
                visit_scalar_epilog(for_statement);
            }
        }

        void VectorizerVisitorForEpilog::visit_vector_epilog(const Nodecl::ForStatement& for_statement)
        {
            // Get analysis info
            Nodecl::NodeclBase enclosing_func = 
                Nodecl::Utils::get_enclosing_function(for_statement).get_function_code();

            Nodecl::CompoundStatement comp_statement = for_statement.get_statement().as<Nodecl::List>().
                front().as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front().
                as<Nodecl::CompoundStatement>();

            // Get analysis info
/*            Nodecl::NodeclBase enclosing_func = 
                Nodecl::Utils::get_enclosing_function(for_statement).get_function_code();

            if ((Vectorizer::_analysis_info == 0) || 
                (Vectorizer::_analysis_info->get_nodecl_origin() != enclosing_func))
            {
                if (Vectorizer::_analysis_info != 0)
                    delete Vectorizer::_analysis_info;

                Vectorizer::_analysis_info = new Analysis::AnalysisStaticInfo(
                        enclosing_func.as<Nodecl::FunctionCode>(),
                        Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS |
                        Analysis::WhichAnalysis::CONSTANTS_ANALYSIS ,
                        Analysis::WhereAnalysis::NESTED_ALL_STATIC_INFO, 100);
            }
*/
            // Push ForStatement as scope for analysis
            _environment._analysis_scopes.push_back(for_statement);

            // Get mask for epilog instructions
            Nodecl::NodeclBase mask_value = for_statement.get_loop_header().
                as<Nodecl::LoopControl>().get_cond();
           
            VectorizerVisitorExpression visitor_mask(_environment);
            visitor_mask.walk(mask_value);
            
            TL::Symbol mask_sym = comp_statement.retrieve_context().
                new_symbol("__mask_" + Vectorizer::_vectorizer->get_var_counter());
            mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
            mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
            mask_sym.set_type(TL::Type::get_mask_type(_environment._unroll_factor));
            
            Nodecl::Symbol mask_nodecl_sym = mask_sym.make_nodecl(true, for_statement.get_locus());

            Nodecl::ExpressionStatement mask_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(mask_nodecl_sym, 
                            mask_value,
                            mask_sym.get_type(),
                            for_statement.get_locus()));

            comp_statement.as<Nodecl::CompoundStatement>().
                get_statements().as<Nodecl::List>().prepend(mask_exp); 

                // Vectorize Loop Body
            _environment._mask_list.push_back(mask_nodecl_sym);

            VectorizerVisitorStatement visitor_stmt(_environment);
            visitor_stmt.walk(comp_statement);

            _environment._mask_list.pop_back();

            // Remove loop header
            for_statement.replace(for_statement.get_statement());

            _environment._analysis_scopes.pop_back();
        }


        void VectorizerVisitorForEpilog::visit_scalar_epilog(const Nodecl::ForStatement& for_statement)
        {
            // Set up environment
            _environment._external_scope = for_statement.retrieve_context();
            _environment._local_scope_list.push_back(
                    for_statement.get_statement().as<Nodecl::List>().front().retrieve_context());

            Nodecl::LoopControl loop_control =
                for_statement.get_loop_header().as<Nodecl::LoopControl>();

            loop_control.set_init(Nodecl::NodeclBase::null());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorForEpilog::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "VectorizerVisitorForEpilog: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus_str()
                << std::endl;

            return Ret();
        }


    }
}
