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
#include "tl-nodecl-utils.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorFor::VectorizerVisitorFor(const VectorizerEnvironment& environment) :
            _environment(environment)
        {
        }

        Nodecl::NodeclBase VectorizerVisitorFor::visit(const Nodecl::ForStatement& for_statement)
        {
            Nodecl::ForStatement epilog;

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
                        Analysis::WhereAnalysis::NESTED_FOR_STATIC_INFO, /* nesting level */ 100);
            }

            // Push ForStatement as scope for analysis
            Vectorizer::_analysis_scopes = new std::list<Nodecl::NodeclBase>();
            Vectorizer::_analysis_scopes->push_back(for_statement);


            // TODO: ???
            analyze_loop(for_statement);


            if (_remain_iterations)
            {
                // Save original ForStatement as Epilog
                epilog = get_epilog(for_statement);
            }

            // Vectorizing Loop Header
            VectorizerVisitorLoopHeader visitor_loop_header(_environment);
            visitor_loop_header.walk(for_statement.get_loop_header());

            // Loop Body Vectorization
            TL::Scope inner_scope_of_for = for_statement.get_statement().retrieve_context();
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                inner_scope_of_for = for_statement.get_statement().as<Nodecl::List>().front().retrieve_context();
            }

            VectorizerVisitorStatement visitor_stmt(_environment);
            visitor_stmt.walk(for_statement.get_statement());

            Vectorizer::_analysis_scopes->pop_back();
            delete Vectorizer::_analysis_scopes;
            Vectorizer::_analysis_scopes = 0;

            if (_remain_iterations)
            {
                return epilog;
            }

            return Nodecl::NodeclBase::null();
        }

        void VectorizerVisitorFor::analyze_loop(const Nodecl::ForStatement& for_statement)
        {
            //TODO
            _remain_iterations = 2;
        }

        Nodecl::ForStatement VectorizerVisitorFor::get_epilog(const Nodecl::ForStatement& for_statement)
        {
            Nodecl::ForStatement epilog = Nodecl::Utils::deep_copy(
                    for_statement, for_statement).as<Nodecl::ForStatement>();

            Nodecl::LoopControl loop_control =
                epilog.get_loop_header().as<Nodecl::LoopControl>();

            loop_control.set_init(Nodecl::NodeclBase::null());

            return epilog;
        }


        Nodecl::NodeclVisitor<Nodecl::NodeclBase>::Ret VectorizerVisitorFor::unhandled_node(const Nodecl::NodeclBase& n)
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
                    Vectorizer::_analysis_scopes->back(), lhs);
            bool rhs_const_flag = Vectorizer::_analysis_info->is_constant(
                    Vectorizer::_analysis_scopes->back(), rhs);

            if (!lhs_const_flag && rhs_const_flag)
            {
                TL::Type rhs_type = rhs.get_type();

                // Step
                // TODO: Not only the trivial case
                Nodecl::NodeclBase step;
                Nodecl::ParenthesizedExpression new_step;

                if (Vectorizer::_analysis_info->is_induction_variable(
                            Vectorizer::_analysis_scopes->back(),
                            lhs))
                {
                    step = const_value_to_nodecl(
                        Vectorizer::_analysis_info->get_induction_variable_increment(
                            Vectorizer::_analysis_scopes->back(),
                            lhs));

                    new_step = Nodecl::ParenthesizedExpression::make(
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    step,
                                    step.get_type(),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_unsigned_int_type(),
                                    const_value_get_unsigned_int(_environment._unroll_factor),
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


                // rhs = (rhs-(step-1))
                const Nodecl::ParenthesizedExpression new_node =
                    Nodecl::ParenthesizedExpression::make(
                            Nodecl::Minus::make(
                                rhs.shallow_copy(),
                                Nodecl::ParenthesizedExpression::make(
                                    Nodecl::Minus::make(
                                        new_step,
                                        Nodecl::IntegerLiteral::make(
                                            TL::Type::get_unsigned_int_type(),
                                            const_value_get_unsigned_int(1),
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
                            Vectorizer::_analysis_scopes->back(),
                            rhs))
                {
                    step = const_value_to_nodecl(
                        Vectorizer::_analysis_info->get_induction_variable_increment(
                            Vectorizer::_analysis_scopes->back(),
                            rhs));

                    new_step = Nodecl::ParenthesizedExpression::make(
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    step,
                                    step.get_type(),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_unsigned_int_type(),
                                    const_value_get_unsigned_int(_environment._unroll_factor),
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
                                            TL::Type::get_unsigned_int_type(),
                                            const_value_get_unsigned_int(1),
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
            const Nodecl::NodeclBase lhs = node.get_lhs();

            if (Vectorizer::_analysis_info->is_induction_variable(
                        Vectorizer::_analysis_scopes->back(),
                        lhs))
            {
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            lhs,
                            Nodecl::Add::make(
                                Nodecl::ParenthesizedExpression::make(
                                    node.get_rhs(),
                                    node.get_type(),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    node.get_type(),
                                    Vectorizer::_analysis_info->get_induction_variable_increment(
                                        Vectorizer::_analysis_scopes->back(),
                                        lhs),
                                    node.get_locus()),
                                node.get_type(),
                                node.get_locus()),
                            node.get_type(),
                            node.get_locus());

                node.replace(new_node);
            }
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Comma& node)
        {
            walk(node.get_lhs());
            walk(node.get_rhs());
        }

        void VectorizerVisitorLoopNext::visit_increment(const Nodecl::NodeclBase& node, const Nodecl::NodeclBase& lhs)
        {
            if (Vectorizer::_analysis_info->is_induction_variable(
                        Vectorizer::_analysis_scopes->back(),
                        lhs))
            {
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            lhs.shallow_copy(),
                            Nodecl::Mul::make(
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_unsigned_int_type(),
                                    const_value_get_unsigned_int(_environment._unroll_factor),
                                    node.get_locus()),
                                Nodecl::IntegerLiteral::make(
                                    node.get_type(),
                                    Vectorizer::_analysis_info->get_induction_variable_increment(
                                        Vectorizer::_analysis_scopes->back(),
                                        lhs),
                                    node.get_locus()),
                                node.get_type(),
                                node.get_locus()),
                            node.get_type(),
                            node.get_locus());

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
    }
}
