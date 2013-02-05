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

#include <climits>

#include "tl-analysis-static-info.hpp"
#include "tl-vectorizer-visitor-for.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorFor::VectorizerVisitorFor(
                const std::string device,
                const unsigned int vector_length,
                const TL::Type& target_type) :
            _device(device), _vector_length(vector_length), _target_type(target_type)
        {
        }

        Nodecl::NodeclBase VectorizerVisitorFor::visit(const Nodecl::ForStatement& for_statement)
        {
            Nodecl::ForStatement epilog;

            // Get analysis info
            Analysis::AnalysisStaticInfo for_analysis_info(
                    Nodecl::Utils::get_enclosing_function(for_statement).get_function_code()
                        .as<Nodecl::FunctionCode>(),
                    Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS |
                    Analysis::WhichAnalysis::CONSTANTS_ANALYSIS ,
                    Analysis::WhereAnalysis::NESTED_FOR_STATIC_INFO, /* nesting level */ 1);

            // TODO: ???
            analyze_loop(for_statement);


            if (_remain_iterations)
            {
                // Save original ForStatement as Epilog
                epilog = get_epilog(for_statement);
            }

            // Vectorizing Loop Header
            VectorizerVisitorLoopHeader visitor_loop_header(for_statement, for_analysis_info, _unroll_factor);
            visitor_loop_header.walk(for_statement.get_loop_header());

            // Loop Body Vectorization
            VectorizerVisitorStatement visitor_stmt(_device,
                    _vector_length,
                    _unroll_factor,
                    _target_type,
                    for_statement.get_statement().retrieve_context(),
                    for_statement,
                    for_analysis_info);
            visitor_stmt.walk(for_statement.get_statement());

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
            _unroll_factor = 4;
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
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopHeader::VectorizerVisitorLoopHeader(
                const Nodecl::ForStatement& for_statement,
                const Analysis::AnalysisStaticInfo& for_analysis_info,
                const unsigned int unroll_factor) :
            _for_statement(for_statement), _for_analysis_info(for_analysis_info), _unroll_factor(unroll_factor)
        {
        }

        void VectorizerVisitorLoopHeader::visit(const Nodecl::LoopControl& loop_control)
        {
            // Init
            VectorizerVisitorLoopInit visitor_loop_init(_for_statement, _for_analysis_info);
            visitor_loop_init.walk(loop_control.get_init());

            // Cond
            VectorizerVisitorLoopCond visitor_loop_cond(_for_statement, _for_analysis_info, _unroll_factor);
            visitor_loop_cond.walk(loop_control.get_cond());

            // Next
            VectorizerVisitorLoopNext visitor_loop_next(_for_statement, _for_analysis_info, _unroll_factor);
            visitor_loop_next.walk(loop_control.get_next());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopHeader::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Header Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopInit::VectorizerVisitorLoopInit(
                const Nodecl::ForStatement& for_statement,
                const Analysis::AnalysisStaticInfo& for_analysis_info) :
            _for_statement(for_statement), _for_analysis_info(for_analysis_info)
        {
        }

        void VectorizerVisitorLoopInit::visit(const Nodecl::ObjectInit& node)
        {
            running_error("Vectorizer (%s): Declaration of new variables is not supported yet "\
                          "in LoopControl. Please, declare them outside of the loop.",
                          node.get_locus().c_str());
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
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopCond::VectorizerVisitorLoopCond(
                const Nodecl::ForStatement& for_statement,
                const Analysis::AnalysisStaticInfo& for_analysis_info,
                const unsigned int unroll_factor) :
            _for_statement(for_statement), _for_analysis_info(for_analysis_info),
            _unroll_factor(unroll_factor)
        {
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::Equal& node)
        {
            visit_condition(node.as<Nodecl::NodeclBase>());
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::LowerThan& node)
        {
            visit_condition(node.as<Nodecl::NodeclBase>());
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::LowerOrEqualThan& node)
        {
            visit_condition(node.as<Nodecl::NodeclBase>());
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::GreaterThan& node)
        {
            visit_condition(node.as<Nodecl::NodeclBase>());
        }

        void VectorizerVisitorLoopCond::visit(const Nodecl::GreaterOrEqualThan& node)
        {
            visit_condition(node.as<Nodecl::NodeclBase>());
        }

        void VectorizerVisitorLoopCond::visit_condition(const Nodecl::NodeclBase& node)
        {
            Nodecl::Equal condition = node.as<Nodecl::Equal>();
            Nodecl::NodeclBase lhs = condition.get_lhs();
            Nodecl::NodeclBase rhs = condition.get_rhs();

            bool lhs_const_flag = _for_analysis_info.is_constant(_for_statement, lhs);
            bool rhs_const_flag = _for_analysis_info.is_constant(_for_statement, rhs);

            if (!lhs_const_flag && rhs_const_flag)
            {
                TL::Type rhs_type = rhs.get_type();

                // Step
                // TODO: Not only the trivial case
                Nodecl::NodeclBase step;
                Nodecl::ParenthesizedExpression new_step;

                if (_for_analysis_info.is_induction_variable(_for_statement, lhs))
                {
                    step = const_value_to_nodecl(
                        _for_analysis_info.get_induction_variable_increment(_for_statement, lhs));

                    new_step = Nodecl::ParenthesizedExpression::make(
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    step,
                                    step.get_type(),
                                    node.get_filename(),
                                    node.get_line()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_unsigned_int_type(),
                                    const_value_get_unsigned_int(_unroll_factor),
                                    node.get_filename(),
                                    node.get_line()),
                                step.get_type(),
                                node.get_filename(),
                                node.get_line()),
                            step.get_type(),
                            node.get_filename(),
                            node.get_line());
                }
                else
                {
                    running_error("Vectorizer (%s): Induction variable cannot be found in LoopCondition.",
                            node.get_locus().c_str());
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
                                            node.get_filename(),
                                            node.get_line()),
                                        rhs_type,
                                        node.get_filename(),
                                        node.get_line()),
                                    rhs_type,
                                    node.get_filename(),
                                    node.get_line()),
                                rhs_type,
                                node.get_filename(),
                                node.get_line()),
                            rhs_type,
                            node.get_filename(),
                            node.get_line());

                rhs.replace(new_node);
            }
            else if (lhs_const_flag && !rhs_const_flag)
            {
                TL::Type lhs_type = lhs.get_type();

                // Step
                // TODO: Not only the trivial case
                Nodecl::NodeclBase step;
                Nodecl::ParenthesizedExpression new_step;

                if (_for_analysis_info.is_induction_variable(_for_statement, rhs))
                {
                    step = const_value_to_nodecl(
                        _for_analysis_info.get_induction_variable_increment(_for_statement, rhs));

                    new_step = Nodecl::ParenthesizedExpression::make(
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    step,
                                    step.get_type(),
                                    node.get_filename(),
                                    node.get_line()),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_unsigned_int_type(),
                                    const_value_get_unsigned_int(_unroll_factor),
                                    node.get_filename(),
                                    node.get_line()),
                                step.get_type(),
                                node.get_filename(),
                                node.get_line()),
                            step.get_type(),
                            node.get_filename(),
                            node.get_line());
                }
                else
                {
                    running_error("Vectorizer (%s): Induction variable cannot be found in LoopCondition.",
                            node.get_locus().c_str());
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
                                            node.get_filename(),
                                            node.get_line()),
                                        lhs_type,
                                        node.get_filename(),
                                        node.get_line()),
                                    lhs_type,
                                    node.get_filename(),
                                    node.get_line()),
                                lhs_type,
                                node.get_filename(),
                                node.get_line()),
                            lhs_type,
                            node.get_filename(),
                            node.get_line());

                lhs.replace(new_node);
            }
            else if (lhs_const_flag && rhs_const_flag)
            {
                running_error("Vectorizer (%s): The loop is not vectorizable because of the loop "
                        "condition. Both expressions are constant.", node.get_locus().c_str());
            }
            else
            {
                running_error("Vectorizer (%s): The loop is not vectorizable because of the loop "
                        "condition. Both expressions are not constant.", node.get_locus().c_str());
            }
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopCond::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Cond Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }

        VectorizerVisitorLoopNext::VectorizerVisitorLoopNext(
                const Nodecl::ForStatement& for_statement,
                const Analysis::AnalysisStaticInfo& for_analysis_info,
                const unsigned int unroll_factor) :
            _for_statement(for_statement), _for_analysis_info(for_analysis_info),
            _unroll_factor(unroll_factor)
        {
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Preincrement& node)
        {
            const Nodecl::NodeclBase rhs = node.get_rhs();

            if (_for_analysis_info.is_induction_variable(_for_statement, rhs))
            {
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            rhs.shallow_copy(),
                            Nodecl::IntegerLiteral::make(
                                node.get_type(),
                                _for_analysis_info.get_induction_variable_increment(_for_statement, rhs),
                                node.get_filename(),
                                node.get_line()),
                            node.get_type(),
                            node.get_filename(),
                            node.get_line());

                node.replace(new_node);
            }
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Postincrement& node)
        {
            const Nodecl::NodeclBase rhs = node.get_rhs();
            if (_for_analysis_info.is_induction_variable(_for_statement, rhs))
            {
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            rhs.shallow_copy(),
                            Nodecl::Mul::make(
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_unsigned_int_type(),
                                    const_value_get_unsigned_int(_unroll_factor),
                                    node.get_filename(),
                                    node.get_line()),
                                Nodecl::IntegerLiteral::make(
                                    node.get_type(),
                                    _for_analysis_info.get_induction_variable_increment(_for_statement, rhs),
                                    node.get_filename(),
                                    node.get_line()),
                                node.get_type(),
                                node.get_filename(),
                                node.get_line()),
                            node.get_type(),
                            node.get_filename(),
                            node.get_line());

                node.replace(new_node);
            }
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::AddAssignment& node)
        {
            const Nodecl::NodeclBase lhs = node.get_lhs();

            if (_for_analysis_info.is_induction_variable(_for_statement, lhs))
            {
                const Nodecl::AddAssignment new_node =
                    Nodecl::AddAssignment::make(
                            lhs,
                            Nodecl::Add::make(
                                Nodecl::ParenthesizedExpression::make(
                                    node.get_rhs(),
                                    node.get_type(),
                                    node.get_filename(),
                                    node.get_line()),
                                Nodecl::IntegerLiteral::make(
                                    node.get_type(),
                                    _for_analysis_info.get_induction_variable_increment(_for_statement, lhs),
                                    node.get_filename(),
                                    node.get_line()),
                                node.get_type(),
                                node.get_filename(),
                                node.get_line()),
                            node.get_type(),
                            node.get_filename(),
                            node.get_line());

                node.replace(new_node);
            }
        }

        void VectorizerVisitorLoopNext::visit(const Nodecl::Comma& node)
        {
            walk(node.get_lhs());
            walk(node.get_rhs());
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopNext::unhandled_node(const Nodecl::NodeclBase& n)
        {
            std::cerr << "Loop Next Visitor: Unknown node "
                << ast_print_node_type(n.get_kind())
                << " at " << n.get_locus()
                << std::endl;

            return Ret();
        }
    }
}
