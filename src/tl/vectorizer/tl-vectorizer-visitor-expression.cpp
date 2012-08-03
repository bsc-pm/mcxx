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

#include "tl-vectorizer-visitor-expression.hpp"

namespace TL 
{
    namespace Vectorizer
    {
        VectorizerVisitorExpression::VectorizerVisitorExpression(
                const unsigned int vector_length) : _vector_length(vector_length)
        { 
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Add& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorAdd vector_add = 
                Nodecl::VectorAdd::make(
                        n.get_lhs().shallow_copy(), 
                        n.get_rhs().shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_add);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Minus& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorMinus vector_minus = 
                Nodecl::VectorMinus::make(
                        n.get_lhs().shallow_copy(), 
                        n.get_rhs().shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_minus);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Mul& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorMul vector_mul = 
                Nodecl::VectorMul::make(
                        n.get_lhs().shallow_copy(), 
                        n.get_rhs().shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_mul);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Div& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorDiv vector_div = 
                Nodecl::VectorDiv::make(
                        n.get_lhs().shallow_copy(), 
                        n.get_rhs().shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_div);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Neg& n)
        {
            Nodecl::NodeclBase rhs = n.get_rhs();

            if (rhs.is<Nodecl::IntegerLiteral>() || // -1
                rhs.is<Nodecl::FloatingLiteral>())
            {
                const Nodecl::ConstantVectorPromotion vector_prom = 
                    Nodecl::ConstantVectorPromotion::make(
                            n.shallow_copy(), 
                            n.get_type().get_vector_to(_vector_length),
                            n.get_filename(), 
                            n.get_line());

                n.replace(vector_prom);
            }
            else // -a
            {
                walk(rhs);

                const Nodecl::VectorNeg vector_neg = 
                    Nodecl::VectorNeg::make(
                            n.get_rhs().shallow_copy(), 
                            n.get_type().get_vector_to(_vector_length),
                            n.get_filename(), 
                            n.get_line());

                n.replace(vector_neg);
            }
        }

        void VectorizerVisitorExpression::visit(const Nodecl::LowerThan& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::VectorLowerThan vector_lt = 
                Nodecl::VectorLowerThan::make(
                        n.get_lhs().shallow_copy(), 
                        n.get_rhs().shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_lt);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::ConditionalExpression& n)
        {
            walk(n.get_condition());
            walk(n.get_true());
            walk(n.get_false());

            const Nodecl::VectorConditionalExpression vector_cond = 
                Nodecl::VectorConditionalExpression::make(
                        n.get_condition().shallow_copy(), 
                        n.get_true().shallow_copy(), 
                        n.get_false().shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_cond);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Assignment& n)
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            walk(n.get_rhs());

            // Computing new vector type
            TL::Type vector_type = n.get_type();
            if (vector_type.is_lvalue_reference())
            {
                vector_type = vector_type.references_to();
            }
            vector_type = vector_type.get_vector_to(_vector_length);

            // Vector Store
            if(lhs.is<Nodecl::ArraySubscript>())
            {
                TL::Type basic_type = lhs.get_type();
                if (basic_type.is_lvalue_reference())
                {
                    basic_type = basic_type.references_to();
                }

                const Nodecl::VectorStore vector_store = 
                    Nodecl::VectorStore::make(
                        Nodecl::Reference::make(
                            Nodecl::ParenthesizedExpression::make(
                                lhs.shallow_copy(),
                                basic_type,
                                n.get_filename(), 
                                n.get_line()),
                            basic_type.get_pointer_to(),
                            n.get_filename(), 
                            n.get_line()),
                        n.get_rhs().shallow_copy(), 
                        vector_type,
                        n.get_filename(), 
                        n.get_line());

                n.replace(vector_store);
            }
            else // Register
            {
                const Nodecl::VectorAssignment vector_assignment = 
                    Nodecl::VectorAssignment::make(
                            lhs.shallow_copy(), 
                            n.get_rhs().shallow_copy(), 
                            vector_type.get_lvalue_reference_to(),
                            n.get_filename(), 
                            n.get_line());

                n.replace(vector_assignment);
            }

        }

        void VectorizerVisitorExpression::visit(const Nodecl::Conversion& n)
        {
            walk(n.get_nest());
        }

        void VectorizerVisitorExpression::visit(const Nodecl::ArraySubscript& n)
        {
            // Computing new vector type
            TL::Type vector_type = n.get_type();
            if (vector_type.is_lvalue_reference())
            {
                vector_type = vector_type.references_to();
            }
            vector_type = vector_type.get_vector_to(_vector_length);

            TL::Type basic_type = n.get_type();
            if (basic_type.is_lvalue_reference())
            {
                basic_type = basic_type.references_to();
            }

            const Nodecl::VectorLoad vector_load = 
                Nodecl::VectorLoad::make(
                        Nodecl::Reference::make(
                            Nodecl::ParenthesizedExpression::make(
                                n.shallow_copy(),
                                basic_type,
                                n.get_filename(), 
                                n.get_line()),
                            basic_type.get_pointer_to(),
                            n.get_filename(), 
                            n.get_line()),
                        vector_type,
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_load);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::FunctionCall& n)
        {
            //Nodecl::NodeclBase called = n.get_called();

            //ERROR_CONDITION(!n.is<Nodecl::Symbol>(), "SIMD-IR: This kind of function call is not supported yet", 0);

            // Vectorizing arguments
            walk(n.get_arguments());

            const Nodecl::VectorFunctionCall vector_function_call = 
                Nodecl::VectorFunctionCall::make(
                        n.get_called().shallow_copy(),
                        n.get_arguments().shallow_copy(),
                        n.get_alternate_name().shallow_copy(),
                        n.get_function_form().shallow_copy(),
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_function_call);            
        }

        void VectorizerVisitorExpression::visit(const Nodecl::Symbol& n)
        {
        }

        void VectorizerVisitorExpression::visit(const Nodecl::IntegerLiteral& n)
        {
            const Nodecl::ConstantVectorPromotion vector_prom = 
                Nodecl::ConstantVectorPromotion::make(
                        n.shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_prom);
        }

        void VectorizerVisitorExpression::visit(const Nodecl::FloatingLiteral& n)
        {
            const Nodecl::ConstantVectorPromotion vector_prom = 
                Nodecl::ConstantVectorPromotion::make(
                        n.shallow_copy(), 
                        n.get_type().get_vector_to(_vector_length),
                        n.get_filename(), 
                        n.get_line());

            n.replace(vector_prom);
        }


        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorExpression::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Unknown 'Expression' node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }
    } 
}
