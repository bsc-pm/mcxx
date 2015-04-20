/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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




#ifndef TL_EXPRESSION_REDUCTION_HPP
#define TL_EXPRESSION_REDUCTION_HPP

// #include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-calc.hpp"

namespace TL {
namespace Optimizations {
    
    /*!
    * This method must be called in pre-order from the bottom of a tree expression
    *
    * R1 :   +                                     R3 :    -
    *      /   \          =>     c1 + c2                 /   \     =>    c1 - c2
    *    c1    c2                                      c1    c2
    *
    * R2 :   +                       +             R4:     -                +
    *      /   \          =>       /   \                 /   \     =>     /   \
    *     t    c                  c     t               t    c          -c     t
    *
    * R5 :   -
    *      /   \               =>    0
    *     t1   t2 , t1 = t2
    *
    * R6a :      +                    +            R6b :   +                 +
    *          /   \               /     \               /   \            /     \
    *         +    c2     =>    c1+c2     t            c1    +     =>  c1+c2     t
    *       /   \                                          /  \
    *     c1    t                                        c2   t
    *
    * R6c :      +                    -            R6d :   +                 -
    *          /   \               /     \               /   \            /     \
    *         -    c2     =>    c1+c2     t            c1    -     =>  c1+c2     t
    *       /   \                                          /  \
    *     c1    t                                        c2   t
    *
    * R6e :      -                    +            R6f :   -                 +
    *          /   \               /     \               /   \            /     \
    *         +    c2     =>    c1-c2     t            c1    +     =>  c1-c2     t
    *       /   \                                          /  \
    *     c1    t                                        c2   t
    *
    * R6g :      -                    -            R6h :   -                 -
    *          /   \               /     \               /   \            /     \
    *         -    c2     =>    c1-c2     t            c1    -     =>  c1-c2     t
    *       /   \                                          /  \
    *     c1    t                                        c2   t
    *
    * R7 :   *                                     R8 :    *                 *
    *      /   \          =>     c1 * c2                 /   \     =>      /   \
    *    c1    c2                                       t    c            c     t
    *
    * R9 :       *                    *
    *          /   \               /     \
    *         *    c2     =>    c1*c2     t
    *       /   \
    *     c1    t
    *
    * R10 :  /
    *      /   \          =>     c1/c2
    *     c1   c2,  c2 != 0
    *
    * R11a : %         %                            R11b  %
    *      /   \  ,  /   \   =>  0                      /   \       =>  1
    *     t    1    t    t                             1    t
    * 
    * 
    * R20a : <                                      R20b:  <
    *      /   \                 <                       /    \             <
    *     +    c2         =>   /    \                   c1     +    =>    /   \
    *   /   \               c1-c2   -t                       /   \     c1-c2  t
    *  c1   t                                               c2   t
    * 
    * R20c : <=                                     R20d:  <=
    *      /    \                <=                      /    \            <=
    *     +     c2        =>   /    \                   c1     +    =>   /    \
    *   /   \               c1-c2   -t                       /   \    c1-c2   t
    *  c1   t                                               c2   t
    * 
    * R21 : <|<=                <|<= 
    *      /    \               /   \ 
    *     +     +         => c1-c2  -
    *   /  \   /  \                / \
    *  c1  t1 c2  t2              t2 t1
    * 
    * R30a :  -                      R30b:   -
    *      /    \                          /   \
    *     +     +         => t3           +     +    => t3
    *   /  \   /  \                      / \   /  \ 
    *  t1  t2 +   t3                    +  t3 t1  t2
    *        / \                       / \
    *       t1  t2                    t1 t2
    * R30c :  -                      R30d:   -
    *      /    \                          /   \
    *     +     +         => t3 - t4      +     +    => t3 - t4
    *   /  \   /  \                      / \   /  \ 
    *  +   t3 +   t4                    +  t3 +   t4
    * / \    / \                       / \   / \
    *t1 t2 t1  t2                    t1 t2  t1 t2
    *
    * R50a : +                  +
    *      /   \              /   \
    *     +     t2        => c     + 
    *   /  \                      / \    
    *  c   t1                    t1  t2
    *
    * R50b : -                  -
    *      /   \              /   \
    *     -     t2        => c     + 
    *   /  \                      / \    
    *  c   t1                    t1  t2
    *
    * R50c : *                  *
    *      /   \              /   \
    *     *     t2        => c     * 
    *   /  \                      / \    
    *  c   t1                    t1  t2
    *
    * R51b : +                  -
    *      /   \              /   \
    *     -     t2        => c     - 
    *   /  \                      / \    
    *  c   t1                    t1  t2
    *
    * R51b : -                  +
    *      /   \              /   \
    *     +     t2        => c     - 
    *   /  \                      / \    
    *  c   t1                    t1  t2
    *
    */
    class LIBTL_CLASS ReduceExpressionVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Calculator _calc;

    public:
        // *** Constructor *** //
        ReduceExpressionVisitor( );

        // *** Visiting methods *** //
        Ret visit_post( const Nodecl::Add& n );
        Ret visit_post( const Nodecl::BitwiseAnd& n );
        Ret visit_post( const Nodecl::BitwiseOr& n );
        Ret visit_post( const Nodecl::BitwiseShl& n );
        Ret visit_post( const Nodecl::BitwiseShr& n );
        Ret visit_post( const Nodecl::Div& n );
        Ret visit_post( const Nodecl::LowerOrEqualThan& n );
        Ret visit_post( const Nodecl::LowerThan& n );
        Ret visit_post( const Nodecl::Minus& n );
        Ret visit_post( const Nodecl::Mod& n );
        Ret visit_post( const Nodecl::Mul& n );
        Ret visit_post( const Nodecl::ObjectInit& n );
        Ret visit_post( const Nodecl::VectorAdd& n );
        Ret visit_post( const Nodecl::VectorBitwiseAnd& n );
        Ret visit_post( const Nodecl::VectorBitwiseOr& n );
        Ret visit_post( const Nodecl::VectorBitwiseShl& n );
        Ret visit_post( const Nodecl::VectorBitwiseShr& n );
        Ret visit_post( const Nodecl::VectorDiv& n );
        Ret visit_post( const Nodecl::VectorLowerOrEqualThan& n );
        Ret visit_post( const Nodecl::VectorLowerThan& n );
        Ret visit_post( const Nodecl::VectorMinus& n );
        Ret visit_post( const Nodecl::VectorMod& n );
        Ret visit_post( const Nodecl::VectorMul& n );
    };

   
    class UnitaryReductor : public Nodecl::NodeclVisitor<void>
    {
        private:
            TL::ObjectList<Nodecl::NodeclBase> _unitary_rhss;

            void print_unitary_rhss();
            bool is_leaf_node(const Nodecl::NodeclBase& n);
            void nullify_nodecl(const Nodecl::NodeclBase& n);

        public:
            UnitaryReductor();

            void reduce(const Nodecl::Minus& n);

            Ret unhandled_node(const Nodecl::NodeclBase& n);
            Ret visit(const Nodecl::Conversion& n);
            Ret visit(const Nodecl::Add& n);
            Ret visit(const Nodecl::Neg& n);
            Ret visit(const Nodecl::Symbol& n);
            Ret visit(const Nodecl::IntegerLiteral& n);
            Ret visit(const Nodecl::Mul& n);
            Ret visit(const Nodecl::BitwiseShl& n);
            Ret visit(const Nodecl::Div& n);
            Ret visit(const Nodecl::BitwiseShr& n);
            Ret visit(const Nodecl::Mod& n);
    };

    class UnitaryDecomposer : public Nodecl::NodeclVisitor<TL::ObjectList<Nodecl::NodeclBase> >
    {
        public:
            UnitaryDecomposer();

            Ret unhandled_node(const Nodecl::NodeclBase& n);
            Ret visit(const Nodecl::Conversion& n);
            Ret visit(const Nodecl::Symbol& n);
            Ret visit(const Nodecl::Add& n);
            Ret visit(const Nodecl::Neg& n);
            Ret visit(const Nodecl::Mul& n);
            Ret visit(const Nodecl::BitwiseShl& n);
            Ret visit(const Nodecl::Div& n);
            Ret visit(const Nodecl::BitwiseShr& n);
            Ret visit(const Nodecl::Mod& n);
            Ret visit(const Nodecl::IntegerLiteral& n);
    };

    class MinusRemover : public Nodecl::ExhaustiveVisitor<void>
    {
        public:
            MinusRemover();

            Ret visit(const Nodecl::Minus& n);
            Ret visit(const Nodecl::Neg& n);
    };
}
}

#endif // TL_EXPRESSION_REDUCTION_HPP
