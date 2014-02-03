


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
    *      /   \          =>     0
    *     0    c,  c != 0
    *
    * R11 :  %         %
    *      /   \  ,  /   \   =>  0
    *     t    1    t    t
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
        Ret visit_post( const Nodecl::Div& n );
        Ret visit_post( const Nodecl::LowerOrEqualThan& n );
        Ret visit_post( const Nodecl::LowerThan& n );
        Ret visit_post( const Nodecl::Minus& n );
        Ret visit_post( const Nodecl::Mod& n );
        Ret visit_post( const Nodecl::Mul& n );
        Ret visit_post( const Nodecl::ObjectInit& n );
        Ret visit_post( const Nodecl::VectorAdd& n );
        Ret visit_post( const Nodecl::VectorDiv& n );
        Ret visit_post( const Nodecl::VectorLowerOrEqualThan& n );
        Ret visit_post( const Nodecl::VectorLowerThan& n );
        Ret visit_post( const Nodecl::VectorMinus& n );
        Ret visit_post( const Nodecl::VectorMod& n );
        Ret visit_post( const Nodecl::VectorMul& n );
    };
    
}
}

#endif // TL_EXPRESSION_REDUCTION_HPP