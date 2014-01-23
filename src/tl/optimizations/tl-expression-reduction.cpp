
#include "tl-expression-reduction.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL {
namespace Optimizations {

    ReduceExpressionVisitor::ReduceExpressionVisitor( )
        : _calc( )
    {}

    void ReduceExpressionVisitor::visit_post( const Nodecl::Add& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs( );
        Nodecl::NodeclBase rhs = n.get_rhs( );
        if( lhs.is_constant( ) && const_value_is_zero( lhs.get_constant( ) ) )
        {   // 0 + t = t
            Nodecl::Utils::replace( n, rhs );
        }
        else if( rhs.is_constant( ) && const_value_is_zero( rhs.get_constant( ) ) )
        {   // t + 0 = t
            Nodecl::Utils::replace( n, lhs );
        }
        else if( lhs.is_constant( ) && rhs.is_constant( ) )
        {   // R1
            const_value_t* const_value = _calc.compute_const_value( n );
            Nodecl::NodeclBase new_n = const_value_to_nodecl(const_value);
            Nodecl::Utils::replace( n, new_n );
        }
        else if( rhs.is_constant( ) )
        {
            if( lhs.is<Nodecl::Add>( ) )
            {   // R6
                Nodecl::Add lhs_add = lhs.as<Nodecl::Add>( );
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs( );
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs( );
                if( lhs_lhs.is_constant( ) )
                {
                    Nodecl::NodeclBase c = Nodecl::Add::make( lhs_lhs, rhs, rhs.get_type( ) );
                    const_value_t* c_value = _calc.compute_const_value( c );
                    if( !const_value_is_zero( c_value ))
                    {
                        Nodecl::Utils::replace( n, Nodecl::Add::make( const_value_to_nodecl(c_value), lhs_rhs,
                                               rhs.get_type( ), n.get_locus( ) ) );
                    }
                    else
                    {
                        Nodecl::Utils::replace( n, lhs_rhs );
                    }
                }
            }
            else
            {   // R2
                Nodecl::Utils::replace( n, Nodecl::Add::make( rhs, lhs, lhs.get_type( ), n.get_locus( ) ) );
            }
        }
        else if( lhs.is_constant( ) )
        {
            if( rhs.is<Nodecl::Add>( ) )
            {   // R6
                Nodecl::Add rhs_add = rhs.as<Nodecl::Add>( );
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs( );
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs( );
                if( rhs_lhs.is_constant( ) )
                {
                    Nodecl::NodeclBase c = Nodecl::Add::make( lhs, rhs_lhs, lhs.get_type( ) );
                    const_value_t* c_value = _calc.compute_const_value( c );
                    if( !const_value_is_zero( c_value ))
                    {
                        Nodecl::Utils::replace( n, Nodecl::Add::make( const_value_to_nodecl(c_value), rhs_rhs,
                                               lhs.get_type( ), n.get_locus( ) ) );
                    }
                    else
                    {
                        Nodecl::Utils::replace( n, rhs_rhs );
                    }
                }
            }
            else
            {   // R2
                Nodecl::Utils::replace( n, Nodecl::Add::make( rhs, lhs, lhs.get_type( ), n.get_locus( ) ) );
            }
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::Div& n )
    {   // R10
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( lhs.is_constant( ) && rhs.is_constant( ) &&
            const_value_is_zero( lhs.get_constant( ) ) && !const_value_is_zero( rhs.get_constant( ) ) )
        {
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) );
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::LowerOrEqualThan& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( rhs.is_constant( ) )
        {
            if( lhs.is<Nodecl::Add>( ) )
            {   // R20
                Nodecl::Add lhs_add = lhs.as<Nodecl::Add>( );
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs( );
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs( );
                if( lhs_lhs.is_constant( ) )
                {
                    Nodecl::NodeclBase c = Nodecl::Minus::make( rhs, lhs_lhs, rhs.get_type( ) );
                    const_value_t* c_value = _calc.compute_const_value( c );
                    Nodecl::Utils::replace( n, Nodecl::LowerOrEqualThan::make( lhs_rhs, const_value_to_nodecl( c_value ),
                                rhs.get_type( ), n.get_locus( ) ) );
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::Minus& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( lhs.is_constant( ) && rhs.is_constant( ) )
        {   // R3
            const_value_t* c_value = _calc.compute_const_value( n );
            Nodecl::Utils::replace( n, const_value_to_nodecl( c_value ) );
        }
        else if( rhs.is_constant( ) )
        {   // R4
            if( const_value_is_zero( rhs.get_constant( ) ) )
            {
                Nodecl::Utils::replace( n, lhs );
            }
            else
            {
                Nodecl::NodeclBase neg_rhs = const_value_to_nodecl( const_value_neg( rhs.get_constant( ) ) );
                Nodecl::Utils::replace( n, Nodecl::Add::make( neg_rhs, lhs, lhs.get_type( ), n.get_locus( ) ) );
            }
        }
        else if( Nodecl::Utils::equal_nodecls( lhs, rhs ) )
        {
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) );
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::Mod& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( ( rhs.is_constant() && lhs.is_constant() && const_value_is_one( lhs.get_constant( ) ) )
            || Nodecl::Utils::equal_nodecls( lhs, rhs ) )
        {   // R11
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) );
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::Mul& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( ( lhs.is_constant( ) && const_value_is_zero( lhs.get_constant( ) ) )
            || ( rhs.is_constant( ) && const_value_is_zero( rhs.get_constant( ) ) ) )
        {   // 0 * t = t , t * 0 = t
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) );
        }
        else if( ( lhs.is_constant( ) && const_value_is_one( lhs.get_constant( ) ) ) )
        {   // 1 * t = t
            Nodecl::Utils::replace( n, rhs );
        }
        else if( ( rhs.is_constant( ) && const_value_is_one( rhs.get_constant( ) ) ) )
        {   // t * 1 = t
            Nodecl::Utils::replace( n, lhs );
        }
        else if( lhs.is_constant( ) && rhs.is_constant( ) )
        {   // R7
            const_value_t* c_value = _calc.compute_const_value( n );
            Nodecl::Utils::replace( n, const_value_to_nodecl( c_value ) );
        }
        else if ( rhs.is_constant( ) )
        {
            if( const_value_is_zero( rhs.get_constant( ) ) )
            {
                Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) );
            }
            else
            {
                if( lhs.is<Nodecl::Mul>( ) )
                {   // R9
                    Nodecl::Mul lhs_mul = lhs.as<Nodecl::Mul>( );
                    Nodecl::NodeclBase lhs_lhs = lhs_mul.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_mul.get_rhs();
                    if( lhs_lhs.is_constant( ) )
                    {
                        if( const_value_is_zero( lhs_lhs.get_constant( ) ) )
                        {
                            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) );
                        }
                        else
                        {
                            Nodecl::NodeclBase c = Nodecl::Mul::make( lhs_lhs, rhs, rhs.get_type() );
                            const_value_t* c_value = _calc.compute_const_value( c );
                            Nodecl::Utils::replace( n, Nodecl::Mul::make( const_value_to_nodecl( c_value ), lhs_rhs,
                                                   rhs.get_type( ), n.get_locus( ) ) );
                        }
                    }
                }
                else
                {   // R8
                    Nodecl::Utils::replace( n, Nodecl::Mul::make( rhs, lhs, lhs.get_type( ), n.get_locus( ) ) );
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::ObjectInit& n )
    {
        TL::Symbol sym = n.get_symbol();

        Nodecl::NodeclBase init = sym.get_value();
        if(!init.is_null())
        {
            walk(init);
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::VectorAdd& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs( );
        Nodecl::NodeclBase rhs = n.get_rhs( );
        if( lhs.is_constant( ) && const_value_is_zero( lhs.get_constant( ) ) )
        {   // 0 + t = t
            Nodecl::Utils::replace( n, rhs );
        }
        else if( rhs.is_constant( ) && const_value_is_zero( rhs.get_constant( ) ) )
        {   // t + 0 = t
            Nodecl::Utils::replace( n, lhs );
        }
        else if( lhs.is_constant( ) && rhs.is_constant( ) )
        {   // R1
            const_value_t* const_value = _calc.compute_const_value( n );
            Nodecl::NodeclBase new_n = const_value_to_nodecl(const_value);
            Nodecl::Utils::replace( n, new_n );
        }
        else if( rhs.is_constant( ) )
        {
            if( lhs.is<Nodecl::VectorAdd>( ) )
            {   // R6
                Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>( );
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs( );
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs( );
                if( lhs_lhs.is_constant( ) )
                {
                    Nodecl::NodeclBase c = Nodecl::VectorAdd::make( lhs_lhs, rhs, n.get_mask(), rhs.get_type( ) );
                    const_value_t* c_value = _calc.compute_const_value( c );
                    if( !const_value_is_zero( c_value ))
                    {
                        Nodecl::Utils::replace( n, Nodecl::VectorAdd::make( const_value_to_nodecl(c_value), lhs_rhs, n.get_mask(),
                                               rhs.get_type( ), n.get_locus( ) ) );
                    }
                    else
                    {
                        Nodecl::Utils::replace( n, lhs_rhs );
                    }
                }
            }
            else
            {   // R2
                Nodecl::Utils::replace( n, Nodecl::VectorAdd::make( rhs, lhs, n.get_mask(), lhs.get_type( ), n.get_locus( ) ) );
            }
        }
        else if( lhs.is_constant( ) )
        {
            if( rhs.is<Nodecl::VectorAdd>( ) )
            {   // R6
                Nodecl::VectorAdd rhs_add = rhs.as<Nodecl::VectorAdd>( );
                Nodecl::NodeclBase rhs_lhs = rhs_add.get_lhs( );
                Nodecl::NodeclBase rhs_rhs = rhs_add.get_rhs( );
                if( rhs_lhs.is_constant( ) )
                {
                    Nodecl::NodeclBase c = Nodecl::VectorAdd::make( lhs, rhs_lhs, n.get_mask(), lhs.get_type( ) );
                    const_value_t* c_value = _calc.compute_const_value( c );
                    if( !const_value_is_zero( c_value ))
                    {
                        Nodecl::Utils::replace( n, Nodecl::VectorAdd::make( const_value_to_nodecl(c_value), rhs_rhs, 
                                    n.get_mask(), lhs.get_type( ), n.get_locus( ) ) );
                    }
                    else
                    {
                        Nodecl::Utils::replace( n, rhs_rhs );
                    }
                }
            }
            else
            {   // R2
                Nodecl::Utils::replace( n, Nodecl::VectorAdd::make( rhs, lhs, n.get_mask(), lhs.get_type( ), n.get_locus( ) ) );
            }
        } 
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::VectorDiv& n )
    {   // R10
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( lhs.is_constant( ) && rhs.is_constant( ) &&
            const_value_is_zero( lhs.get_constant( ) ) && !const_value_is_zero( rhs.get_constant( ) ) )
        {
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_make_vector_from_scalar(
                            n.get_type().vector_num_elements(),
                            const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) ) );
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::VectorLowerOrEqualThan& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( rhs.is_constant( ) )
        {
            if( lhs.is<Nodecl::VectorAdd>( ) )
            {   // R20
                Nodecl::VectorAdd lhs_add = lhs.as<Nodecl::VectorAdd>( );
                Nodecl::NodeclBase lhs_lhs = lhs_add.get_lhs( );
                Nodecl::NodeclBase lhs_rhs = lhs_add.get_rhs( );
                if( lhs_lhs.is_constant( ) )
                {
                    Nodecl::NodeclBase c = Nodecl::VectorMinus::make( rhs, lhs_lhs, n.get_mask(), rhs.get_type( ) );
                    const_value_t* c_value = _calc.compute_const_value( c );
                    Nodecl::Utils::replace( n, Nodecl::VectorLowerOrEqualThan::make( lhs_rhs, const_value_to_nodecl( c_value ),
                                n.get_mask(), rhs.get_type( ), n.get_locus( ) ) );
                }
            }
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::VectorMinus& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( lhs.is_constant( ) && rhs.is_constant( ) )
        {   // R3
            const_value_t* c_value = _calc.compute_const_value( n );
            Nodecl::Utils::replace( n, const_value_to_nodecl( c_value ) );
        }
        else if( rhs.is_constant( ) )
        {   // R4
            if( const_value_is_zero( rhs.get_constant( ) ) )
            {
                Nodecl::Utils::replace( n, lhs );
            }
            else
            {
                Nodecl::NodeclBase neg_rhs = const_value_to_nodecl( const_value_neg( rhs.get_constant( ) ) );
                Nodecl::Utils::replace( n, Nodecl::VectorAdd::make( neg_rhs, lhs, n.get_mask(), lhs.get_type( ), n.get_locus( ) ) );
            }
        }
        else if( Nodecl::Utils::equal_nodecls( lhs, rhs ) )
        {
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_make_vector_from_scalar(
                            n.get_type().vector_num_elements(),
                            const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) ) );
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::VectorMod& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( ( rhs.is_constant() && lhs.is_constant() && const_value_is_one( lhs.get_constant( ) ) )
            || Nodecl::Utils::equal_nodecls( lhs, rhs ) )
        {   // R11
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_make_vector_from_scalar(
                            n.get_type().vector_num_elements(),
                            const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) ) );
        }
    }

    void ReduceExpressionVisitor::visit_post( const Nodecl::VectorMul& n )
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        if( ( lhs.is_constant( ) && const_value_is_zero( lhs.get_constant( ) ) )
            || ( rhs.is_constant( ) && const_value_is_zero( rhs.get_constant( ) ) ) )
        {   // 0 * t = t , t * 0 = t
            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_make_vector_from_scalar(
                            n.get_type().vector_num_elements(),
                            const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) ) );
        }
        else if( ( lhs.is_constant( ) && const_value_is_one( lhs.get_constant( ) ) ) )
        {   // 1 * t = t
            Nodecl::Utils::replace( n, rhs );
        }
        else if( ( rhs.is_constant( ) && const_value_is_one( rhs.get_constant( ) ) ) )
        {   // t * 1 = t
            Nodecl::Utils::replace( n, lhs );
        }
        else if( lhs.is_constant( ) && rhs.is_constant( ) )
        {   // R7
            const_value_t* c_value = _calc.compute_const_value( n );
            Nodecl::Utils::replace( n, const_value_to_nodecl( c_value ) );
        }
        else if ( rhs.is_constant( ) )
        {
            if( const_value_is_zero( rhs.get_constant( ) ) )
            {
                Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_make_vector_from_scalar(
                                n.get_type().vector_num_elements(),
                                const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) ) );
            }
            else
            {
                if( lhs.is<Nodecl::VectorMul>( ) )
                {   // R9
                    Nodecl::VectorMul lhs_mul = lhs.as<Nodecl::VectorMul>( );
                    Nodecl::NodeclBase lhs_lhs = lhs_mul.get_lhs();
                    Nodecl::NodeclBase lhs_rhs = lhs_mul.get_rhs();
                    if( lhs_lhs.is_constant( ) )
                    {
                        if( const_value_is_zero( lhs_lhs.get_constant( ) ) )
                        {
                            Nodecl::Utils::replace( n, const_value_to_nodecl( const_value_make_vector_from_scalar(
                                            n.get_type().vector_num_elements(),
                                            const_value_get_zero( /*num_bytes*/ 4, /*sign*/1 ) ) ) );
                        }
                        else
                        {
                            Nodecl::NodeclBase c = Nodecl::VectorMul::make( lhs_lhs, rhs, n.get_mask(),  rhs.get_type() );
                            const_value_t* c_value = _calc.compute_const_value( c );
                            Nodecl::Utils::replace( n, Nodecl::VectorMul::make( const_value_to_nodecl( c_value ), lhs_rhs, n.get_mask(),
                                                   rhs.get_type( ), n.get_locus( ) ) );
                        }
                    }
                }
                else
                {   // R8
                    Nodecl::Utils::replace( n, Nodecl::VectorMul::make( rhs, lhs, n.get_mask(), lhs.get_type( ), n.get_locus( ) ) );
                }
            }
        }
    }
    
}
}