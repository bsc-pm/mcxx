

/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             **
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

#ifndef TL_RANGE_ANALYSIS_UTILS_HPP
#define TL_RANGE_ANALYSIS_UTILS_HPP

#include "tl-induction-variables-data.hpp"
#include "tl-nodecl-visitor.hpp"

#define RANGES_DEBUG debug_options.ranges_verbose

namespace TL {
namespace Analysis {
namespace Utils {
    
    // ******************************************************************************************* //
    // *********************************** Ranges arithmetic ************************************* //

    Type get_range_type(Type t1, Type t2);

    bool nodecl_is_Z_range(const NBase& n);

    NBase range_sub(const NBase& r1, const NBase& r2);

    NBase range_addition(const NBase& r1, const NBase& r2);
    NBase range_subtraction(const NBase& r1, const NBase& r2);
    NBase range_multiplication(const NBase& r1, const NBase& r2);
    NBase range_division(const NBase& dividend, const NBase& divisor);
    NBase range_intersection(const NBase& r, const NBase& r2);
    NBase range_union(const NBase& r1, const NBase& r2);
    Nodecl::Range range_value_add(const Nodecl::Range& r, const NBase& v);
    Nodecl::Range range_value_sub(const Nodecl::Range& r, const NBase& v);
    Nodecl::Range range_value_mul(const Nodecl::Range& r, const NBase& v);
    Nodecl::Range range_value_div(const Nodecl::Range& r, const NBase& v);

    // ********************************* END Ranges arithmetic *********************************** //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ******************************* Range Analysis Constraints ******************************** //

    #define CONSTRAINT_KIND_LIST        \
    CONSTRAINT_KIND(Array)              \
    CONSTRAINT_KIND(BackEdge)           \
    CONSTRAINT_KIND(BinaryOp)           \
    CONSTRAINT_KIND(ComparatorTrue)     \
    CONSTRAINT_KIND(ComparatorFalse)    \
    CONSTRAINT_KIND(Function)           \
    CONSTRAINT_KIND(GlobalVar)          \
    CONSTRAINT_KIND(Mod)                \
    CONSTRAINT_KIND(ModTrue)            \
    CONSTRAINT_KIND(ModFalse)           \
    CONSTRAINT_KIND(NonLocalSym)        \
    CONSTRAINT_KIND(Parameter)          \
    CONSTRAINT_KIND(Propagated)         \
    CONSTRAINT_KIND(Replace)            \
    CONSTRAINT_KIND(UnaryOp)            \
    CONSTRAINT_KIND(Undefined)

    enum ConstraintKind {
        #undef CONSTRAINT_KIND
        #define CONSTRAINT_KIND(X) __##X,
        CONSTRAINT_KIND_LIST
        #undef CONSTRAINT_KIND
    };

    /*! The possible constraints are:
     *  - Y = [lb, ub]
     *  - Y = X1 bin_op X2
     *  - Y = phi(X1, X2)
     *  - Y = X ∩ [lb, ub]
     */
    struct Constraint {
        TL::Symbol _ssa_sym;    /*!< symbol associated to a given variable at this point of the program */
        NBase _value;           /*!< value applying to the variable */
        ConstraintKind _kind;   /*!< kind of the constraint */

        // *** Constructors *** //
        Constraint();               // Required for std::map operations
        Constraint(const TL::Symbol& constr_sym,
                   const NBase& value, const ConstraintKind& kind);

        // *** Getters and Setters *** //
        const TL::Symbol& get_symbol() const;
        void set_symbol(const TL::Symbol& s);
        NBase& get_value();
        const ConstraintKind& get_kind() const;

        // *** Comparators *** //
        bool operator!=(const Constraint& c) const;
        bool operator==(const Constraint& c) const;

        // *** Utils *** //
        void print_constraint();
    };

    // ***************************** END Range Analysis Constraints ****************************** //
    // ******************************************************************************************* //


    class LIBTL_CLASS InfinityCalculator : public Nodecl::NodeclVisitor<NBase>
    {
    private:
        // *** Visitors *** //
        Ret unhandled_node(const Nodecl::NodeclBase& n);
        Ret visit(const Nodecl::Add& n);
        Ret visit(const Nodecl::Analysis::MinusInfinity& n);
        Ret visit(const Nodecl::Analysis::PlusInfinity& n);
        Ret visit(const Nodecl::BooleanLiteral& n);
        Ret visit(const Nodecl::ComplexLiteral& n);
        Ret visit(const Nodecl::Div& n);
        Ret visit(const Nodecl::FloatingLiteral& n);
        Ret visit(const Nodecl::IntegerLiteral& n);
        Ret visit(const Nodecl::Minus& n);
        Ret visit(const Nodecl::Mul& n);
        Ret visit(const Nodecl::Neg& n);
        Ret visit(const Nodecl::Plus& n);
        Ret visit(const Nodecl::StringLiteral& n);
        Ret visit(const Nodecl::Symbol& n);

    public:
        // *** Constructors *** //
        InfinityCalculator();

        NBase compute(Nodecl::NodeclBase val);
    };

}
}
}

#endif          // TL_RANGE_ANALYSIS_UTILS_HPP
