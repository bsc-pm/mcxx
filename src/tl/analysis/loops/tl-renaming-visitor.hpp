/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#ifndef TL_RENAMING_VISITOR_HPP
#define TL_RENAMING_VISITOR_HPP

#include <map>

#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {

    //! This class traverses a Nodecl and creates a new Nodecl renaming the symbols inside
    //! depending on the renaming map.
    /*!
    * The list may contain:
    * - Zero elements: no renames has been performed
    * - More element: renamed nodecls
    */
    class LIBTL_CLASS RenamingVisitor : public Nodecl::NodeclVisitor<TL::ObjectList<Nodecl::NodeclBase> >
    {
    private:
        //! map used to rename de nodes
        //! when this value is null, the renaming visitor just substitutes a range by its LB or UB, depending on #limit value
        std::map<Symbol, Nodecl::NodeclBase> _rename_map;
        const char* _filename;
        unsigned int _line;

        //! Value stored to recuperate info about the symbol that matches in a rename process
        Symbol _s;

        bool _computing_limits;

        // *** Auxiliary methods *** //

        nodecl_t create_nodecl_list(ObjectList<Nodecl::NodeclBase> list);

        template <typename T>
        Ret visit_binary(const T& n);

        template <typename T>
        void create_new_range_from_binary_node(T& n, Nodecl::NodeclBase lb1, Nodecl::NodeclBase ub1,
                                                            Nodecl::NodeclBase lb2, Nodecl::NodeclBase ub2,
                                                            Nodecl::NodeclBase stride);

        template <typename T>
        Nodecl::NodeclBase create_new_binary_node(T& n, Nodecl::NodeclBase lhs, Nodecl::NodeclBase rhs);

        template <typename T>
        Ret visit_unary(const T& n);

    public:
        // *** Constructors *** //
        RenamingVisitor(std::map<Symbol, Nodecl::NodeclBase> rename_map, const char* _filename, int _line);
        RenamingVisitor(const RenamingVisitor& rename_v);


        // *** Getters and Setters *** //
        Symbol get_matching_symbol() const;

        void set_computing_range_limits(bool computing_limits);

        static Nodecl::NodeclBase combine_variable_values(Nodecl::NodeclBase node1, Nodecl::NodeclBase node2);

        // ************************************************************************************** //
        // ********************************** Visiting methods ********************************** //

        Ret unhandled_node( const Nodecl::NodeclBase& n );
        Ret visit( const Nodecl::Add& n );
        Ret visit( const Nodecl::AddAssignment& n );
        Ret visit( const Nodecl::ArithmeticShr& n );
        Ret visit( const Nodecl::ArithmeticShrAssignment& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::Assignment& n );
        Ret visit( const Nodecl::BitwiseAnd& n );
        Ret visit( const Nodecl::BitwiseAndAssignment& n );
        Ret visit( const Nodecl::BitwiseNot& n );
        Ret visit( const Nodecl::BitwiseOr& n );
        Ret visit( const Nodecl::BitwiseOrAssignment& n );
        Ret visit( const Nodecl::BitwiseShl& n );
        Ret visit( const Nodecl::BitwiseShlAssignment& n );
        Ret visit( const Nodecl::BitwiseShr& n );
        Ret visit( const Nodecl::BitwiseShrAssignment& n);
        Ret visit( const Nodecl::BitwiseXor& n );
        Ret visit( const Nodecl::BitwiseXorAssignment& n );
        Ret visit( const Nodecl::BooleanLiteral& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::ComplexLiteral& n );
        Ret visit( const Nodecl::ConditionalExpression& n );
        Ret visit( const Nodecl::Conversion& n );
        Ret visit( const Nodecl::Dereference& n );
        Ret visit( const Nodecl::Different& n );
        Ret visit( const Nodecl::Div& n );
        Ret visit( const Nodecl::DivAssignment& n );
        Ret visit( const Nodecl::Equal& n );
        Ret visit( const Nodecl::FloatingLiteral& n );
        Ret visit( const Nodecl::FunctionCall& n );
        Ret visit( const Nodecl::GreaterOrEqualThan& n );
        Ret visit( const Nodecl::GreaterThan& n );
        Ret visit( const Nodecl::IntegerLiteral& n );
        Ret visit( const Nodecl::LogicalAnd& n );
        Ret visit( const Nodecl::LogicalNot& n );
        Ret visit( const Nodecl::LogicalOr& n );
        Ret visit( const Nodecl::LowerOrEqualThan& n );
        Ret visit( const Nodecl::LowerThan& n );
        Ret visit( const Nodecl::MaskLiteral& n );
        Ret visit( const Nodecl::Minus& n );
        Ret visit( const Nodecl::MinusAssignment& n );
        Ret visit( const Nodecl::Mod& n );
        Ret visit( const Nodecl::ModAssignment& n );
        Ret visit( const Nodecl::Mul& n );
        Ret visit( const Nodecl::MulAssignment& n );
        Ret visit( const Nodecl::Neg& n );
        Ret visit( const Nodecl::Plus& n );
        Ret visit( const Nodecl::Postdecrement& n );
        Ret visit( const Nodecl::Postincrement& n );
        Ret visit( const Nodecl::Power& n );
        Ret visit( const Nodecl::Predecrement& n );
        Ret visit( const Nodecl::Preincrement& n );
        Ret visit( const Nodecl::Range& n );
        Ret visit( const Nodecl::Reference& n );
        Ret visit( const Nodecl::Sizeof& n );
        Ret visit( const Nodecl::StringLiteral& n );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::VirtualFunctionCall& n );

        // ******************************** END visiting methods ******************************** //
        // ************************************************************************************** //
    };
}
}

#endif      // TL_RENAMING_VISITOR_HPP

