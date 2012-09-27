/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#include "tl-nodecl-utils.hpp"
#include "tl-induction-variables-data.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ********************************************************************************************* //
    // ************************* Class representing and induction variable ************************* //

    InductionVariableData::InductionVariableData( ExtendedSymbol var,
                                                  InductionVarType type, Nodecl::NodeclBase family )
        : _var( var ), _lb( Nodecl::NodeclBase::null( ) ), _ub( Nodecl::NodeclBase::null( ) ),
          _stride( Nodecl::NodeclBase::null( ) ), _type( type ), _family( family )
    {}

    ExtendedSymbol InductionVariableData::get_variable() const
    {
        return _var;
    }

    void InductionVariableData::set_variable( Nodecl::NodeclBase var )
    {
        _var = var;
    }

    Nodecl::NodeclBase InductionVariableData::get_lb( ) const
    {
        return _lb;
    }

    void InductionVariableData::set_lb( Nodecl::NodeclBase lb )
    {
        _lb = lb;
    }

    Nodecl::NodeclBase InductionVariableData::get_ub( ) const
    {
        return _ub;
    }

    void InductionVariableData::set_ub( Nodecl::NodeclBase ub )
    {
        _ub = ub;
    }

    Nodecl::NodeclBase InductionVariableData::get_stride( ) const
    {
        return _stride;
    }

    void InductionVariableData::set_stride( Nodecl::NodeclBase stride )
    {
        _stride = stride;
    }

    bool InductionVariableData::is_basic( )
    {
        return ( _type == BASIC_IV );
    }

    std::string InductionVariableData::get_type_as_string( ) const
    {
        std::string t = "";

        switch ( _type )
        {
            case BASIC_IV:      t = "BASIC_IV";
            case DERIVED_IV:    t = "DERIVED_IV";
            default: ;
        }

        return t;
    }

    Nodecl::NodeclBase InductionVariableData::get_family( ) const
    {
        return _family;
    }

    bool InductionVariableData::operator==( const InductionVariableData& rhs ) const
    {
        return ( Nodecl::Utils::equal_nodecls( _var.get_nodecl( ), rhs._var.get_nodecl( ) )
                 && Nodecl::Utils::equal_nodecls( _lb, rhs._lb )
                 && Nodecl::Utils::equal_nodecls( _ub, rhs._ub )
                 && Nodecl::Utils::equal_nodecls( _stride, rhs._stride )
                 && ( _type == rhs._type ) && ( _family == rhs._family ) );
    }

    // *********************** END class representing and induction variable *********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************************* Induction Variables utils ********************************* //

    bool induction_variable_list_contains_variable( ObjectList<InductionVariableData> iv_list,
                                                    Nodecl::NodeclBase var )
    {
        for( ObjectList<InductionVariableData>::iterator it = iv_list.begin( );
             it != iv_list.end( ); ++it )
        {
            if( Nodecl::Utils::equal_nodecls( it->get_variable().get_nodecl(), var ) )
                return true;
        }

        return true;
    }

    // ******************************* END Induction Variables utils ******************************* //
    // ********************************************************************************************* //
}
}
}