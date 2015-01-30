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

#ifndef TL_OMP_AUTO_SCOPE_HPP
#define TL_OMP_AUTO_SCOPE_HPP

#include "tl-analysis-interface.hpp"
#include "tl-pragmasupport.hpp"

namespace TL {
namespace OpenMP {

    /*! \brief Phase for Automatic Data-Sharing computation
     * This phase computes the data-sharing of all variables involved in OpenMP tasks 
     * that are not manually scoped when it appears the clause default(AUTO)
     * The results are printed in the standard error
     */
    class AutoScopePhase : public TL::PragmaCustomCompilerPhase
    {
    private:
        std::string _auto_scope_enabled_str;
        bool _auto_scope_enabled;
        void set_auto_scope(const std::string auto_scope_enabled_str);
        
        std::string _ompss_mode_str;
        bool _ompss_mode_enabled;
        void set_ompss_mode( const std::string& ompss_mode_str);

    public:
        AutoScopePhase( );
        virtual ~AutoScopePhase( ) {}

        virtual void pre_run(TL::DTO& dto);
        virtual void run( TL::DTO& dto );
    };
    
    // ************* END phase for Automatic Data-Sharing computation *************** //
    // ****************************************************************************** //
}
}

#endif // TL_OMP_AUTO_SCOPE_HPP
