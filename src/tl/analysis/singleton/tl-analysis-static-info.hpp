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

#ifndef TL_ANALYSIS_STATIC_INFO_HPP
#define TL_ANALYSIS_STATIC_INFO_HPP

#include "tl-analysis-singleton.hpp"
#include "tl-induction-variables-data.hpp"
#include "tl-objectlist.hpp"

namespace TL {
namespace Analysis {

    class NodeclStaticInfo
    {
        private:
            ObjectList<Analysis::Utils::InductionVariableData*> _induction_variables;
            ObjectList<Nodecl::NodeclBase> _constants;

        public:
            NodeclStaticInfo( ObjectList<Analysis::Utils::InductionVariableData*> induction_variables,
                              ObjectList<Nodecl::NodeclBase> constants );

            bool is_constant( const Nodecl::NodeclBase& n ) const;
            bool is_induction_variable( const Nodecl::NodeclBase& n );

    };


    class AnalysisStaticInfo
    {
        private:
            typedef std::map<Nodecl::NodeclBase, NodeclStaticInfo> static_info_map_t;
            typedef std::pair<Nodecl::NodeclBase, NodeclStaticInfo> static_info_pair_t;
            static_info_map_t _static_info_map;

        public:
            AnalysisStaticInfo( const Nodecl::NodeclBase );

            bool is_constant( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const;
            bool is_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n );
    };

}
}

#endif // TL_ANALYSIS_STATIC_INFO_HPP
