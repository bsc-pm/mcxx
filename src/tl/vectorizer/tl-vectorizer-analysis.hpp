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

#ifndef TL_VECTORIZER_ANALYSIS_STATIC_INFO_HPP
#define TL_VECTORIZER_ANALYSIS_STATIC_INFO_HPP

#include "tl-nodecl-base.hpp"
#include "tl-analysis-static-info.hpp"
#include <map>


namespace TL 
{ 
    namespace Vectorization 
    {
        class VectorizerAnalysisStaticInfo : public Analysis::AnalysisStaticInfo
        {
            private:
                Nodecl::NodeclBase _original_node;

                std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _orig_to_copy_nodes;
                std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _copy_to_orig_nodes;
                
                std::map<TL::Symbol, TL::Symbol> _orig_to_copy_sym;
                std::map<TL::Symbol, TL::Symbol> _copy_to_orig_sym;

            public:
                VectorizerAnalysisStaticInfo(const Nodecl::NodeclBase& n, Analysis::WhichAnalysis analysis_mask,
                        Analysis::WhereAnalysis nested_analysis_mask, int nesting_level);

        };
    }
}

#endif //TL_VECTORIZER_ANALYSIS_STATIC_INFO_HPP

