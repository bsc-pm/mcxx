/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef TL_OMP_CORE_HPP
#define TL_OMP_CORE_HPP

#include "tl-compilerphase.hpp"
#include "tl-pragmasupport.hpp"

#include "tl-omp.hpp"

namespace TL
{
    namespace OpenMP
    {
        class Core : public TL::PragmaCustomCompilerPhase
        {
            private:
                void register_omp_constructs();

                // Handler functions
#define OMP_DIRECTIVE(_directive, _name) \
                void _name##_handler_pre(PragmaCustomConstruct); \
                void _name##_handler_post(PragmaCustomConstruct);
#define OMP_CONSTRUCT(_directive, _name) \
                OMP_DIRECTIVE(_directive, _name)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_DIRECTIVE

                RefPtr<OpenMP::Info> _openmp_info;

                void get_clause_symbols(PragmaCustomClause clause, ObjectList<Symbol>& sym_list);
                void get_reduction_symbols(PragmaCustomConstruct construct, 
                        PragmaCustomClause clause, ObjectList<ReductionSymbol>& sym_list);
                void get_data_explicit_attributes(PragmaCustomConstruct construct, 
                        DataSharing& data_sharing);
                void get_data_implicit_attributes(PragmaCustomConstruct construct, 
                        DataAttribute default_data_attr, 
                        DataSharing& data_sharing);
                void get_data_implicit_attributes_task(PragmaCustomConstruct construct,
                        DataSharing& data_sharing,
                        DataAttribute default_data_attr);

                void get_dependences_info(PragmaCustomConstruct construct, DataSharing& data_sharing);
                void get_dependences_info_clause(PragmaCustomClause clause,
                        DataSharing& data_sharing,
                        DependencyItem::DependencyAttribute dep_attr);

                DataAttribute get_default_data_sharing(PragmaCustomConstruct construct,
                        DataAttribute fallback_data_sharing);

                void common_parallel_handler(PragmaCustomConstruct ctr, DataSharing& data_sharing);
                void common_for_handler(PragmaCustomConstruct ctr, DataSharing& data_sharing);
                void common_workshare_handler(PragmaCustomConstruct construct, DataSharing& data_sharing);

                // This member function is implemented in tl-omp-tasks.cpp
                void task_function_handler_pre(PragmaCustomConstruct construct);
            public:
                Core();

                virtual void run(TL::DTO& dto);
                virtual void pre_run(TL::DTO& dto);

                virtual ~Core() { }
        };
    }
}

#endif // TL_OMP_CORE_HPP
