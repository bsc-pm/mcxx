/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#ifndef TL_NANOS6_HPP
#define TL_NANOS6_HPP

#include "tl-compilerphase.hpp"
#include "tl-nodecl.hpp"

namespace TL { namespace Nanos6 {

    class LoweringPhase : public TL::CompilerPhase
    {
        public:
            LoweringPhase();

            virtual void run(DTO& dto);
            virtual void pre_run(DTO& dto);

            virtual void phase_cleanup(DTO& data_flow);

            Nodecl::List &get_extra_c_code() { return _extra_c_code; }

        private:
            void fortran_load_api(DTO& dto);

            std::string _final_clause_transformation_str;
            bool _final_clause_transformation_disabled;
            void set_disable_final_clause_transformation(const std::string& str);


            Nodecl::List _extra_c_code;
            
            friend struct Lower;
    };

} }

#endif // TL_NANOS6_HPP
