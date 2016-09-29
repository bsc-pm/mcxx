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

#ifndef TL_FINAL_STMTS_GENERATOR_HPP
#define TL_FINAL_STMTS_GENERATOR_HPP

#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"

#include <map>

namespace TL {

    class FinalStmtsGenerator : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            bool _ompss_mode;
            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _final_stmts_map;
            Nodecl::Utils::SimpleSymbolMap _function_translation_map;

        public:
            FinalStmtsGenerator(bool ompss_mode);

            void visit(const Nodecl::OpenMP::Task& task);
            void visit(const Nodecl::OmpSs::TaskCall& task_call);
            void visit(const Nodecl::OmpSs::TaskExpression& task_expr);
            void visit(const Nodecl::OpenMP::For& for_construct);

            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& get_final_stmts();

        private:
            Nodecl::NodeclBase generate_final_stmts(Nodecl::NodeclBase original_stmts);
    };

}
#endif // TL_FINAL_STMTS_GENERATOR_HPP
