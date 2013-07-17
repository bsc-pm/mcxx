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

#ifndef TL_VECTORIZER_UTILS_VERSIONING_HPP
#define TL_VECTORIZER_UTILS_VERSIONING_HPP

#include "tl-nodecl-visitor.hpp"
#include <map>


namespace TL 
{ 
    namespace Vectorization 
    {
        namespace Utils
        {
            class LookForReturnVisitor : public Nodecl::ExhaustiveVisitor<void>
            {
                private:
                    bool* _return_inside;

                public:
                    LookForReturnVisitor(bool * return_inside);

                    virtual void visit(const Nodecl::ReturnStatement& n);
            };

            Nodecl::NodeclBase get_new_mask_symbol(TL::Scope& scope,
                    const int masks_size);
            Nodecl::NodeclBase emit_disjunction_mask(
                    const ObjectList<Nodecl::NodeclBase>& bb_exit_mask_list,
                    Nodecl::List& output_stmt_list,
                    TL::Scope& scope,
                    const int masks_size);

            bool is_declared_in_scope(const scope_t *const  target_scope,
                    const scope_t *const symbol_scope);
        }
    }
}

#endif //TL_VECTORIZER_UTILS_VERSIONING_HPP

