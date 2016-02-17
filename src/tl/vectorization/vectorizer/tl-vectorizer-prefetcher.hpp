/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#ifndef TL_VECTORIZER_PREFETCHER_HPP
#define TL_VECTORIZER_PREFETCHER_HPP


#include "tl-vectorizer-environment.hpp"
#include "tl-vectorization-prefetcher-common.hpp"
#include "tl-vectorization-common.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"


namespace TL
{
    namespace Vectorization
    {
        typedef std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase> pair_nodecl_nodecl_t;
        typedef std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> map_nodecl_nodecl_t;
        class Prefetcher : 
            public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                const prefetch_info_t& _pref_info;
                const VectorizerEnvironment& _environment;

            public:
                Prefetcher(const prefetch_info_t& pref_info,
                        const VectorizerEnvironment& environment);

                void visit(const Nodecl::ForStatement& n);
        };

        class GenPrefetch : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                const Nodecl::NodeclBase _loop;
                const map_nodecl_nodecl_t& _vaccesses;
                const VectorizerEnvironment& _environment;
                const prefetch_info_t& _pref_info;
                const objlist_nodecl_t _linear_vars;
                objlist_nodecl_t _pref_instr;
                Nodecl::NodeclBase _object_init;


                Nodecl::NodeclBase get_prefetch_node(
                        const Nodecl::NodeclBase& address,
                        const PrefetchKind kind,
                        const int distance);

            public:
                GenPrefetch(const Nodecl::NodeclBase& loop,
                        const map_nodecl_nodecl_t& vaccesses,
                        const VectorizerEnvironment& environment,
                        const prefetch_info_t& pref_info);

                void visit(const Nodecl::ForStatement& n);
                void visit(const Nodecl::VectorLoad& n);
                void visit(const Nodecl::VectorStore& n);
                void visit(const Nodecl::ObjectInit& n);

                objlist_nodecl_t get_prefetch_instructions();
        };
    }
}

#endif // TL_VECTORIZER_PREFETCHER_HPP
