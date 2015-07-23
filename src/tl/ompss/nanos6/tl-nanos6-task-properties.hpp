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


#ifndef TL_NANOS6_TASK_PROPERTIES_HPP
#define TL_NANOS6_TASK_PROPERTIES_HPP

#include "tl-nodecl.hpp"
#include "tl-type.hpp"
#include "tl-symbol.hpp"

namespace TL { namespace Nanos6 {

    struct TaskProperties
    {
        private:
            typedef std::map<TL::Symbol, TL::Symbol> field_map_t;
            field_map_t field_map;

            void create_outline_function();
            void create_dependences_function();
            void create_copies_function();

            void add_field_to_class(
                    TL::Symbol class_symbol,
                    TL::Scope class_scope,
                    TL::Symbol var,
                    TL::Type field_type);

            TL::Scope compute_scope_for_environment_structure();

            TL::Type info_structure;
            TL::Symbol outline_function;
            TL::Symbol dependences_function;
            TL::Symbol copies_function;

        public:
            TL::ObjectList<TL::Symbol> shared;
            TL::ObjectList<TL::Symbol> private_;
            TL::ObjectList<TL::Symbol> firstprivate;
            Nodecl::NodeclBase final_;
            bool is_tied;
            std::string task_label;

            TL::ObjectList<Nodecl::NodeclBase> dep_in;
            TL::ObjectList<Nodecl::NodeclBase> dep_out;
            TL::ObjectList<Nodecl::NodeclBase> dep_inout;

            TL::ObjectList<Nodecl::NodeclBase> copy_in;
            TL::ObjectList<Nodecl::NodeclBase> copy_out;
            TL::ObjectList<Nodecl::NodeclBase> copy_inout;

            bool is_function_task;
            Nodecl::NodeclBase task_body;

            // For inline related_function is the enclosing task,
            // for function tasks, it is the function task itself
            TL::Symbol related_function;
            const locus_t* locus_of_task;

            TaskProperties()
                : is_tied(true), is_function_task(false) { }

            static TaskProperties gather_task_properties(const Nodecl::OpenMP::Task& node);

            void create_task_info(
                    /* out */
                    TL::Symbol &task_info,
                    Nodecl::NodeclBase& local_init);

            void create_environment_structure(
                    /* out */
                    TL::Type& data_env_struct,
                    Nodecl::NodeclBase& args_size);

            void capture_environment(
                    TL::Symbol args,
                    /* out */
                    Nodecl::NodeclBase& capture_env);
    };

} }

#endif // TL_NANOS6_TASK_PROPERTIES_HPP
