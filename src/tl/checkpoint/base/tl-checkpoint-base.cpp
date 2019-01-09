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

#include "tl-checkpoint-base.hpp"

#include "cxx-diagnostic.h"

namespace TL { namespace Checkpoint {

    Base::Base()
        : PragmaCustomCompilerPhase()
    {
        set_phase_name("Checkpoint directives to IR");
        set_phase_description("This phase lowers our checkpoints directives into Mercurium's IR");

        register_directive("chk", "store");
        register_directive("chk", "load");
        register_directive("chk", "init");
        register_directive("chk", "shutdown");

        dispatcher("chk").directive.post["store"].connect(
                std::bind(&Base::store_directive_handler_post, this, std::placeholders::_1));
        dispatcher("chk").directive.post["load"].connect(
                std::bind(&Base::load_directive_handler_post, this, std::placeholders::_1));
        dispatcher("chk").directive.post["init"].connect(
                std::bind(&Base::init_directive_handler_post, this, std::placeholders::_1));
        dispatcher("chk").directive.post["shutdown"].connect(
                std::bind(&Base::shutdown_directive_handler_post, this, std::placeholders::_1));
    }

    void Base::store_directive_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();

        PragmaClauseArgList parameter = directive.get_pragma_line().get_parameter();
        if (parameter.is_null())
            fatal_printf_at(directive.get_locus(), "A store construct must specify some data to be checkpointed");

        Nodecl::List env;
        env.append(Nodecl::Checkpoint::Data::make(
                    Nodecl::List::make(parameter.get_arguments_as_expressions())));

        if (!pragma_line.get_clause("level").is_defined())
            fatal_printf_at(directive.get_locus(), "missing mandatory 'level' clause");

        TL::ObjectList<Nodecl::NodeclBase> level_args = pragma_line.get_clause("level").get_arguments_as_expressions();
        if (level_args.size() != 1)
            fatal_printf_at(directive.get_locus(), "invalid number of arguments in 'level' clause (it should be one argument)");

        env.append(Nodecl::Checkpoint::Level::make(level_args[0]));

        if (!pragma_line.get_clause("id").is_defined())
            fatal_printf_at(directive.get_locus(), "missing mandatory 'id' clause");

        TL::ObjectList<Nodecl::NodeclBase> id_args = pragma_line.get_clause("id").get_arguments_as_expressions();
        if (id_args.size() != 1)
            fatal_printf_at(directive.get_locus(), "invalid number of arguments in 'id' clause (it should be one argument)");

        env.append(Nodecl::Checkpoint::Id::make(id_args[0]));

        if (pragma_line.get_clause("kind").is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> kind_args = pragma_line.get_clause("kind").get_arguments_as_expressions();
            if (kind_args.size() != 1)
                fatal_printf_at(directive.get_locus(), "invalid number of arguments in 'kind' clause (it should be one argument)");

            env.append(Nodecl::Checkpoint::Kind::make(kind_args[0]));
        }

        if (pragma_line.get_clause("if").is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> if_args = pragma_line.get_clause("if").get_arguments_as_expressions();
            if (if_args.size() != 1)
                fatal_printf_at(directive.get_locus(), "invalid number of arguments in 'if' clause (it should be one argument)");

            env.append(Nodecl::Checkpoint::If::make(if_args[0]));
        }

        directive.replace(
               Nodecl::Checkpoint::Store::make(env, directive.get_locus()));
    }

    void Base::load_directive_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();

        PragmaClauseArgList parameter = directive.get_pragma_line().get_parameter();
        if (parameter.is_null())
            fatal_printf_at(directive.get_locus(), "A load construct must specify some data to be checkpointed");

        if (pragma_line.get_clause("if").is_defined())
            fatal_printf_at(directive.get_locus(), "The 'if' clause is not currently supported on the load construct");

        Nodecl::List env;
        env.append(Nodecl::Checkpoint::Data::make(
                    Nodecl::List::make(parameter.get_arguments_as_expressions())));
        directive.replace(
               Nodecl::Checkpoint::Load::make(env, directive.get_locus()));
    }

    void Base::init_directive_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        if (!pragma_line.get_clause("comm").is_defined())
            fatal_printf_at(directive.get_locus(), "missing mandatory 'comm' clause");

        TL::ObjectList<Nodecl::NodeclBase> comm_args = pragma_line.get_clause("comm").get_arguments_as_expressions();
        if (comm_args.size() != 1)
            fatal_printf_at(directive.get_locus(), "invalid number of arguments in 'commm' clause (it should be one argument)");

        Nodecl::List env;
        env.append(Nodecl::Checkpoint::Comm::make(comm_args[0]));
        directive.replace(
               Nodecl::Checkpoint::Init::make(env, directive.get_locus()));
    }

    void Base::shutdown_directive_handler_post(TL::PragmaCustomDirective directive)
    {
        directive.replace(Nodecl::Checkpoint::Shutdown::make());
    }
}}

EXPORT_PHASE(TL::Checkpoint::Base)
