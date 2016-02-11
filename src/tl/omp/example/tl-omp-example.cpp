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

#include "tl-omp-example.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-source.hpp"
#include <iostream>

/*
   Note, for the sake of the example, this is written in a single-big-file(TM)
   but none prevents the developer from splitting the methods and the classes.
 */

/*
 * How to read this example?
 *
 * Go first to Lowering::run below and follow from there.
 */

namespace TL { namespace OpenMPExample {

    struct LoweringVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        // This follows the GoF Visitor pattern using dynamic dispatch
        //
        // http://en.wikipedia.org/wiki/Visitor_pattern
        //
        // Define a
        //
        //   virtual void visit(const T&) { ... }
        //
        // where T is the kind of node you are interested.
        //
        // The list of nodes is generated mechanically using the definition in
        // src/frontend/cxx-nodecl.def. Adding new nodes is beyond the scope of
        // this example.
        //
        // In that file you will find nodes of the form NODECL_OPEN_M_P*FOO_BAR.
        //
        // The compiler has a Nodecl::OpenMP::FooBar node for each of it.
        //
        // The default visit of an ExhaustiveVisitor<void> is defined like this.
        //
        // virtual void visit(const T& t)
        // {
        //    visit_pre(t);
        //    walk(t.get_child_1());
        //    walk(t.get_child_2());
        //    ...
        //    walk(t.get_child_N()); /* Our nodes have at most N = 4 */
        //    visit_post(t);
        // }
        //
        // walk is the method to continue the visit in the current visitor. If you override visit
        // and do not call walk with a children, the visit will stop there, obviously.

        // This is a simple example with an OpenMP::Task
        // We do not override visit because we will use pre and post
        //
        // Typically in pre one does "analysis" (i.e. you gather information
        // for visit_post or later visit_pre of inner children)
        virtual void visit_pre(const Nodecl::OpenMP::Task& task);
        // Typically in pre one does "synthesis" (i.e. transformations)
        virtual void visit_post(const Nodecl::OpenMP::Task& task);

        // Now go and read the implementation of these member functions below
    };

    Lowering::Lowering()
    {
        // In the constructor the phase self-describes. This information is used in --help
        set_phase_name("OpenMP Lowering Example phase");
        set_phase_description("This phase showcases how to lower from Mercurium parallel IR");

        // Parameters of the phase may also be registered here, but in this
        // example we do not register any
    }

    void Lowering::pre_run(DTO& dto)
    {
        // This is called before parsing takes place. It can be used to
        // register "builtin" stuff befor anything is parsed. That said, it is
        // seldomly used these days.
        std::cerr << __PRETTY_FUNCTION__ << std::endl;
    }

    void Lowering::run(DTO& dto)
    {
        // This is the entry point of this phase. It is called after the file has
        // been parsed. Phases are called in the same order as they appear in the
        // compiler_phase=... option in the profile of Mercurium.
        std::cerr << __PRETTY_FUNCTION__<< std::endl;

        Nodecl::NodeclBase n = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
        // n is the root node of the translation file (i.e. the file),
        // typically one uses an exhaustive visitor and defines visitors for
        // the nodes you may be interested.

        // Now we create a LoweringVisitor, a visitor class that we will use to
        // traverse the tree in a handy manner
        LoweringVisitor visitor;

        // Now we walk the whole file.
        //
        // Go now to the definition of LoweringVisitor above and read
        // the comments on how it works.
        visitor.walk(n);
    }

    void Lowering::phase_cleanup(DTO& data_flow)
    {
        // This is called after all the phases have run
        std::cerr << __PRETTY_FUNCTION__ << std::endl;
    }

    /*** Visitor ***/

    void LoweringVisitor::visit_pre(const Nodecl::OpenMP::Task& task)
    {
        std::cerr << __PRETTY_FUNCTION__ << std::endl;
        // No analysis in this example
    }

    void LoweringVisitor::visit_post(const Nodecl::OpenMP::Task& task)
    {
        std::cerr << __PRETTY_FUNCTION__ << std::endl;

        // For this example we assume the user #include'd <stdio.h>

        Nodecl::NodeclBase new_statement; // This will be invalid until we parse
        Source src;
        src
            << "{"
            <<     "fprintf(stderr, \"Before the task\\n\");"
            <<     statement_placeholder(new_statement)
            <<     "fprintf(stderr, \"After the task\\n\");"
            << "}"
            ;

        // Here task plays the role of context for a successful parsing
        Nodecl::NodeclBase generated_code = src.parse_statement(task);

        // After parse_statement, new_statement is an empty statement (';')
        // that can be replaced with another tree (so generated_code now is
        // a sort of skeleton)

        // We will replace new_statement with the statements of the task.
        // This may look a bit crude (and indeed it is) but it is fine
        // because we are not changing any "symbolic bindings" or stuff
        // like this
        new_statement.replace(task.get_statements());

        // Now replace the original task code with the new code we parsed above (and that now includes
        // the body of the task inside)
        task.replace(generated_code);
    }

} }


// This defines the entry point for loading the plugin
EXPORT_PHASE(TL::OpenMPExample::Lowering);
