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

#ifndef TL_LOWER_TASK_COMMON_HPP
#define TL_LOWER_TASK_COMMON_HPP

namespace TL { namespace Intel {

// This is used in task reduction functions to replace omp_in/omp_out by
// vector accesses
struct ReplaceInOutVect : Nodecl::ExhaustiveVisitor<void>
{
    TL::Symbol _orig_omp_in;
    TL::Symbol _orig_omp_out;
    TL::Symbol _new_omp_in;
    TL::Symbol _new_omp_out;
    TL::Symbol _ind_var;
    TL::Scope _scope;

    ReplaceInOutVect(
            TL::Symbol orig_omp_in,
            TL::Symbol orig_omp_out,
            TL::Symbol new_omp_in,
            TL::Symbol new_omp_out,
            TL::Symbol ind_var,
            TL::Scope scope)
        : _orig_omp_in(orig_omp_in), _orig_omp_out(orig_omp_out),
        _new_omp_in(new_omp_in), _new_omp_out(new_omp_out),
        _ind_var(ind_var), _scope(scope)
    { }

    virtual void visit(const Nodecl::Symbol& node)
    {
        TL::Symbol sym = node.get_symbol();

        if (sym == _orig_omp_in) {
            node.replace(Source(as_symbol(_new_omp_in) + "[" + as_symbol(_ind_var) + "]").parse_expression(_scope));
        }
        else if (sym == _orig_omp_out) {
            node.replace(Source(as_symbol(_new_omp_out) + "[" + as_symbol(_ind_var) + "]").parse_expression(_scope));
        }
    }
};

struct TaskEnvironmentVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        // These attributes apply for all kinds of tasks (inline task and
        // function tasks)
        bool is_untied;
        Nodecl::NodeclBase priority;

        // Grainsize for taskloop
        Nodecl::NodeclBase grainsize;
        Nodecl::NodeclBase num_tasks;

        // This attribute only for function task. Inline tasks will never have
        // a node here because tl-omp-base.cpp has already lowered the 'if'
        // clause
        Nodecl::NodeclBase if_condition;
        Nodecl::NodeclBase final_condition;

        // This attribute is used only for instrumentation
        Nodecl::NodeclBase task_label;

        TaskEnvironmentVisitor()
            : is_untied(false),
            priority(),
            grainsize(),
            if_condition(),
            final_condition(),
            task_label()
        {
        }

        void visit(const Nodecl::OpenMP::Priority& priority_)
        {
            this->priority = priority_.get_priority();
        }

        void visit(const Nodecl::OpenMP::Grainsize& grainsize_)
        {
            this->grainsize = grainsize_.get_grainsize();
        }

        void visit(const Nodecl::OpenMP::NumTasks& num_tasks_)
        {
            this->num_tasks = num_tasks_.get_num_tasks();
        }

        void visit(const Nodecl::OpenMP::Untied& untied)
        {
            this->is_untied = true;
        }

        void visit(const Nodecl::OpenMP::If& if_condition_)
        {
            this->if_condition = if_condition_.get_condition();
        }

        void visit(const Nodecl::OpenMP::Final& final_condition_)
        {
            this->final_condition = final_condition_.get_condition();
        }

};

} }
#endif // TL_LOWER_TASK_COMMON_HPP

