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

struct ReplaceSymbols : Nodecl::ExhaustiveVisitor<void>
{
    std::map<TL::Symbol, std::string> _m;
    TL::Scope _scope;

    ReplaceSymbols(std::map<TL::Symbol, std::string> m, TL::Scope scope)
        : _m(m), _scope(scope)
    { }

    virtual void visit(const Nodecl::Symbol& node)
    {
        TL::Symbol sym = node.get_symbol();
        auto it = _m.find(sym);
        if (it != _m.end()) {
            node.replace(Source(it->second).parse_expression(_scope));
        }
    }
};

struct SymbolExistenceCheck : Nodecl::ExhaustiveVisitor<void>
{
    TL::Symbol _sym;
    bool exist = false;

    SymbolExistenceCheck(TL::Symbol sym)
        : _sym(sym)
    { }

    bool exist_symbol() const { return exist; }

    virtual void visit(const Nodecl::Symbol& node)
    {
        TL::Symbol sym = node.get_symbol();
        if (_sym == sym) exist = true;
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

