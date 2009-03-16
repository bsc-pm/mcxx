/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - David Rodenas Pico
    Copyright (C) 2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef ACOTES_OUTPUTTASKS_HPP
#define ACOTES_OUTPUTTASKS_HPP

#include <map>

#include "tl-object.hpp"
#include "tl-refptr.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"

namespace TL
{
    struct OutputTask
    {
        std::string filename;
        AST_t code;
        ScopeLink scope_link;
    };

    class OutputTasks : public Object
    {
        private:
            std::map<std::string, ObjectList<OutputTask> > _map_tasks;
        public:
            OutputTasks() { }
            void add_task(const OutputTask &output_task);

            ObjectList<std::string> get_files() const;
            ObjectList<OutputTask> get_file_tasks(const std::string &filename) const;
    };
}

#endif // ACOTES_OUTPUTFILES_HPP
