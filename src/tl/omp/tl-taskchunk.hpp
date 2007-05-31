/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TL_TASKCHUNK_HPP
#define TL_TASKCHUNK_HPP

#include "tl-omptransform.hpp"
namespace TL
{
    struct TaskWhileInfo
    {
        public:
            std::string chunking;
            Source& pre_src;
            Source& post_src;

            TaskWhileInfo()
                : pre_src(*(new Source)),
                post_src(*(new Source))
            {
            }
    };

    extern std::stack<TaskWhileInfo> task_while_stack;
}
#endif // TL_TASKCHUNK_HPP
