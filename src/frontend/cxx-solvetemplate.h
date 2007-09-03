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
#ifndef CXX_SOLVETEMPLATE_H
#define CXX_SOLVETEMPLATE_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

#include "cxx-typeunif.h"
#include "cxx-buildscope-decls.h"
#include "cxx-scope-decls.h"

struct matching_pair_tag
{
    scope_entry_t* entry;
    unification_set_t* unif_set;
};

matching_pair_t* solve_template(decl_context_t decl_context,
        scope_entry_list_t* candidate_templates, 
        template_argument_list_t* arguments, 
        char give_exact_match);

char match_one_template(template_argument_list_t* arguments, 
        template_argument_list_t* specialized, scope_entry_t* specialized_entry, 
        unification_set_t* unif_set,
        decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_SOLVETEMPLATE_H
