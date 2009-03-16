/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::master_postorder(OpenMP::MasterConstruct master_construct)
        {
            Source master_source;

            Statement statement = master_construct.body();

            master_source
                << "if (in__tone_is_master_())"
                << "{"
                <<    statement.prettyprint()
                << "}"
                ;

            AST_t master_tree = master_source.parse_statement(master_construct.get_ast(),
                    master_construct.get_scope_link());

            master_construct.get_ast().replace(master_tree);
        }
    }
}
