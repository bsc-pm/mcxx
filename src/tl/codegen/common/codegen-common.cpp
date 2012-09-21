/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "codegen-common.hpp"

namespace Codegen
{

CodegenVisitor::CodegenVisitor()
: _is_file_output(false)
{
}

bool CodegenVisitor::is_file_output() const
{
    return _is_file_output;
}

std::string CodegenVisitor::codegen_to_str(const Nodecl::NodeclBase& n, TL::Scope sc)
{
    bool old_is_file_output = this->_is_file_output;
    this->_is_file_output = false;

    this->push_scope(sc);
    std::string result = this->codegen(n);
    this->pop_scope();

    this->_is_file_output = old_is_file_output;
    return result;
}

void CodegenVisitor::codegen_top_level(const Nodecl::NodeclBase& n, FILE* f)
{
    bool old_is_file_output = this->_is_file_output;
    this->_is_file_output = true;

    this->push_scope( n.retrieve_context() );

    this->codegen_cleanup();

    std::string str ( this->codegen(n) );
    this->pop_scope();

    fprintf(f, "%s", str.c_str());

    this->_is_file_output = old_is_file_output;
}

CodegenVisitor::Ret CodegenVisitor::unhandled_node(const Nodecl::NodeclBase & n)
{ 
    internal_error("Unhandled node %s\n", ast_print_node_type(n.get_kind()));
    return Ret(); 
}

}
