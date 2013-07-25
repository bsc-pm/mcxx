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

#include <unistd.h>
#include <fcntl.h>

// This is a g++ extension
#include <ext/stdio_filebuf.h>

namespace Codegen
{

CodegenVisitor::CodegenVisitor()
: _is_file_output(false), file(NULL)
{
}

bool CodegenVisitor::is_file_output() const
{
    return _is_file_output;
}

void CodegenVisitor::set_is_file_output(bool b)
{
    _is_file_output = b;
}

std::string CodegenVisitor::codegen_to_str(const Nodecl::NodeclBase& n, TL::Scope sc)
{
    std::stringstream out;

    this->push_scope(sc);
    this->codegen(n, &out);
    this->pop_scope();

    return out.str();
}

void CodegenVisitor::codegen_top_level(const Nodecl::NodeclBase& n, FILE* f)
{
    this->set_is_file_output(true);
    this->push_scope( n.retrieve_context() );

    this->codegen_cleanup();

    int acc_mode = (::fcntl(fileno(f), F_GETFL) & O_ACCMODE);
    ERROR_CONDITION(acc_mode != O_WRONLY,
            "Invalid file descriptor: must be opened for write only", 0);

    // g++ extension
    __gnu_cxx::stdio_filebuf<char> filebuf(::fileno(f), std::ios::out);
    std::ostream out(&filebuf);

    this->codegen(n, &out);
    this->pop_scope();

    this->set_is_file_output(false);
}

CodegenVisitor::Ret CodegenVisitor::unhandled_node(const Nodecl::NodeclBase & n)
{ 
    internal_error("Unhandled node %s\n", ast_print_node_type(n.get_kind()));
    return Ret();
}

}
