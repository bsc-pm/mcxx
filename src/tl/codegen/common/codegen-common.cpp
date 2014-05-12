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
: _is_file_output(false), _last_is_newline(true), _current_line(1), file(NULL)
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

// Inspired from an example in http://wordaligned.org/articles/cpp-streambufs
template <typename char_type,
          typename traits = std::char_traits<char_type> >
class CodegenStreambuf:
    public std::basic_streambuf<char_type, traits>
{
    public:
        typedef typename traits::int_type int_type;

        CodegenStreambuf(std::basic_streambuf<char_type, traits> * sb, CodegenVisitor* v)
            : _sb(sb), _v(v) { }

    private:
        virtual int_type overflow(int_type c)
        {
            if (c == '\n')
            {
                _v->set_current_line(_v->get_current_line() + 1);
            }
            _v->set_last_is_newline(c == '\n');
            return _sb->sputc(c);
        }

        virtual int sync()
        {
            return _sb->pubsync();
        }

    private:
        std::basic_streambuf<char_type, traits> * _sb;
        CodegenVisitor* _v;
};

void CodegenVisitor::codegen_top_level(const Nodecl::NodeclBase& n, FILE* f, const std::string& output_filename_)
{
    this->set_is_file_output(true);
    this->set_output_filename(output_filename_);
    this->push_scope( n.retrieve_context() );

    this->codegen_cleanup();

    int acc_mode = (::fcntl(fileno(f), F_GETFL) & O_ACCMODE);
    ERROR_CONDITION((acc_mode != O_WRONLY) && (acc_mode != O_RDWR),
            "Invalid file descriptor: must be opened for read/write or write", 0);

    // g++ extension
    __gnu_cxx::stdio_filebuf<char> filebuf(f, std::ios::out | std::ios::app);

    if (CURRENT_CONFIGURATION->line_markers)
    {
        CodegenStreambuf<char> codegen_streambuf(&filebuf, this);
        std::ostream out(&codegen_streambuf);

        this->codegen(n, &out);
    }
    else
    {
        std::ostream out(&filebuf);
        this->codegen(n, &out);
    }

    this->pop_scope();

    this->set_is_file_output(false);
}

CodegenVisitor::Ret CodegenVisitor::unhandled_node(const Nodecl::NodeclBase & n)
{ 
    internal_error("Unhandled node %s\n", ast_print_node_type(n.get_kind()));
    return Ret();
}

}
