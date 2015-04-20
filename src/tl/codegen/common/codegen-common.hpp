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

#ifndef CODEGEN_COMMON_HPP
#define CODEGEN_COMMON_HPP

#include "tl-nodecl-visitor.hpp"
#include <string>
#include <cstdio>
#include <sstream>
#include <fstream>

namespace Codegen
{
    enum codegen_status_t
    {
        CODEGEN_STATUS_NONE = 0,
        CODEGEN_STATUS_DECLARED = 1,
        CODEGEN_STATUS_DEFINED = 2
    };

    class CodegenModuleVisitor;

    class CodegenVisitor : public Nodecl::ModularVisitor<void>
    {
        private:
            bool _is_file_output;
            bool _last_is_newline;
            int _current_line;
        protected:
            std::ostream *file;
            std::string output_filename;
            virtual void codegen(const Nodecl::NodeclBase&, std::ostream *out) = 0;
            virtual void codegen_cleanup() = 0;

        public:
            CodegenVisitor();

            void set_output_filename(const std::string& str) { output_filename = str; }
            std::string get_output_filename() const { return output_filename; }

            bool is_file_output() const;
            void set_is_file_output(bool b);

            void set_last_is_newline(bool b) { _last_is_newline = b; }
            bool last_is_newline() const { return _last_is_newline; }

            int get_current_line() const { return _current_line; }
            void set_current_line(int n) { _current_line = n; }

            void codegen_top_level(const Nodecl::NodeclBase& n, FILE* f, const std::string& output_filename);
            std::string codegen_to_str(const Nodecl::NodeclBase& n, TL::Scope sc);

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n);

            virtual void push_scope(TL::Scope sc) { }
            virtual void pop_scope() { }

            friend class CodegenModuleVisitor;
    };

    class CodegenModuleVisitor : public Nodecl::ModuleVisitor<void>
    {
        public:
            CodegenModuleVisitor(CodegenVisitor* codegen_visitor)
                : Nodecl::ModuleVisitor<void>(codegen_visitor),
                file(codegen_visitor->file)
        {
        }

        protected:
            std::ostream* file;
    };
}

#endif // CODEGEN_COMMON_HPP
