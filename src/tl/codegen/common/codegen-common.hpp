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
        protected:
            std::stringstream file;
            virtual std::string codegen(const Nodecl::NodeclBase&) = 0;
        public:
            CodegenVisitor();

            bool is_file_output() const;

            std::string codegen_to_str(const Nodecl::NodeclBase& n, TL::Scope sc);

            void codegen_top_level(const Nodecl::NodeclBase& n, FILE* f);

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n);

            virtual void push_scope(TL::Scope sc) { }
            virtual void pop_scope() { }

            friend class CodegenModuleVisitor;
    };

    class CodegenModuleVisitor : public Nodecl::ModuleVisitor<void>
    {
        CodegenModuleVisitor(CodegenVisitor* codegen_visitor)
            : Nodecl::ModuleVisitor<void>(codegen_visitor),
            file(codegen_visitor->file)
        {
        }

        protected:
            std::stringstream& file;
    };
}

#endif // CODEGEN_COMMON_HPP
