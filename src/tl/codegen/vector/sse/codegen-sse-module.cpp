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

#include "codegen-sse-module.hpp"

namespace Codegen 
{
    SSEModuleVisitor::SSEModuleVisitor(CodegenVisitor* base_codegen) 
        : CodegenModuleVisitor(base_codegen)
    {
    }

    void SSEModuleVisitor::visit(const Nodecl::VectorAdd& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_add";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorMinus& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_sub";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_signed_int() ||
            type.is_unsigned_int()) 
        { 
            file << "_epi32"; 
        } 
        else if (type.is_signed_short_int() ||
            type.is_unsigned_short_int()) 
        { 
            file << "_epi16"; 
        } 
        else if (type.is_char() || 
            type.is_signed_char() ||
            type.is_unsigned_char()) 
        { 
            file << "_epi8"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }      

        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorAssignment& node) 
    { 
        walk(node.get_lhs());
        file << " = ";
        walk(node.get_rhs());
    }                                                 

    void SSEModuleVisitor::visit(const Nodecl::VectorLoad& node) 
    { 
        TL::Type type = node.get_type().basic_type();

        // Intrinsic name
        file << "_mm_load";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_integral_type()) 
        { 
            file << "_si128"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }
       
        file << "("; 
        walk(node.get_rhs());
        file << ")"; 
    }

    void SSEModuleVisitor::visit(const Nodecl::VectorStore& node) 
    { 
        TL::Type type = node.get_lhs().get_type().basic_type();

        // Intrinsic name
        file << "_mm_store";
        
        // Postfix
        if (type.is_float()) 
        { 
            file << "_ps"; 
        } 
        else if (type.is_double()) 
        { 
            file << "_pd"; 
        } 
        else if (type.is_integral_type()) 
        { 
            file << "_si128"; 
        } 
        else
        {
            running_error("SSE Codegen: Node %s at %s has an unsupported type.", 
                    ast_print_node_type(node.get_kind()),
                    node.get_locus().c_str());
        }
       
        file << "("; 
        walk(node.get_lhs());
        file << ", ";
        walk(node.get_rhs());
        file << ")"; 
    }

    Nodecl::NodeclVisitor<void>::Ret SSEModuleVisitor::unhandled_node(const Nodecl::NodeclBase& n) 
    { 
        std::cerr << "SSE Codegen: Unknown node " 
            << ast_print_node_type(n.get_kind()) 
            << " at " << n.get_locus() 
            << std::endl;

        return Ret(); 
    }
}
