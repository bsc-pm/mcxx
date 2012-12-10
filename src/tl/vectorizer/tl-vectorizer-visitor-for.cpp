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

#include "tl-vectorizer-visitor-for.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerVisitorFor::VectorizerVisitorFor(
                const std::string device,
                const unsigned int vector_length,
                const TL::Type& target_type) : 
            _device(device), _vector_length(vector_length), _target_type(target_type)
        {
        }

        Nodecl::NodeclBase VectorizerVisitorFor::visit(const Nodecl::ForStatement& for_statement)
        {
            Nodecl::ForStatement epilog;
            AnalysisStaticInfo for_analysis_info(for_statement);

            // TODO: ???
            analyze_loop(for_statement);


            if (_remain_iterations)
            {
                // Save original ForStatement as Epilog
                epilog = get_epilog(for_statement);
            }

            // Vectorizing Loop Header
            VectorizerVisitorLoopHeader visitor_loop_header(_vector_length);
            visitor_loop_header.walk(for_statement.get_loop_header());

            // Loop Body Vectorization      
            VectorizerVisitorStatement visitor_stmt(_device, 
                    _vector_length, 
                    _target_type,
                    for_statement.get_statement().retrieve_context());  
            visitor_stmt.walk(for_statement.get_statement());

            if (_remain_iterations)
            {
                return epilog;
            }

            return Nodecl::NodeclBase::null();
        }

        void VectorizerVisitorFor::analyze_loop(const Nodecl::ForStatement& for_statement) 
        {
            _remain_iterations = 2;
        }

        Nodecl::ForStatement VectorizerVisitorFor::get_epilog(const Nodecl::ForStatement& for_statement) 
        {
            Nodecl::ForStatement epilog = Nodecl::Utils::deep_copy(
                    for_statement, for_statement).as<Nodecl::ForStatement>();

            Nodecl::LoopControl loop_control = 
                epilog.get_loop_header().as<Nodecl::LoopControl>();

            loop_control.set_init(Nodecl::NodeclBase::null());

            return epilog;
        }


        Nodecl::NodeclVisitor<Nodecl::NodeclBase>::Ret VectorizerVisitorFor::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "For Visitor: Unknown node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }

        VectorizerVisitorLoopHeader::VectorizerVisitorLoopHeader(
                const unsigned int vector_length) : _vector_length(vector_length)
        {
        }

        void VectorizerVisitorLoopHeader::visit(const Nodecl::NodeclBase& loop_header)
        {
            ERROR_CONDITION(!loop_header.is<Nodecl::LoopControl>(), "LoopHeader is not a LoopControl", 0);

            Nodecl::LoopControl loop_control = loop_header.as<Nodecl::LoopControl>();
/*
            // Init
            VectorizerVisitorLoopInit visitor_loop_init();
            visitor_loop_init.walk(loop_control.get_init());

            // Cond
            VectorizerVisitorLoopCond visitor_loop_cond();
            visitor_loop_cond.walk(loop_control.get_cond());

            // Next
            VectorizerVisitorLoopNext visitor_loop_next();
            visitor_loop_next.walk(loop_control.get_next());
            */
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopHeader::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Loop Header Visitor: Unknown node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }

        VectorizerVisitorLoopInit::VectorizerVisitorLoopInit()
        {
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopInit::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Loop Init Visitor: Unknown node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }

        VectorizerVisitorLoopCond::VectorizerVisitorLoopCond(
                const unsigned int vector_length) : _vector_length(vector_length)
        {
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopCond::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Loop Cond Visitor: Unknown node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }

        VectorizerVisitorLoopNext::VectorizerVisitorLoopNext(
                const unsigned int vector_length) : _vector_length(vector_length)
        {
        }

        Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLoopNext::unhandled_node(const Nodecl::NodeclBase& n) 
        { 
            std::cerr << "Loop Next Visitor: Unknown node " 
                << ast_print_node_type(n.get_kind()) 
                << " at " << n.get_locus() 
                << std::endl;

            return Ret(); 
        }
    } 
}
