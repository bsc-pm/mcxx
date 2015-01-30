/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#ifndef TL_NANOS_HPP
#define TL_NANOS_HPP

#include <map>
#include <string>

#include "tl-pragmasupport.hpp"

namespace TL
{
    namespace Nanos
    {
        class Version
        {
            private:
                static std::map<std::string, int> _interfaces;
            public:
                const static int DEFAULT_VERSION;
                const static char* DEFAULT_FAMILY;

                static bool interface_has_family(const std::string &family);
                static bool interface_has_version(int version);

                static bool interface_is(const std::string &family, int version);
                static bool interface_is_at_least(const std::string &family, int version);
                static bool interface_is_range(const std::string &family, int lower, int upper);

                static int version_of_interface(const std::string& family);

                friend class Interface;
        };
        
        class Interface : public PragmaCustomCompilerPhase
        {
            private:
                typedef std::map<std::string, std::string> map_events;
                map_events _map_events;

                void reset_version_info();

                static bool _already_registered;
                
            public:
                Interface();
                void interface_preorder(TL::PragmaCustomDirective);
                void interface_postorder(TL::PragmaCustomDirective);

                void instrument_declare_pre(TL::PragmaCustomDirective);
                void instrument_declare_post(TL::PragmaCustomDirective);

                void instrument_emit_pre(TL::PragmaCustomDirective);
                void instrument_emit_post(TL::PragmaCustomDirective);

                virtual void run(TL::DTO& dto);
                virtual void phase_cleanup(DTO& dto);

                void walk(Nodecl::NodeclBase top_level);
                
                ~Interface() { }
        };
    }
}

#endif // TL_NANOS_HPP
