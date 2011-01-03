/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "cxx-utils.h"

#include "tl-nanos.hpp"

#include "tl-ast.hpp"

#include <sstream>

namespace TL
{
    namespace Nanos
    {
        // Definition of static members
        const int Version::DEFAULT_VERSION = 399;
        const char* Version::DEFAULT_FAMILY = "trunk";
        std::map<std::string, int> Version::_interfaces;

        bool Version::interface_has_family(const std::string &fam)
        {
            if (Version::_interfaces.find(fam) != Version::_interfaces.end())
                return true;
            return false;
        }

        bool Version::interface_has_version(int ver)
        {
            for(std::map<std::string, int>::iterator it=_interfaces.begin();
                    it != _interfaces.end();
                    it++)
            {
                if (it->second == ver)
                    return true;
            }
            
            return false;
        }

        bool Version::interface_is(const std::string &fam, int ver)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                if (it->second == ver)
                    return true;
            }

            return false;
        }

        bool Version::interface_is_at_least(const std::string &fam, int ver)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                if (it->second >= ver)
                    return true;
            }

            return false;
        }

        bool Version::interface_is_range(const std::string &fam, int ver_lower, int ver_upper)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                if ((it->second <= ver_lower)
                        && (it->second <= ver_upper))
                    return true;
            }

            return false;
        }

        int Version::version_of_interface(const std::string& fam)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                return it->second;
            }
            return -1;
        }

        Interface::Interface()
            : PragmaCustomCompilerPhase("nanos")
        {
            _n_loads = 0;
            Version::_interfaces[Version::DEFAULT_FAMILY] = Version::DEFAULT_VERSION;
            
            set_phase_name("Nanos Runtime Source-Compiler Versioning Interface");
            set_phase_description("This phase enables support for '#pragma nanos', the interface for versioning runtime and compiler for Nanos");

            register_directive("interface");
            on_directive_pre["interface"].connect(functor(&Interface::interface_preorder, *this));
            on_directive_post["interface"].connect(functor(&Interface::interface_postorder, *this));
        }

        void Interface::run(TL::DTO& dto)
        {
            _n_loads++;
            // Run looking up for every "#pragma nanos"
            PragmaCustomCompilerPhase::run(dto);
            
            // Create versioning symbols
            Source versioning_symbols;

            DEBUG_CODE()
            {
                for(std::map<std::string, int>::iterator it = Version::_interfaces.begin();
                        it != Version::_interfaces.end();
                        it++)
                    std::cerr << "Interface =>  Version::family '" << it->first << "'" 
                              << ", Version::version '" << it->second << "'" << std::endl;
            }

            CXX_LANGUAGE()
            {
                versioning_symbols
                    << "extern \"C\" { "
                    ;
            }

            // Code to maintain the Nanos4 version
            versioning_symbols
                << "const char* __nanos_family __attribute__((weak)) = \"" << Version::_interfaces.begin()->first << "\";"
                << "int __nanos_version __attribute__((weak)) = " << Version::_interfaces.begin()->second << ";"
            ;
            
            // Code for Nanox version
            for(std::map<std::string, int>::iterator it = Version::_interfaces.begin();
                    it != Version::_interfaces.end();
                    it++)
                versioning_symbols
                    << "int __mcc_" << it->first << " __attribute__((weak)) = " << it->second << ";"
                    ;
                
            CXX_LANGUAGE()
            {
                versioning_symbols
                    << "}"
                    ;
            }

            AST_t translation_unit = dto["translation_unit"];
            ScopeLink scope_link = dto["scope_link"];

            AST_t versioning_symbols_tree = versioning_symbols.parse_global(translation_unit,
                    scope_link);
                    
            // Get the translation_unit tree
            // and prepend these declarations
            translation_unit.prepend_to_translation_unit(versioning_symbols_tree);
        }

        void Interface::phase_cleanup(DTO& dto)
        {
            _n_loads = 0;
        }

        void Interface::interface_preorder(PragmaCustomConstruct construct)
        {
            PragmaCustomClause version_clause = construct.get_clause("version");
            PragmaCustomClause family_clause = construct.get_clause("family");
            
            // The runtime must provide always a pair of Family/Version, never only one of them
            if (family_clause.is_defined()
                && !family_clause.get_arguments(ExpressionTokenizer()).empty()
                && version_clause.is_defined()
                && !version_clause.get_arguments(ExpressionTokenizer()).empty())
            {
                std::string new_family = family_clause.get_arguments(ExpressionTokenizer())[0];
                if (Version::_interfaces.find(new_family) != Version::_interfaces.end()
                    && (new_family != Version::DEFAULT_FAMILY
                    || Version::_interfaces[new_family] != Version::DEFAULT_VERSION))
                {
                    std::stringstream ss;
                    ss << Version::_interfaces[family_clause.get_arguments(ExpressionTokenizer())[0]];
                    running_error("error: Nanos family %s previously defined with version %s\n",
                                  family_clause.get_arguments(ExpressionTokenizer())[0].c_str(), 
                                  ss.str().c_str());
                }
                else
                {
                    // If it is the first load of the phase, remove the default pair of Family/Version from the hash
                    if (_n_loads==1)
                        if (Version::_interfaces.find("trunk") != Version::_interfaces.end())
                            Version::_interfaces.erase(Version::_interfaces.find("trunk"));
                    Version::_interfaces[family_clause.get_arguments(ExpressionTokenizer())[0]] 
                            = atoi(version_clause.get_arguments(ExpressionTokenizer())[0].c_str());                
                }
            }
            else
            {
                running_error("error: Both, family and version must be provided by the runtime.\n");
            }
        }

        void Interface::interface_postorder(PragmaCustomConstruct construct)
        {
            construct.get_ast().remove_in_list();
        }
    }
}

EXPORT_PHASE(TL::Nanos::Interface);
