/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

        int Version::version(Version::DEFAULT_VERSION);
        std::string Version::family(Version::DEFAULT_FAMILY);

        bool Version::is_family(const std::string &_fam)
        {
            return _fam == Version::family;
        }

        bool Version::is_version(int _ver)
        {
            return _ver == Version::version;
        }

        bool Version::is_interface(const std::string &_fam, int _ver)
        {
            return is_family(_fam) && is_version(_ver);
        }

        Interface::Interface()
            : PragmaCustomCompilerPhase("nanos")
        {
            set_phase_name("Nanos Runtime Source-Compiler Versioning Interface");
            set_phase_description("This phase enables support for '#pragma nanos', the interface for versioning runtime and compiler for Nanos");

            register_directive("interface");
            on_directive_pre["interface"].connect(functor(&Interface::interface_preorder, *this));
            on_directive_post["interface"].connect(functor(&Interface::interface_postorder, *this));
        }

        void Interface::run(TL::DTO& dto)
        {
            // Run looking up for every "#pragma nanos"
            PragmaCustomCompilerPhase::run(dto);
            
            // Create versioning symbols
            Source versioning_symbols;

            DEBUG_CODE()
            {
                std::cerr << "Version::family '" << Version::family << "'" << std::endl;
                std::cerr << "Version::version '" << Version::version << "'" << std::endl;
            }

            CXX_LANGUAGE()
            {
                versioning_symbols
                    << "extern \"C\" { "
                    ;
            }

            versioning_symbols
                << "const char* __nanos_family __attribute__((weak)) = \"" << Version::family << "\";"
                << "int __nanos_version __attribute__((weak)) = " << Version::version << ";"
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

        void Interface::interface_preorder(PragmaCustomConstruct construct)
        {
            PragmaCustomClause version_clause = construct.get_clause("version");
            PragmaCustomClause family_clause = construct.get_clause("family");

            if (version_clause.is_defined()
                    && !version_clause.get_arguments(ExpressionTokenizer()).empty())
            {
                // Convert into an integer
                std::stringstream ss;

                ss << version_clause.get_arguments(ExpressionTokenizer())[0];
                ss >> Version::version;
            }

            if (family_clause.is_defined()
                    && !family_clause.get_arguments(ExpressionTokenizer()).empty())
            {
                Version::family = family_clause.get_arguments(ExpressionTokenizer())[0];
            }
        }

        void Interface::interface_postorder(PragmaCustomConstruct construct)
        {
            construct.get_ast().remove_in_list();
        }
    }
}

EXPORT_PHASE(TL::Nanos::Interface);
