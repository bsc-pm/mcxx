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

#ifndef TL_VERSIONING_HPP
#define TL_VERSIONING_HPP

#include <map>
#include <string>
#include "tl-symbol.hpp"
#include "tl-nodecl.hpp"


namespace TL
{
    template <class V>
    class Version
    {
        private:
            const V _value;

        public:
            Version(const V& value);
            
            virtual bool operator== (const Version& op) const;
            V get_value();
    };

    template < class K, class V>
    class Versioning
    {
        private:
            typedef std::map< K, Version<V> > versions_map_t;
            versions_map_t _versions;

        public:
            Versioning() : _versions() {}

            void add_version(const K& key, const Version<V>& value);

            /*
               Source get_pending_specific_functions(ReplaceSrcGenericFunction& replace);
               Source get_pending_specific_declarations(ReplaceSrcGenericFunction& replace);

               void add_specific_definition(
               const TL::Symbol& scalar_func_sym, 
               const specific_function_kind_t func_kind, 
               const std::string& device_name, 
               const int width, 
               const bool need_prettyprint,
               const bool need_def_decl,
               const std::string default_func_name = "");

               void add_specific_definition(
               const TL::Symbol& scalar_func_sym, 
               const TL::Symbol& simd_func_sym, 
               const specific_function_kind_t func_kind, 
               const std::string& device_name, 
               const int width, 
               const bool need_prettyprint,
               const bool need_def_decl,
               const std::string default_func_name = "");

               bool contains_generic_definition(
               const TL::Symbol& scalar_func_sym) const;
               bool contains_specific_definition(
               const TL::Symbol& scalar_func_sym, 
               const specific_function_kind_t spec_func_kind, 
               const std::string& device_name, 
               const int width) const;

               std::string get_specific_func_name(
               const TL::Symbol& scalar_func_sym,
               const std::string& device_name,
               const int width);
             */
    };
}

#endif //TL_VERSIONING_HPP


