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




#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL
{
    namespace OpenMP
    {
        class LIBTL_CLASS UDRInfoItem : public TL::Object
        {
            private:
                // Symbol that refers to this info
                // It will be set to non-null by sign_in_scope
                Symbol _symbol_holder;

                std::string _name;
			    Type _type;

                Symbol _basic_function;
                Symbol _cleanup_function;

                bool _is_builtin;

                Nodecl::NodeclBase _identity;

            public:
                // Constructor
                UDRInfoItem();

                // Methods
                void sign_in_scope(Scope sc) const;

                static Nodecl::NodeclBase compute_nodecl_of_udr_name(
                        const std::string& reductor_name,
                        TL::Type udr_type,
                        const std::string& filename,
                        int line);

                // Factory
                static UDRInfoItem& get_udr_info_item_from_symbol_holder(TL::Symbol symbol);

                // Factory
                static UDRInfoItem lookup_udr(
                        Scope sc,
                        Nodecl::NodeclBase reductor_name_node,

                        bool &found);

                static std::string udr_get_symbol_name(
                        const std::string &reductor_name,
                        Type t);

                Nodecl::NodeclBase parse_omp_udr_operator_name(
                        ReferenceScope ref_scope,
                        const std::string &omp_udr_oper_name);

                TL::Symbol get_symbol_holder() const;

                // Getters, setters and consults
                std::string get_name() const;
                void set_name(const std::string& str);

                Type get_type() const;
                void set_type(Type t);

                bool is_builtin_operator() const;
                void set_is_builtin_operator(bool is_builtin);

                Nodecl::NodeclBase get_identity() const;
                void set_identity(Nodecl::NodeclBase identity);

                Symbol get_basic_reductor_function() const;
                void set_basic_reductor_function(Symbol sym);

                // FIXME - Vectorial version
                // Symbol get_basic_reductor_function() const;
                // void set_basic_reductor_function(Symbol sym);

                Symbol get_cleanup_function() const;
                void set_cleanup_function(Symbol sym);
        };

        void initialize_builtin_udr_reductions(TL::Scope global_scope);
        bool udr_is_builtin_operator(const std::string &op_name);
    }
}

#endif // TL_OMP_UDR_HPP
