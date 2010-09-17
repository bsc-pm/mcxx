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

#ifndef TL_OMP_UDR_2_HPP
#define TL_OMP_UDR_2_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL
{
    namespace OpenMP
    {
        class LIBTL_CLASS UDRInfoItem2 : public TL::Object
        {

            private:
                std::string _name;
			    Type _type;
			    AST_t _combine_expression;
			    Symbol _in_symbol;
			    Symbol _out_symbol;

                bool _is_builtin;

                bool _has_identity;
                AST_t _identity;
                bool _need_equal_initializer;
                bool _is_constructor;

                Symbol _function_definition_symbol;

            public:
                // Constructor
                UDRInfoItem2();

                // Methods
                void sign_in_scope(Scope sc, Type types) const;

                UDRInfoItem2 lookup_udr(Scope sc,
                        bool &found,
                        Type udr_type,
                        AST_t reductor_tree,
                        int udr_counter) const;

                std::string get_symbol_name(Type t) const;

                AST_t parse_omp_udr_operator_name(const std::string &omp_udr_oper_name, 
                        AST_t ref_tree,
                        ScopeLink sl);

                // Getters, setters and consults
                std::string get_name() const;
                void set_name(const std::string& str);

                Type get_type() const;
                void set_type(Type t);

                AST_t get_combine_expr() const;
                void set_combine_expr(AST_t combine_expr);

                Symbol get_in_symbol() const;
                void set_in_symbol(Symbol s);
                Symbol get_out_symbol() const;
                void set_out_symbol(Symbol s);

                bool is_builtin_operator() const;
                void set_is_builtin_operator(bool is_builtin);
                bool udr_is_builtin_operator_2(const std::string& op_name);

                bool get_is_constructor() const;
                void set_is_constructor(bool constructor);
                bool get_need_equal_initializer() const;
                void set_need_equal_initializer(bool need_equal_init);
                bool has_identity() const; 
                AST_t get_identity() const;
                AST_t get_raw_identity() const;
                void set_identity(AST_t identity);
                bool identity_is_constructor() const;

                Symbol get_function_definition_symbol() const;
                void set_function_definition_symbol(Symbol sym);
        };

        // Functions used in tl-omp-core.cpp
        // {
        void initialize_builtin_udr_reductions_2(AST_t translation_unit, ScopeLink scope_link);
        bool udr_is_builtin_operator_2(const std::string &op_name);
        // }
    }
}

#endif // TL_OMP_UDR_2_HPP
