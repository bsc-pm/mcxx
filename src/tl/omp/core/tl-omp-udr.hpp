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
            public:
                enum Associativity
                {
                    NONE = 0,
                    LEFT,
                    RIGHT,
                    UNDEFINED,
                };

            private:
                Associativity _assoc;

                bool _is_builtin;
                std::string _builtin_op;

                IdExpression _op_expr;

                ObjectList<Symbol> _op_symbols;

                Type _reduction_type;

                bool _is_template;

                bool _is_array;
                int _num_dimensions;

                bool _is_commutative;

                bool _has_identity;
                AST_t _identity;

                Symbol _deduction_function;
            public:
                UDRInfoItem();

                void set_associativity(Associativity);
                Associativity get_associativity() const;

                void set_builtin_operator(const std::string& str);
                bool is_builtin_operator() const;
                std::string get_builtin_operator() const;

                void set_operator(IdExpression id_expr);
                IdExpression get_operator() const;

                ObjectList<Symbol> get_operator_symbols() const;
                void set_operator_symbols(const ObjectList<Symbol>& sym_list);

                void set_reduction_type(Type t);
                Type get_reduction_type() const;

                void set_is_array_reduction(bool b);
                bool get_is_array_reduction() const;

                void set_num_dimensions(int n);
                int get_num_dimensions() const;

                void set_is_template_reduction(bool b);
                bool get_is_template_reduction() const;
                Symbol get_deduction_function();

                bool has_identity() const; 
                void set_identity(AST_t identity);
                AST_t get_identity() const;
                AST_t get_raw_identity() const;
                bool identity_is_constructor() const;

                bool get_is_commutative() const;
                void set_is_commutative(bool b);

                void sign_in_scope(Scope sc) const;

                // ----
                // ObjectList<Symbol> lookup_udr(Scope sc);
                UDRInfoItem lookup_udr(Scope sc, 
                        ScopeLink scope_link,
                        bool &found, ObjectList<Symbol> &all_viables, 
                        const std::string& filename, int line) const;
                // ----

                std::string get_symbol_name() const;
        };

        // Functions used in tl-omp-core.cpp
        // {
        void initialize_builtin_udr_reductions(Scope global_scope);
        bool udr_is_builtin_operator(const std::string &op_name);
        // }
    }
}

#endif // TL_OMP_UDR_HPP
