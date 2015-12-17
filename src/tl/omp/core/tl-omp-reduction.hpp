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




#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL { namespace OpenMP {

    class LIBTL_CLASS Reduction : public TL::Object
    {
        private:
            //! Scope where this reduction is valid
            TL::Scope _scope;

            //! Artificial scope created to give context to the expressions
            TL::Scope _expr_scope;

            //! Name of the reduction. It will be an identifier or a base operator
            std::string _name;
            //! Type of the reduction (non cv-qualified)
            Type _type;

            /*! 
              Expression that combines omp_out and omp_in to create a new
              value to be stored in omp_out
              */
            Nodecl::NodeclBase _combiner;
            /*! 
              Expression that initializes the reduction using omp_priv (and possibly omp_orig)
              */
            Nodecl::NodeclBase _initializer;

            TL::Symbol _omp_in;
            TL::Symbol _omp_out;
            TL::Symbol _omp_priv;
            TL::Symbol _omp_orig;

            const locus_t* _locus;

            // This boolean will be true if the initializer was an
            // AST_INIT_DECLARATOR node. Otherwise it will be false.
            bool _is_initialization;

            TL::Symbol _symbol;

            // Constructor
            Reduction(TL::Scope, const std::string& name, TL::Type t);

            // Symbol holding us
            void set_symbol(TL::Symbol sym)
            {
                _symbol = sym;
            }

            static ObjectList<Reduction*> lookup(TL::Scope, Nodecl::NodeclBase id_expression, TL::Type t,
                    bool disable_koenig, bool allow_array_types);

            bool _is_builtin;
        public:
            //! Factory of a reduction in a given scope
            /*!
             * Returns NULL if the reduction has already been declared in the scope
             */
            static Reduction* new_reduction(TL::Scope, const std::string& name, TL::Type t);

            //! Lookup of a reduction
            /*!
             * Returns NULL if no reduction has been found
             */
            static Reduction* simple_lookup(TL::Scope, const std::string& name,
                    TL::Type t, bool allow_array_types);

            //! Lookup of a reduction using a nodecl name
            /*!
             * May return more than one matching reduction
             */
            static ObjectList<Reduction*> lookup(TL::Scope, Nodecl::NodeclBase id_expression,
                    TL::Type t, bool allow_array_types);

            static Reduction* get_reduction_info_from_symbol(TL::Symbol sym);

            //! Returns the symbol representing the OpenMP reduction
            TL::Symbol get_symbol() const
            {
                return _symbol;
            }

            //! Returns the type of the reduction
            TL::Type get_type() const
            {
                return _type;
            }

            // Special symbols
            TL::Symbol get_omp_in() const
            {
                return _omp_in;
            }

            TL::Symbol get_omp_out() const
            {
                return _omp_out;
            }

            TL::Symbol get_omp_priv() const
            {
                return _omp_priv;
            }

            TL::Symbol get_omp_orig() const
            {
                return _omp_orig;
            }

            TL::Scope get_scope() const
            {
                return _scope;
            }

            TL::Scope get_expressions_scope() const
            {
                return _expr_scope;
            }

            // Name
            std::string get_name() const
            {
                return _name;
            }

            // Initializer
            void set_initializer(Nodecl::NodeclBase n)
            {
                _initializer = n;
            }

            Nodecl::NodeclBase get_initializer() const
            {
                return _initializer;
            }

            // Combiner
            void set_combiner(Nodecl::NodeclBase n)
            {
                _combiner = n;
            }

            Nodecl::NodeclBase get_combiner() const
            {
                return _combiner;
            }

            void set_locus(const locus_t* locus)
            {
                _locus = locus;
            }

            const locus_t* get_locus() const
            {
                return _locus;
            }

            std::string get_locus_str() const
            {
                return ::locus_to_str(_locus);
            }

            void set_is_builtin(bool b)
            {
                this->_is_builtin = b;
            }

            bool is_builtin() const
            {
                return this->_is_builtin;
            }

            bool get_is_initialization() const
            {
                return _is_initialization;
            }

            void set_is_initialization(bool b)
            {
                _is_initialization = b;
            }
    };

} }

#endif // TL_OMP_UDR_HPP

