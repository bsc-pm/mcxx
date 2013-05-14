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




#include "hlt-stripmine.hpp"

namespace TL
{
    namespace HLT
    {
        StripMine::StripMine(ForStatement for_stmt, Source amount)
            : _for_stmt(for_stmt), _amount(amount)
        {
        }

        Source StripMine::get_source()
        {
            if (!_for_stmt.is_regular_loop())
            {
                return _for_stmt.prettyprint();
            }
            else
            {
                IdExpression induction_var = _for_stmt.get_induction_variable();
                Expression lower_bound = _for_stmt.get_lower_bound();
                Expression upper_bound = _for_stmt.get_upper_bound();
                Expression step = _for_stmt.get_step();
                TL::Source operator_bound = _for_stmt.get_bound_operator();

                Symbol induction_var_sym = induction_var.get_symbol();
                Type induction_var_type = induction_var_sym.get_type();

                Source result;

                Source new_var_name;
                new_var_name << "__" << induction_var_sym.get_name();

                Source strip_loop_header, by_strip_loop;

                result 
                    << strip_loop_header
                    << "{"
                    << by_strip_loop
                    << "}"
                    ;

                strip_loop_header
                    << "for(" 
                    << induction_var_type.get_declaration(_for_stmt.get_scope(), new_var_name) << "=" << lower_bound << ";" 
                    << new_var_name << operator_bound << upper_bound << ";"
                    << new_var_name << "+= ( ( " << step << ")*(" << _amount << ")))"
                    ;

                Source by_strip_loop_header, by_strip_body;
                by_strip_loop
                    << by_strip_loop_header
                    << by_strip_body;

                Source min_bound, min_a, min_b;

                min_a << "(" << upper_bound << ")";
                min_b << "(" << new_var_name << "+" << step << "*((" << _amount << ") -1))";

                min_bound
                    << "(" << min_a << "<" << min_b << "?" << min_a << ":" << min_b << ")"
                    ;

                Source induction_var_init;

                by_strip_loop_header 
                    << "for("
                    << induction_var_init << "=" << new_var_name << ";"
                    << induction_var << operator_bound << min_bound << ";"
                    << induction_var << "+=" << step
                    << ")"
                    ;

                AST_t iterating_init = _for_stmt.get_iterating_init();

                if (Expression::predicate(iterating_init))
                {
                    induction_var_init << induction_var;
                }
                else
                {
                    induction_var_init
                        << induction_var_sym.get_type().get_declaration(
                                _for_stmt.get_scope(), 
                                induction_var_sym.get_name())
                        ;
                }

                by_strip_body 
                    << _for_stmt.get_loop_body();

                return result;
            }
        }
    }

    HLT::StripMine HLT::stripmine_loop(ForStatement for_stmt, Source amount)
    {
        return HLT::StripMine(for_stmt, amount);
    }

}
