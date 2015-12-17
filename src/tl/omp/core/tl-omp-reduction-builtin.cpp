/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#include "tl-omp-core.hpp"
#include "tl-omp-reduction.hpp"
#include "cxx-diagnostic.h"
#include "cxx-buildscope.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"

namespace TL { namespace OpenMP {

    void Core::parse_declare_reduction(ReferenceScope ref_sc,
            const std::string &name,
            const std::string &typenames,
            const std::string &initializer,
            const std::string &combiner)
    {
        parse_declare_reduction(ref_sc,
                name + ":" + typenames + ":" + combiner + ":" + initializer,
                /* is_builtin */ false);
    }

    void Core::parse_builtin_reduction(ReferenceScope ref_sc,
            const std::string &name,
            const std::string &typenames,
            const std::string &initializer,
            const std::string &combiner)
    {

        parse_declare_reduction(ref_sc,
                "#line 0 \"<openmp-builtin-reductions>\"\n" +
                name + ":" + typenames + ":" + combiner + ":" + initializer,
                /* is_builtin */ true);
    }

    static std::string as_constant_expression(std::string symbol_name)
    {
        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        TL::Symbol sym = sc.get_symbol_from_name(symbol_name);

        ERROR_CONDITION(!sym.is_valid(), "Symbol is not valid", 0);

        Nodecl::NodeclBase value = sym.get_value();
        ERROR_CONDITION(value.is_null(), "The value of the symbol '%s' is null", sym.get_name().c_str(), 0);

        return as_expression(value.shallow_copy());
    }

    void Core::initialize_builtin_reductions(Scope sc)
    {
        if (_reductions_already_registered)
            return;
        _reductions_already_registered = true;

        _silent_declare_reduction = true;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            std::string integer_types =
                "int, unsigned int,"
                "char, signed char, unsigned char, "
                "short, unsigned short,"
                "long, unsigned long,"
                "long long, unsigned long long";

            std::string unsigned_integers =
                "unsigned int,"
                "unsigned char, "
                "unsigned short,"
                "unsigned long,"
                "unsigned long long";

            std::string arithmetic_types =
                "int, unsigned int,"
                "char, signed char, unsigned char, "
                "short, unsigned short,"
                "long, unsigned long,"
                "long long, unsigned long long,"
                "float, double, long double,"
                "_Complex float, _Complex double, _Complex long double";

            if (IS_C_LANGUAGE)
            {
                arithmetic_types += ", _Bool";
                integer_types += ", _Bool";
            }
            if (IS_CXX_LANGUAGE)
            {
                arithmetic_types += ", bool";
                integer_types += ", bool";
            }

            parse_builtin_reduction(sc,
                    "+",
                    arithmetic_types,
                    "omp_priv = 0",
                    "omp_out += omp_in");
            parse_builtin_reduction(sc,
                    "*",
                    arithmetic_types,
                    "omp_priv = 1",
                    "omp_out *= omp_in");
            parse_builtin_reduction(sc,
                    "-",
                    arithmetic_types,
                    "omp_priv = 0",
                    "omp_out += omp_in");
            parse_builtin_reduction(sc,
                    "&",
                    integer_types,
                    "omp_priv = ~0",
                    "omp_out &= omp_in");
            parse_builtin_reduction(sc,
                    "|",
                    integer_types,
                    "omp_priv = 0",
                    "omp_out |= omp_in");
            parse_builtin_reduction(sc,
                    "^",
                    integer_types,
                    "omp_priv = 0",
                    "omp_out ^= omp_in");
            parse_builtin_reduction(sc,
                    "&&",
                    arithmetic_types,
                    "omp_priv = 0",
                    "omp_out = omp_in && omp_out");
            parse_builtin_reduction(sc,
                    "||",
                    arithmetic_types,
                    "omp_priv = 0",
                    "omp_out = omp_in || omp_out");
            // max
            std::string max_combiner = "omp_out = omp_in > omp_out ? omp_in : omp_out";
            parse_builtin_reduction(sc,
                    "max",
                    unsigned_integers,
                    "omp_priv = 0",
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "signed char",
                    "omp_priv = " + as_constant_expression("mercurium_schar_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "signed short",
                    "omp_priv = " + as_constant_expression("mercurium_shrt_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "signed int",
                    "omp_priv = " + as_constant_expression("mercurium_int_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "signed long",
                    "omp_priv = " + as_constant_expression("mercurium_long_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "signed long long",
                    "omp_priv = " + as_constant_expression("mercurium_long_long_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "float",
                    "omp_priv = " + as_constant_expression("mercurium_flt_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "double",
                    "omp_priv = " + as_constant_expression("mercurium_dbl_min"),
                    max_combiner);
            parse_builtin_reduction(sc,
                    "max",
                    "long double",
                    "omp_priv = " + as_constant_expression("mercurium_ldbl_min"),
                    max_combiner);

            // min
            std::string min_combiner = "omp_out = omp_in < omp_out ? omp_in : omp_out";
            parse_builtin_reduction(sc,
                    "min",
                    "unsigned char",
                    "omp_priv = " + as_constant_expression("mercurium_uchar_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "unsigned short",
                    "omp_priv = " + as_constant_expression("mercurium_ushrt_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "unsigned int",
                    "omp_priv = " + as_constant_expression("mercurium_uint_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "unsigned long",
                    "omp_priv = " + as_constant_expression("mercurium_ulong_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "unsigned long long",
                    "omp_priv = " + as_constant_expression("mercurium_ulong_long_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "signed char",
                    "omp_priv = " + as_constant_expression("mercurium_schar_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "signed short",
                    "omp_priv = " + as_constant_expression("mercurium_shrt_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "signed int",
                    "omp_priv = " + as_constant_expression("mercurium_int_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "signed long",
                    "omp_priv = " + as_constant_expression("mercurium_long_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "signed long long",
                    "omp_priv = " + as_constant_expression("mercurium_long_long_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "float",
                    "omp_priv = " + as_constant_expression("mercurium_flt_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "double",
                    "omp_priv = " + as_constant_expression("mercurium_dbl_max"),
                    min_combiner);
            parse_builtin_reduction(sc,
                    "min",
                    "long double",
                    "omp_priv = " + as_constant_expression("mercurium_ldbl_max"),
                    min_combiner);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            std::string integer_types = "INTEGER(1), INTEGER(2), INTEGER(4), INTEGER(8)";
            std::string logical_types = "LOGICAL(1), LOGICAL(2), LOGICAL(4), LOGICAL(8)";
#ifdef HAVE_INT128
            integer_types += ", INTEGER(16)";
            logical_types += ", LOGICAL(16)";
#endif
            std::string floating_types = "REAL(4), REAL(8)";
            std::string complex_types = "COMPLEX(4), COMPLEX(8)";
#ifdef HAVE_QUADMATH_H
            floating_types += ", REAL(16)";
            complex_types += ", COMPLEX(16)";
#endif

            std::string arithmetic_types = integer_types + ", " + floating_types + ", " + complex_types;

            parse_builtin_reduction(sc,
                    "+",
                    arithmetic_types,
                    "omp_priv = 0",
                    "omp_out = omp_in + omp_out");
            parse_builtin_reduction(sc,
                    "*",
                    arithmetic_types,
                    "omp_priv = 1",
                    "omp_out = omp_in * omp_out");
            parse_builtin_reduction(sc,
                    "-",
                    arithmetic_types,
                    "omp_priv = 0",
                    "omp_out = omp_in - omp_out");
            parse_builtin_reduction(sc,
                    ".and.",
                    logical_types,
                    "omp_priv = .true.",
                    "omp_out = omp_in .and. omp_out");
            parse_builtin_reduction(sc,
                    ".or.",
                    logical_types,
                    "omp_priv = .false.",
                    "omp_out = omp_in .or. omp_out");
            parse_builtin_reduction(sc,
                    ".eqv.",
                    logical_types,
                    "omp_priv = .true.",
                    "omp_out = omp_in .eqv. omp_out");
            parse_builtin_reduction(sc,
                    ".neqv.",
                    logical_types,
                    "omp_priv = .false.",
                    "omp_out = omp_in .neqv. omp_out");
            parse_builtin_reduction(sc,
                    "iand",
                    integer_types,
                    "omp_priv = NOT(0)",
                    "omp_out = iand(omp_in, omp_out)");
            parse_builtin_reduction(sc,
                    "ior",
                    integer_types,
                    "omp_priv = 0",
                    "omp_out = ior(omp_in, omp_out)");
            parse_builtin_reduction(sc,
                    "ieor",
                    integer_types,
                    "omp_priv = 0",
                    "omp_out = ieor(omp_in, omp_out)");
            // max
            parse_builtin_reduction(sc,
                    "max",
                    integer_types,
                    "omp_priv = -HUGE(omp_priv)",
                    "omp_out = max(omp_in, omp_out)");
            parse_builtin_reduction(sc,
                    "max",
                    floating_types,
                    "omp_priv = -HUGE(omp_priv)",
                    "omp_out = max(omp_in, omp_out)");
            // min
            parse_builtin_reduction(sc,
                    "min",
                    integer_types,
                    "omp_priv = HUGE(omp_priv)",
                    "omp_out = min(omp_in, omp_out)");
            parse_builtin_reduction(sc,
                    "min",
                    floating_types,
                    "omp_priv = HUGE(omp_priv)",
                    "omp_out = min(omp_in, omp_out)");
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        _silent_declare_reduction = false;
    }

} }
