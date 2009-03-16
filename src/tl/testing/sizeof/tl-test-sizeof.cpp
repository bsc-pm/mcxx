/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-test-sizeof.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-scope.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    void SizeofTest::run(DTO& dto)
    {
        // This phase is intended for C only and we will consider only
        // file-scope here

        TL::AST_t translation_unit = dto["translation_unit"];
        TL::ScopeLink scope_link = dto["scope_link"];

        TL::Scope sc = scope_link.get_scope(translation_unit);

        ObjectList<Symbol> list_of_symbols = sc.get_all_symbols(/* include_hidden */ false);

        int test_num = 0;
        for(ObjectList<Symbol>::iterator it = list_of_symbols.begin();
                it != list_of_symbols.end();
                it++)
        {
            Symbol &sym(*it);

            Type t = sym.get_type();

            if (sym.is_typename() 
                    && !t.is_dependent()
                    && !t.is_incomplete()
                    && !t.is_function())
            {
                std::cerr << "Symbol '" << sym.get_name() << "' is a typename. Generating test for it" << std::endl;

                Source test_src;
                Source evaluated_sizeof_src;
                evaluated_sizeof_src << "sizeof(" << sym.get_name() << ")"
                    ;

                AST_t expr_sizeof_tree = evaluated_sizeof_src.parse_expression(translation_unit, scope_link);
                Expression expr_sizeof(expr_sizeof_tree, scope_link);

                if (expr_sizeof.is_constant())
                {
                    bool valid = false;
                    int i = expr_sizeof.evaluate_constant_int_expression(valid);

                    if (valid)
                    {
                        Source src;
                        src << i;
                        evaluated_sizeof_src = src;
                    }
                }

                test_src
                    << "int _size_test_" << test_num << "[sizeof(" << sym.get_name() << ") == " << evaluated_sizeof_src << " ? 1 : -1];"
                    ;

                AST_t test_tree = test_src.parse_declaration(translation_unit,
                        scope_link);

                std::cerr << "Test is : " << test_tree.prettyprint() << std::endl;

                translation_unit.append_to_translation_unit(test_tree);

                test_num++;
            }

        }
    }
}

EXPORT_PHASE(TL::SizeofTest);
