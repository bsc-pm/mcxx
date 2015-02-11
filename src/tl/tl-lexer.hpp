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



#ifndef TL_LEXER_HPP
#define TL_LEXER_HPP

#include "tl-objectlist.hpp"
#include "tl-lexer-tokens.hpp"

namespace TL
{
    class LexerImpl;
    class Lexer
    {
        private:
            enum lexer_kind
            {
                LEXER_INVALID = 0,
                LEXER_CURRENT,
                LEXER_C,
                LEXER_CXX,
                LEXER_FORTRAN
            };

            Lexer(lexer_kind);
            LexerImpl* _lexer;

        public:
            typedef std::pair<int, std::string> pair_token;

            static Lexer get_current_lexer();
            static Lexer get_c_lexer();
            static Lexer get_cxx_lexer();
            static Lexer get_fortran_lexer();

            ObjectList<pair_token> lex_string(const std::string& str);

            ~Lexer();
    };
}

#endif // TL_LEXER_HPP
