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



#ifdef HAVE_CONFIG_H
 #include "config.h"
#endif

#include "cxx-utils.h"
#include "tl-lexer.hpp"

#include "c99-parser.h"
#include "cxx-parser.h"
#include "cxx-lexer.h"
#include "fortran03-parser.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "mem.h"

/* 
   Ugly hacks because of the unavailable modularization of Flex
   */

extern "C"
{

    typedef void* YY_BUFFER_STATE;

    extern YY_BUFFER_STATE mc99_scan_string (const char *yy_str);
    extern void mc99_switch_to_buffer (YY_BUFFER_STATE new_buffer);
    extern void mc99_delete_buffer(YY_BUFFER_STATE b);
    extern int mc99lex(void);

    extern token_atrib_t mc99lval;

    extern YY_BUFFER_STATE mcxx_scan_string (const char *yy_str);
    extern void mcxx_switch_to_buffer (YY_BUFFER_STATE new_buffer);
    extern void mcxx_delete_buffer(YY_BUFFER_STATE b);
    extern int mcxxlex(void);

    extern token_atrib_t mcxxlval;

#ifdef FORTRAN_NEW_SCANNER
    extern int mf03_prepare_string_for_scanning(const char* str);
#else
    extern YY_BUFFER_STATE mf03_scan_string (const char *yy_str);
    extern void mf03_switch_to_buffer (YY_BUFFER_STATE new_buffer);
    extern void mf03_delete_buffer(YY_BUFFER_STATE b);
#endif
    extern int mf03lex(void);

    extern token_atrib_t mf03lval;
}

namespace TL {

    class LexerImpl
    {
        public:
        virtual ObjectList<Lexer::pair_token> lex_string(const std::string& str) = 0;

        LexerImpl() { }
        virtual ~LexerImpl() { }
    };

    class LexerImpl_C : public LexerImpl
    {
        public:
        ObjectList<Lexer::pair_token> lex_string(const std::string& str)
        {
            ObjectList<Lexer::pair_token> result;

            char *line = ::xstrdup(str.c_str());
			YY_BUFFER_STATE scan_line = mc99_scan_string(line);
			mc99_switch_to_buffer(scan_line);

			int token = mc99lex();
			while (token != 0)
			{
                std::string token_text;
                if (mc99lval.token_text != NULL)
                    token_text = mc99lval.token_text;

                result.append(Lexer::pair_token(token, token_text));
                token = mc99lex();
            }

			mc99_delete_buffer(scan_line);

            DELETE(line);

            return result;
        }
    };

    class LexerImpl_CXX : public LexerImpl
    {
        public:
        ObjectList<Lexer::pair_token> lex_string(const std::string& str)
        {
            ObjectList<Lexer::pair_token> result;

            char *line = ::xstrdup(str.c_str());
			YY_BUFFER_STATE scan_line = mcxx_scan_string(line);
			mcxx_switch_to_buffer(scan_line);

			int token = mcxxlex();
			while (token != 0)
			{
                std::string token_text;
                if (mcxxlval.token_text != NULL)
                    token_text = mcxxlval.token_text;

                result.append(Lexer::pair_token(token, token_text));
                token = mcxxlex();
            }

			mcxx_delete_buffer(scan_line);

            DELETE(line);

            return result;
        }
    };

    class LexerImpl_Fortran : public LexerImpl
    {
        public:
        ObjectList<Lexer::pair_token> lex_string(const std::string& str)
        {
            ObjectList<Lexer::pair_token> result;

            char *line = ::xstrdup(str.c_str());
#ifdef FORTRAN_NEW_SCANNER
            mf03_prepare_string_for_scanning(line);
#else
			YY_BUFFER_STATE scan_line = mf03_scan_string(line);
			mf03_switch_to_buffer(scan_line);
#endif

			int token = mf03lex();
			while (token != 0)
			{
                std::string token_text;
                if (mf03lval.token_text != NULL)
                    token_text = mf03lval.token_text;

                result.append(Lexer::pair_token(token, token_text));
                token = mf03lex();
            }

#ifdef FORTRAN_NEW_SCANNER
#else
			mf03_delete_buffer(scan_line);
#endif

            DELETE(line);

            return result;
        }
    };

    Lexer Lexer::get_current_lexer()
    {
        return Lexer(LEXER_CURRENT);
    }

    Lexer Lexer::get_c_lexer()
    {
        return Lexer(LEXER_C);
    }

    Lexer Lexer::get_cxx_lexer()
    {
        return Lexer(LEXER_CXX);
    }

    Lexer Lexer::get_fortran_lexer()
    {
        return Lexer(LEXER_FORTRAN);
    }

    Lexer::Lexer(lexer_kind kind)
        : _lexer(0)
    {
        if (kind == LEXER_CURRENT)
        {
            if (IS_C_LANGUAGE)
            {
                kind = LEXER_C;
            }
            else if (IS_CXX_LANGUAGE)
            {
                kind = LEXER_CXX;
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                kind = LEXER_FORTRAN;
            }
            else
            {
                kind = LEXER_INVALID;
            }
        }
        switch ((int)kind)
        {
            case LEXER_C:
                {
                    _lexer = new LexerImpl_C();
                    break;
                }
            case LEXER_CXX:
                {
                    _lexer = new LexerImpl_CXX();
                    break;
                }
            case LEXER_FORTRAN:
                {
                    _lexer = new LexerImpl_Fortran();
                    break;
                }
            default:
                {
                    internal_error("Invalid lexer kind %d", (int) kind);
                }
        }
    }

    Lexer::~Lexer()
    {
        delete _lexer;
    }

    ObjectList<Lexer::Lexer::pair_token> Lexer::lex_string(const std::string &str)
    {
        return _lexer->lex_string(str);
    }
}
