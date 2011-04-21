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


#ifdef HAVE_CONFIG_H
 #include "config.h"
#endif

#include "cxx-utils.h"
#include "tl-lexer.hpp"

#include "c99-parser.h"
#include "cxx-parser.h"
#ifdef FORTRAN_SUPPORT
#include "fortran03-parser.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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

    extern YY_BUFFER_STATE mcxx_scan_string (const char *yy_str);
    extern void mcxx_switch_to_buffer (YY_BUFFER_STATE new_buffer);
    extern void mcxx_delete_buffer(YY_BUFFER_STATE b);
    extern int mcxxlex(void);

#ifdef FORTRAN_SUPPORT
    extern YY_BUFFER_STATE mf03_scan_string (const char *yy_str);
    extern void mf03_switch_to_buffer (YY_BUFFER_STATE new_buffer);
    extern void mf03_delete_buffer(YY_BUFFER_STATE b);
    extern int mf03lex(void);
#endif
}

namespace TL {

    class LexerImpl
    {
        public:
        virtual ObjectList<int> lex_string(const std::string& str) = 0;

        LexerImpl() { }
        virtual ~LexerImpl() { }
    };

    class LexerImpl_C : public LexerImpl
    {
        public:
        ObjectList<int> lex_string(const std::string& str)
        {
            ObjectList<int> result;

            char *line = ::strdup(str.c_str());
			YY_BUFFER_STATE scan_line = mc99_scan_string(line);
			mc99_switch_to_buffer(scan_line);

			int token = mc99lex();
			while (token != 0)
			{
                result.append(token);
                token = mc99lex();
            }

			mc99_delete_buffer(scan_line);

            ::free(line);

            return result;
        }
    };

    class LexerImpl_CXX : public LexerImpl
    {
        public:
        ObjectList<int> lex_string(const std::string& str)
        {
            ObjectList<int> result;

            char *line = ::strdup(str.c_str());
			YY_BUFFER_STATE scan_line = mcxx_scan_string(line);
			mcxx_switch_to_buffer(scan_line);

			int token = mcxxlex();
			while (token != 0)
			{
                result.append(token);
                token = mcxxlex();
            }

			mcxx_delete_buffer(scan_line);

            ::free(line);

            return result;
        }
    };

    class LexerImpl_Fortran : public LexerImpl
    {
        public:
        ObjectList<int> lex_string(const std::string& str)
        {
#ifndef FORTRAN_SUPPORT
            internal_error("Fortran not supported", 0);
            return ObjectList<int>();
#else
            ObjectList<int> result;

            char *line = ::strdup(str.c_str());
			YY_BUFFER_STATE scan_line = mf03_scan_string(line);
			mf03_switch_to_buffer(scan_line);

			int token = mf03lex();
			while (token != 0)
			{
                result.append(token);
                token = mf03lex();
            }

			mf03_delete_buffer(scan_line);

            ::free(line);

            return result;
#endif
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

    ObjectList<int> Lexer::lex_string(const std::string &str)
    {
        return _lexer->lex_string(str);
    }
}
