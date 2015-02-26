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



#ifndef PRESCANNER_IDENTIFIER_H
#define PRESCANNER_IDENTIFIER_H

#include "cxx-macros.h"
#include "libmf03-prescanner-common.h"
#include "prescanner-process.h"

MCXX_BEGIN_DECLS

enum language_level_t {
    LANG_TOP_LEVEL = 0,
    LANG_DECLARATION_PART = 1,
    LANG_INSTRUCTION_PART = 2
};

typedef enum language_level_t language_level;

LIBMF03_PRESCANNER_EXTERN language_level convert_line(prescanner_t* prescanner, language_level previous, char** line, int num_line);

enum statement_type_t {
    ST_ASSIGNMENT = 1,
    ST_TYPEDECL,
    ST_TYPESPEC,
    ST_INITIALIZATION,
    ST_PROGRAM,
    ST_SUBROUTINE,
    ST_FUNCTION,
    ST_DUBIOUS_FUNCTION,
    ST_MODULE,
    ST_MODULE_PROCEDURE,
    ST_BLOCKDATA,
    ST_USE,
    ST_IMPLICIT,
    ST_PARAMETER,
    ST_FORMAT,
    ST_ENTRY,
    ST_ACCESS,
    ST_ALLOCATABLE,
    ST_COMMON,
    ST_CONTAINS,
    ST_DATA,
    ST_DIMENSION,
    ST_EQUIVALENCE,
    ST_EXTERNAL,
    ST_INTENT,
    ST_INTRINSIC,
    ST_NAMELIST,
    ST_OPTIONAL,
    ST_POINTER,
    ST_SAVE,
    ST_VALUE,
    ST_VOLATILE,
    ST_TARGET,
    ST_DO,
    ST_LABELED_DO,
    ST_FORALL,
    ST_ARITHMETIC_IF,
    ST_IF,
    ST_IF_STMT,
    ST_WHERE,
    ST_ALLOCATE,
    ST_BACKSPACE,
    ST_BLOCK,
    ST_CALL,
    ST_CLOSE,
    ST_CONTINUE,
    ST_CYCLE,
    ST_DEALLOCATE,
    ST_ENDFILE,
    ST_EXIT,
    ST_GOTO,
    ST_LABEL_ASSIGN,
    ST_INQUIRE,
    ST_NULLIFY,
    ST_OPEN,
    ST_PRINT,
    ST_PRIVATE,
    ST_PUBLIC,
    ST_READ,
    ST_RETURN,
    ST_REWIND,
    ST_PAUSE,
    ST_STOP,
    ST_WRITE,
    ST_INTERFACE,
    ST_ELSE,
    ST_ELSEIF,
    ST_SELECTCASE,
    ST_CASE,
    ST_END,
    ST_BIND,
    DC_INCLUDE
};

typedef enum statement_type_t statement_type;

MCXX_END_DECLS

#endif // PRESCANNER_IDENTIFIER_H
