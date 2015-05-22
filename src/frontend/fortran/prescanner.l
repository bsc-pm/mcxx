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


%{

#include "prescanner-identifier.h"

// #define YY_DECL int yylex()

static int inside_interface = 0;

%}

%option noyyalloc noyyrealloc noyyfree noinput nounput

INTEGER integer
REAL real
DOUBLEPRECISION doubleprecision
COMPLEX complex
DOUBLECOMPLEX doublecomplex
LOGICAL logical
CHARACTER character
DERIVED_TYPE {TYPE}"("{IDENTIFIER}")"
TYPE type
IDENTIFIER [[:alpha:]]([[:alnum:]_])*
INCLUDE include

TYPE_SPEC {TYPE_SPEC1}|{TYPE_SPEC2}|{DERIVED_TYPE}
TYPE_SPEC1 ({INTEGER}|{REAL}|{DOUBLECOMPLEX}|{DOUBLEPRECISION}|{COMPLEX}|{LOGICAL}){KIND_SELECTOR}?
TYPE_SPEC2 {CHARACTER}({CHAR_SELECTOR})?

KIND_SELECTOR {KIND_SELECTOR1}|{KIND_SELECTOR2}
KIND_SELECTOR1 ("("(KIND=)?(([0-9]+)|{IDENTIFIER})")")
KIND_SELECTOR2 ([*][0-9]+)

CHAR_SELECTOR {LENGTH_SELECTOR}|{CHAR_SELECTOR1}|{CHAR_SELECTOR2}
CHAR_SELECTOR1 "("LEN=[^,]+,(KIND=)?.+")"
CHAR_SELECTOR2 "("KIND=.+(,LEN=.+)?")"
LENGTH_SELECTOR ("("LEN=.+")")|("*"{CHAR_LENGTH})
CHAR_LENGTH ("(".+")")|([+-]?[[:digit:]]+)

PROGRAM program
SUBROUTINE subroutine
FUNCTION function
PROCEDURE procedure
MODULE module
BLOCK block
BLOCKDATA blockdata
USE use
IMPLICIT implicit
ENTRY entry
ACCESS access
ALLOCATABLE allocatable
COMMON common
DATA data
DIMENSION dimension
EXTERNAL external
INTRINSIC intrinsic
OPTIONAL optional
POINTER pointer
SAVE save
VALUE value
VOLATILE volatile
TARGET target
DO do
CALL call
BACKSPACE backspace
CYCLE cycle
ENDFILE endfile
EXIT exit
GOTO goto
INQUIRE inquire
NULLIFY nullify
PRINT print
PRIVATE private
PUBLIC public
RETURN return
REWIND rewind
STOP stop
PAUSE pause
INTERFACE interface
PURE pure
RECURSIVE recursive
ELEMENTAL elemental
ASSIGN assign
CONTAINS contains

SUBPROGRAM_SPEC ({PURE}|{RECURSIVE}|{ELEMENTAL})+

RESULT result

PARAMETER parameter
FORMAT format
INTENT intent
NAMELIST namelist
EQUIVALENCE equivalence
FORALL forall
IF if
BIND bind
ELSE else
ELSEIF elseif
SELECTCASE selectcase
CASE case
WHERE where
ALLOCATE allocate
CONTINUE continue
DEALLOCATE deallocate
OPEN open
READ read
WRITE write
END end
SELECT select
CLOSE close
THEN then


%x PREFIX_SPEC
%x END_APPENDINGS
%x MODULE_PROC
%x OMP_DIRECTIVE
%x OMP_CLAUSE
%x OMP_EXPR

%%

^{PROGRAM} {
    return (ST_PROGRAM);
}

^({PURE}|{RECURSIVE}|{ELEMENTAL})*{SUBROUTINE}{IDENTIFIER}("("(({IDENTIFIER},)*{IDENTIFIER})?")")? {
    return (ST_SUBROUTINE);
}

^.*{FUNCTION}{IDENTIFIER}"("(({IDENTIFIER},)*{IDENTIFIER})?")"{RESULT}"("{IDENTIFIER}")" {
    return (ST_FUNCTION);
}

^.*{FUNCTION}{IDENTIFIER}"("(({IDENTIFIER},)*{IDENTIFIER})?")" {
    return (ST_DUBIOUS_FUNCTION);
}

^{REAL}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{INTEGER}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{LOGICAL}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{COMPLEX}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{DOUBLECOMPLEX}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{DOUBLEPRECISION}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{CHARACTER}.*{FUNCTION}{IDENTIFIER} {
    return (ST_DUBIOUS_FUNCTION);
}

^{REAL}([*][0-9]+)? {
    return (ST_TYPESPEC);
}

^{INTEGER}([*][0-9]+)? {
    return (ST_TYPESPEC);
}

^{LOGICAL}([*][0-9]+)? {
    return (ST_TYPESPEC);
}

^{COMPLEX}([*][0-9]+)? {
    return (ST_TYPESPEC);
}

^{DOUBLECOMPLEX} {
    return (ST_TYPESPEC);
}

^{DOUBLEPRECISION}([*][0-9]+)? {
    return (ST_TYPESPEC);
}

^{CHARACTER}([*][0-9]+)? {
    return (ST_TYPESPEC);
}

^{DERIVED_TYPE} {
    return (ST_TYPESPEC);
}

^{TYPE} {
    // This is a derived type declaration not a 
    // TYPE(mytype)
    return (ST_TYPEDECL);
}


^{MODULE}{PROCEDURE} {
    if (inside_interface)
    {
        return (ST_MODULE_PROCEDURE);
    }
    else
    {
        // We fall back to usual MODULEidentifier
        REJECT;
    }
}

^{MODULE} {
    return (ST_MODULE);
}

^{INTERFACE} {
    inside_interface = 1;
    return (ST_INTERFACE);
}

^{BLOCK} {
    return (ST_BLOCK);
}

^{BLOCKDATA} {
    return (ST_BLOCKDATA);
}

^{USE} {
    return (ST_USE);
}

^{IMPLICIT} {
    return (ST_IMPLICIT);
}

^{PARAMETER} {
    return (ST_PARAMETER);
}

^{ENTRY} {
    return (ST_ENTRY);
}

^{ACCESS} {
    return (ST_ACCESS);
}

^{ALLOCATABLE} {
    return (ST_ALLOCATABLE);
}

^{COMMON} {
    return (ST_COMMON);
}

^{DATA} {
    return (ST_DATA);
}

^{DIMENSION} {
    return (ST_DIMENSION);
}

^{EXTERNAL} {
    return (ST_EXTERNAL);
}

^{INTRINSIC} {
    return (ST_INTRINSIC);
}

^{OPTIONAL} {
    return (ST_OPTIONAL);
}

^{POINTER} {
    return (ST_POINTER);
}

^{VALUE} {
    return (ST_VALUE);
}

^{VOLATILE} {
    return (ST_VOLATILE);
}

^{SAVE} {
    return (ST_SAVE);
}

^{TARGET} {
    return (ST_TARGET);
}

^({IDENTIFIER}:)?{DO}[0-9]+ {
    return (ST_LABELED_DO);
}

^({IDENTIFIER}:)?{DO} {
    return (ST_DO);
}

^{BACKSPACE} {
    return (ST_BACKSPACE);
}

^{CALL} {
    return (ST_CALL);
}

^{CYCLE} {
    return (ST_CYCLE);
}

^{ENDFILE} {
    return (ST_ENDFILE);
}

^{EXIT} {
    return (ST_EXIT);
}

^{GOTO} {
    return (ST_GOTO);
}

^{INQUIRE} {
    return (ST_INQUIRE);
}

^{NULLIFY} {
    return (ST_NULLIFY);
}

^{PRINT} {
    return (ST_PRINT);
}

^{PRIVATE} {
    return (ST_PRIVATE);
}

^{PUBLIC} {
    return (ST_PUBLIC);
}

^{RETURN} {
    return (ST_RETURN);
}

^{REWIND} {
    return (ST_REWIND);
}

^{STOP} {
    return (ST_STOP);
}

^{PAUSE} {
    return (ST_PAUSE);
}

^{INTENT} {
    return (ST_INTENT);
}

^{NAMELIST} {
    return (ST_NAMELIST);
}

^{EQUIVALENCE} {
    return (ST_EQUIVALENCE);
}

^({IDENTIFIER}:)?{FORALL} {
    return (ST_FORALL);
}

^({IDENTIFIER}:)?{IF}"(".+")"{THEN} {
    return (ST_IF);
}

^{IF}"(".+")"[0-9]+,[0-9]+,[0-9]+ {
    return (ST_ARITHMETIC_IF);
}

^{IF} {
    return (ST_IF_STMT);
}

^{BIND} {
    return (ST_BIND);
}

^{ELSEIF} {
    return (ST_ELSEIF);
}

^{ELSE} {
    return (ST_ELSE);
}

^{FORMAT} {
    return (ST_FORMAT);
}


^({IDENTIFIER}:)?{SELECTCASE} {
    return (ST_SELECTCASE);
}

^{CASE} {
    return (ST_CASE);
}

^({IDENTIFIER}:)?{WHERE} {
    return (ST_WHERE);
}

^{ALLOCATE} {
    return (ST_ALLOCATE);
}

^{ASSIGN} {
    return (ST_LABEL_ASSIGN);
}

^{CONTINUE} {
    return (ST_CONTINUE);
}

^{CONTAINS} {
    return (ST_CONTAINS);
}

^{DEALLOCATE} {
    return (ST_DEALLOCATE);
}

^{OPEN} {
    return (ST_OPEN);
}

^{CLOSE} {
    return (ST_CLOSE);
}

^{READ} {
    return (ST_READ);
}

^{WRITE} {
    return (ST_WRITE);
}

^{END}({FORALL}|{IF}|{MODULE}|{INTERFACE}|{SUBROUTINE}|{TYPE}|{FUNCTION}|{PROGRAM}|{DO}|{BLOCKDATA}|{SELECT}|{WHERE})? {
    return (ST_END);
}

^{INCLUDE} {
    return (DC_INCLUDE);
}

<PREFIX_SPEC>{PURE}|{RECURSIVE}|{ELEMENTAL} {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{REAL}([*][0-9]+)? {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{INTEGER}([*][0-9]+)? {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{LOGICAL}([*][0-9]+)? {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{DOUBLECOMPLEX} {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{COMPLEX}([*][0-9]+)? {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{DOUBLEPRECISION}([*][0-9]+)? {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{CHARACTER}([*][0-9]+)? {
    return (KW_PREFIX);
}

<PREFIX_SPEC>{FUNCTION} {
    return (KW_FUNCTION);
}

<PREFIX_SPEC>{SUBROUTINE} {
    return (KW_SUBROUTINE);
}

<PREFIX_SPEC>. {
    return (KW_OTHER);
}

<END_APPENDINGS>{END} {
    return (KW_END);
}


<END_APPENDINGS>{FORALL}|{IF}|{MODULE}|{SUBROUTINE}|{TYPE}|{FUNCTION}|{PROGRAM}|{DO}|{BLOCKDATA}|{SELECT}|{WHERE} {
    return (KW_END_APPEND);
}

<END_APPENDINGS>{INTERFACE} {
    inside_interface = 0;
    return (KW_END_APPEND);
}

<END_APPENDINGS>[!].* {
    return (KW_END_COMMENT);
}

<MODULE_PROC>{MODULE} {
    return (KW_MODULE_PROC);
}

<MODULE_PROC>{PROCEDURE} {
    return (KW_MODULE_PROC);
}

<OMP_DIRECTIVE>(parallel(do|sections|workshare)?)|(do)|(single)|(atomic)|(sections)|(section)|(workshare)|(master)|(critical)|(barrier)|(flush)|(ordered) {
    BEGIN(OMP_CLAUSE);
    return (KW_OMP_DIRECTIVE);
}

<OMP_DIRECTIVE>threadprivate[(][^)]+[)] {
    BEGIN(OMP_CLAUSE);
    return (KW_OMP_DIRECTIVE);
}

<OMP_DIRECTIVE>end {
    // Note we do not jump to OMP_CLAUSE because we are still waiting the real directive
    return (KW_OMP_DIRECTIVE);
}

<OMP_DIRECTIVE>.+ {
   // This directive is unknown do not convert at all
   BEGIN(0);
   return (KW_OMP_UNKNOWN_DIRECTIVE);
}

 /* These are special */
<OMP_CLAUSE>(ordered|nowait|presend)/.* {
    return (KW_OMP_CLAUSE);
}

<OMP_CLAUSE>[[:alpha:]_][[:alnum:]_]*/[(] {
    BEGIN(OMP_EXPR);
    return (KW_OMP_CLAUSE_EXPR);
}

<OMP_CLAUSE>[(][^)]+[)] {
    return (KW_OMP_CLAUSE);
}

 /* <OMP_CLAUSE>((reduction)|(copyin)|(groups)|(shared)|((last|first|copy)?private))?[(][^)]+[)] {
    return (KW_OMP_CLAUSE);
}

<OMP_CLAUSE>default[(](private|shared|none)[)] {
    return (KW_OMP_CLAUSE);
}

<OMP_CLAUSE>schedule[(](static|dynamic|guided|runtime)[)] {
    return (KW_OMP_CLAUSE);
}


<OMP_CLAUSE>(num_threads|if) {
    BEGIN(OMP_EXPR);
    return (KW_OMP_CLAUSE_EXPR);
}

*/

<OMP_CLAUSE>[,] {
    // Eat these as they are not necessary
}


<OMP_EXPR>["]([^"\n]|(["]["]))*["] {
    return (KW_OMP_EXPR_TOK);
}

<OMP_EXPR>[']([^'\n]|(['][']))*['] {
    return (KW_OMP_EXPR_TOK);
}

<OMP_EXPR>. {
    return (KW_OMP_EXPR_TOK);
}

<*>. {
    return 0;
}

%%

// ^.* {
//  fprintf(stderr, "UNCLASSIFIABLE STATEMENT \n'%s'\n", yytext);
//  exit(EXIT_FAILURE);
// }

// Needed
int yywrap()
{
    return 1;
}

void * prescanneralloc (size_t bytes)
{
    return xmalloc(bytes);
}
void * prescannerrealloc (void * ptr, size_t bytes)
{
    return xrealloc(ptr, bytes);
}
void   prescannerfree (void * ptr)
{
    DELETE(ptr);
}
