/*
    SMP superscalar Compiler
    Copyright (C) 2008 Barcelona Supercomputing Center

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

%{
/*
    Parser for a subset of ISO/IEC 9899:1999 (C99) constants
*/

#define _GNU_SOURCE
#include <math.h>

#include <stdio.h>

#include "calculator-parser-types.h"


typedef struct {
	long double value;
	long double divisor;
} digit_sequence_t;


static void calculator_error (calculator_value_t *result, char const *error_message)
{
	// fprintf(stderr, "%s\n", error_message);
}

extern int calculator_lex(void);

%}

%glr-parser
%name-prefix="calculator_"
%expect-rr 120
// %defines
%verbose
%parse-param {calculator_value_t *result}
// %debug

%union {
	uint8_t uint8;
	calculator_value_t value;
	digit_sequence_t digit_sequence;
	long long long_long;
};


// Tokens


// Non-terminals
%type<uint8> digit
%type<uint8> nonzero_digit
%type<uint8> octal_digit
%type<uint8> hexadecimal_digit

%type<value> constant
%type<value> integer_constant
%type<value> decimal_constant
%type<value> octal_constant
%type<value> hexadecimal_constant

%type<value> integer_suffix_opt
%type<value> unsigned_suffix_opt
%type<value> long_suffix_opt

%type<value> floating_constant
%type<value> decimal_floating_constant
%type<value> hexadecimal_floating_constant
%type<value> fractional_constant
%type<value> hexadecimal_fractional_constant

%type<value> floating_suffix_opt

%type<long_long> exponent_part
%type<long_long> binary_exponent_part

%type<digit_sequence> digit_sequence
%type<digit_sequence> hexadecimal_digit_sequence

%start start

%%

start :
	constant
		{ *result = $1; }
;


// *********************************************************
// A.1.3 - Identifiers
// *********************************************************

// 
// (6.4.2.1)
// 
digit :
	'0'
		{ $$ = 0; }
	| '1'
		{ $$ = 1; }
	| '2'
		{ $$ = 2; }
	| '3'
		{ $$ = 3; }
	| '4'
		{ $$ = 4; }
	| '5'
		{ $$ = 5; }
	| '6'
		{ $$ = 6; }
	| '7'
		{ $$ = 7; }
	| '8'
		{ $$ = 8; }
	| '9'
		{ $$ = 9; }
;


// *********************************************************
// A.1.5 - Constants
// *********************************************************

// 
// (6.4.4)
// 
constant :
	integer_constant
	| floating_constant
/*
	| enumeration_constant
	| character_constant
*/
;

//
// (6.4.4.1)
// 
integer_constant :
	decimal_constant integer_suffix_opt
		{ $$.type_class = $2.type_class; $$.type_signedness = $2.type_signedness; $$.value.integer_value = $$.value.integer_value; }
	| octal_constant integer_suffix_opt
		{ $$.type_class = $2.type_class; $$.type_signedness = $2.type_signedness; $$.value.integer_value = $$.value.integer_value; }
	| hexadecimal_constant integer_suffix_opt
		{ $$.type_class = $2.type_class; $$.type_signedness = $2.type_signedness; $$.value.integer_value = $$.value.integer_value; }
;

decimal_constant :
	nonzero_digit
		{ $$.value.integer_value = $1; }
	| decimal_constant digit
		{ $$.value.integer_value = $1.value.integer_value * 10 + $2; }
;

octal_constant :
	'0'
		{ $$.value.integer_value = 0; }
	| octal_constant octal_digit
		{ $$.value.integer_value = $1.value.integer_value * 8 + $2; }
;

hexadecimal_constant :
	hexadecimal_prefix hexadecimal_digit
		{ $$.value.integer_value = $2; }
	|  hexadecimal_constant hexadecimal_digit
		{ $$.value.integer_value = $1.value.integer_value * 16 + $2; }
;

hexadecimal_prefix :
	'0' 'x'
	| '0' 'X'
;

nonzero_digit :
	'1'
		{ $$ = 1; }
	| '2'
		{ $$ = 2; }
	| '3'
		{ $$ = 3; }
	| '4'
		{ $$ = 4; }
	| '5'
		{ $$ = 5; }
	| '6'
		{ $$ = 6; }
	| '7'
		{ $$ = 7; }
	| '8'
		{ $$ = 8; }
	| '9'
		{ $$ = 9; }
;

octal_digit :
	'0'
		{ $$ = 0; }
	| '1'
		{ $$ = 1; }
	| '2'
		{ $$ = 2; }
	| '3'
		{ $$ = 3; }
	| '4'
		{ $$ = 4; }
	| '5'
		{ $$ = 5; }
	| '6'
		{ $$ = 6; }
	| '7'
		{ $$ = 7; }
;

hexadecimal_digit :
	'0'
		{ $$ = 0; }
	| '1'
		{ $$ = 1; }
	| '2'
		{ $$ = 2; }
	| '3'
		{ $$ = 3; }
	| '4'
		{ $$ = 4; }
	| '5'
		{ $$ = 5; }
	| '6'
		{ $$ = 6; }
	| '7'
		{ $$ = 7; }
	| '8'
		{ $$ = 8; }
	| '9'
		{ $$ = 9; }
	| 'a'
		{ $$ = 10; }
	| 'b'
		{ $$ = 11; }
	| 'c'
		{ $$ = 12; }
	| 'd'
		{ $$ = 13; }
	| 'e'
		{ $$ = 14; }
	| 'f'
		{ $$ = 15; }
	| 'A'
		{ $$ = 10; }
	| 'B'
		{ $$ = 11; }
	| 'C'
		{ $$ = 12; }
	| 'D'
		{ $$ = 13; }
	| 'E'
		{ $$ = 14; }
	| 'F'
		{ $$ = 15; }
;

integer_suffix_opt :
	/* LAMBDA */
		{ $$.type_class = int_type; $$.type_signedness = signed_type; }
	| unsigned_suffix long_suffix_opt
		{ $$.type_class = $2.type_class; $$.type_signedness = unsigned_type; }
	| unsigned_suffix long_long_suffix
		{ $$.type_class = long_long_type; $$.type_signedness = unsigned_type; }
	| long_suffix unsigned_suffix_opt
		{ $$.type_class = long_type; $$.type_signedness = $2.type_signedness; }
	| long_long_suffix unsigned_suffix_opt
		{ $$.type_class = long_long_type; $$.type_signedness = $2.type_signedness; }
;

unsigned_suffix :
	'u'
	| 'U'
;

unsigned_suffix_opt :
	/* LAMBDA */
		{ $$.type_signedness = signed_type; }
	| unsigned_suffix
		{ $$.type_signedness = unsigned_type; }
;

long_suffix :
	'l'
	| 'L'
;

long_suffix_opt :
	/* LAMBDA */
		{ $$.type_class = int_type; }
	| long_suffix
		{ $$.type_class = long_type; }
;

long_long_suffix :
	'l' 'l'
	| 'L' 'L'
;

//
// (6.4.4.2)
// 
floating_constant :
	decimal_floating_constant
		{ $$ = $1; }
	| hexadecimal_floating_constant
		{ $$ = $1; }
;

decimal_floating_constant :
	fractional_constant floating_suffix_opt
		{ $$.type_class = $2.type_class; $$.value.floating_value = $1.value.floating_value; }
	| fractional_constant exponent_part floating_suffix_opt
		{ $$.type_class = $3.type_class; $$.value.floating_value = $1.value.floating_value * exp10l($2); }
	| digit_sequence exponent_part floating_suffix_opt
		{ $$.type_class = $3.type_class; $$.value.floating_value = $1.value * exp10l($2); }
;

hexadecimal_floating_constant :
	hexadecimal_prefix hexadecimal_fractional_constant binary_exponent_part floating_suffix_opt
		{ $$.type_class = $4.type_class; $$.value.floating_value = $2.value.floating_value * exp2l($3); }
	| hexadecimal_prefix hexadecimal_digit_sequence binary_exponent_part floating_suffix_opt
		{ $$.type_class = $4.type_class; $$.value.floating_value = $2.value * exp2l($3); }
;

fractional_constant :
	'.' digit_sequence
		{ $$.value.floating_value = $2.value / $2.divisor; }
	| digit_sequence '.' digit_sequence
		{ $$.value.floating_value = $1.value + $3.value / $3.divisor; }
	| digit_sequence '.'
		{ $$.value.floating_value = $1.value; }
;

exponent_part :
	'e' digit_sequence
		{ $$ = $2.value; }
	| 'e' '+' digit_sequence
		{ $$ = $3.value; }
	| 'e' '-' digit_sequence
		{ $$ = 0L - $3.value; }
	| 'E' digit_sequence
		{ $$ = $2.value; }
	| 'E' '+' digit_sequence
		{ $$ = $3.value; }
	| 'E' '-' digit_sequence
		{ $$ = 0L - $3.value; }
;

 /*
sign :
	'+'
	| '-'
;
 */

digit_sequence :
	digit
		{ $$.value = $1; $$.divisor = 10.L; }
	| digit_sequence digit
		{ $$.value = $1.value * 10.L + $2; $$.divisor = $1.divisor * 10.L; }
;

hexadecimal_fractional_constant :
	'.' hexadecimal_digit_sequence
		{ $$.value.floating_value = $2.value / $2.divisor; }
	| hexadecimal_digit_sequence '.' hexadecimal_digit_sequence
		{ $$.value.floating_value = $1.value + $3.value / $3.divisor; }
	| hexadecimal_digit_sequence '.'
		{ $$.value.floating_value = $1.value; }
;

binary_exponent_part :
	'p' digit_sequence
		{ $$ = $2.value; }
	| 'p' '+' digit_sequence
		{ $$ = $3.value; }
	| 'p' '-' digit_sequence
		{ $$ = 0L - $3.value; }
	| 'P' digit_sequence
		{ $$ = $2.value; }
	| 'P' '+' digit_sequence
		{ $$ = $3.value; }
	| 'P' '-' digit_sequence
		{ $$ = 0L - $3.value; }
;

hexadecimal_digit_sequence :
	hexadecimal_digit
		{ $$.value = $1; $$.divisor = 16.L; }
	| hexadecimal_digit_sequence hexadecimal_digit
		{ $$.value = $1.value * 16.L + $2; $$.divisor = $1.divisor * 16.L; }
;

floating_suffix_opt :
	/* LAMBDA */
		{ $$.type_class = double_type; }
	| 'f'
		{ $$.type_class = float_type; }
	| 'F'
		{ $$.type_class = float_type; }
	| 'l'
		{ $$.type_class = long_double_type; }
	| 'L'
		{ $$.type_class = long_double_type; }
;


%%















