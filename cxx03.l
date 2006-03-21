%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-ast.h"
#include "cxx-parser.h"

int yywrap();

void parse_token();
void parse_token_text();
int no_line=1;
int first = 0;

%}

%option stack

 /* Lexical conventions A.2 */
 /* A.2.1 */
hexquad         {hexdigit}{hexdigit}{hexdigit}{hexdigit}
 /* A.2.2 */
uchar           (\\u{hexquad}|\\U{hexquad}{hexquad})
 /* A.2.12 */
nondigit    	([_a-zA-Z]|uchar)
 /* A.2.13 */
digit		[0-9]
 /* A.2.20 */
nonzero         [1-9]
 /* A.2.21 */
octaldigit      [0-7]
 /* A.2.22 */
hexdigit        [a-fA-F0-9]
 /* A.2.23 includes GNU extension of long long */
integersuffix 	({longsuffix}{unsignedsuffix}?|{unsignedsuffix}{longsuffix}?|{unsignedsuffix}{longlongsuffix}|{longlongsuffix}{unsignedsuffix}?)
 /* A.2.24 */
unsignedsuffix	[uU]
 /* A.2.25 */
longsuffix	[lL]

 /* GNU Extension */
longlongsuffix [lL][lL]


 /* A.2.28 */
cchar		([^\\'\n]|{escape_seq})




 /* A.2.29 */
escape_seq      ((\\["abfnrtv])|(\\{octaldigit}{1,3})|((\\x)+{hexdigit}+))
 /* A.2.34 */
fractconst	({digit}*\.{digit}+|{digit}+\.)
 /* A.2.35 */
exponent	([eE][+-]?{digit}+)
 /* A.2.38 */
floatingsuffix	[flFL]
 /* A.2.40 */

schar		([^\\"\n]|{escape_seq})

idnondigit	({nondigit}|{uchar})
hexprefix	0[xX]


%x longcomment
%x linecomment
%x preprocess

%%
 /* preprocess */

"#"		{ yy_push_state(preprocess); }
<preprocess>.		{ /* ignore line */  }
<preprocess>\n 	{ no_line++; yy_pop_state(); }


 /* C comments ( 6.4.9 ) */

 /* // comments. // can be splitted across different lines */

"/"(\\\n)*"/"		{ yy_push_state(linecomment); }

 /* escaped new line does not end comment */
<linecomment>\\\n	{ no_line++; }
<linecomment>.		{ /* ignore comment */  }
<linecomment>\n 	{ no_line++; yy_pop_state(); }

 /* long C comments. tags can be splitted across diferent lines */

"/"(\\\n)*"*"				{ yy_push_state(longcomment); }
<longcomment>\n				{ no_line++; }
<longcomment>.				{ /* ignore comment */ }
<longcomment>"*"(\\\n)*"/"	{ yy_pop_state(); }


 /* A.2.11 */
"char"			{ parse_token(); return CHAR; }
"wchar_t"		{ parse_token(); return WCHAR_T; }
"bool"			{ parse_token(); return BOOL; }
"short"			{ parse_token(); return SHORT; }
"int"			{ parse_token(); return INT; }
"long"			{ parse_token(); return LONG; }
"signed"		{ parse_token(); return SIGNED; }
"unsigned"		{ parse_token(); return UNSIGNED; }
"float"			{ parse_token(); return FLOAT; }
"double"		{ parse_token(); return DOUBLE; }
"void"			{ parse_token(); return VOID; }

"typedef"		{return TYPEDEF; }
"class"			{return CLASS; }
"enum"			{return ENUM; } 
"template"		{return TEMPLATE; } 
"struct"		{return STRUCT; } 
"export"		{return EXPORT; } 
"friend"		{return FRIEND; } 
"auto"			{return AUTO; } 
"register"		{return REGISTER; } 
"static"		{return STATIC; } 
"extern"		{return EXTERN; } 
"mutable"		{return MUTABLE; } 
"inline"		{return INLINE; } 
"virtual"		{return VIRTUAL; } 
"explicit"		{return EXPLICIT; } 
"typename"		{return TYPENAME; } 
"const"			{return CONST; } 
"volatile"		{return VOLATILE; } 
"union"			{return UNION; } 
"using"			{return USING; } 
"namespace"		{return NAMESPACE; } 
"asm"			{return ASM; } 
"private"		{return PRIVATE; } 
"protected"		{return PROTECTED; } 
"public"		{return PUBLIC; } 
"throw"			{return THROW; } 
"operator"		{return OPERATOR; } 

"new"			{return NEW; } 
"delete"		{return DELETE; }


"this"			{return THIS; }
"dynamic_cast"		{return DYNAMIC_CAST; }
"static_cast"		{return STATIC_CAST; }
"reinterpret_cast"		{return REINTERPRET_CAST; }
"const_cast"		{return CONST_CAST; }
"typeid"		{return TYPEID; }
"sizeof"		{return SIZEOF; }
"case"			{return CASE; }
"default"		{return DEFAULT; }
"if"			{return IF; }
"else"			{return ELSE; }
"switch"		{return SWITCH; }
"while"			{return WHILE; }
"do"			{return DO; }
"for"			{return FOR; }
"break"			{return BREAK; }
"continue"		{return CONTINUE; }
"return"		{return RETURN; }
"goto"			{return GOTO; }
"try"			{return TRY; }
"catch"			{return CATCH; }

 /* A.2.41 */
"false"         { parse_token_text(); return BOOLEAN_LITERAL; }
"true"          { parse_token_text(); return BOOLEAN_LITERAL; }

 /* GNU Extensions */
"__builtin_va_arg" { parse_token_text(); return BUILTIN_VA_ARG; }
"__extension__" { parse_token_text(); return EXTENSION; }
"__alignof__" { parse_token_text(); return ALIGNOF; }
"__real__" { parse_token_text(); return REAL; }
"__imag__" { parse_token_text(); return IMAG; }
"__label__" { parse_token_text(); return LABEL; }
"__complex__" { parse_token_text(); return COMPLEX; }
"__typeof__" { parse_token_text(); return TYPEOF; }
"__restrict__" { parse_token_text(); return RESTRICT; }
"__attribute__" { parse_token_text(); return ATTRIBUTE; }
"thread" { parse_token_text(); return THREAD; }
 
"__thread" { /* C99 ? */ parse_token_text(); return THREAD; } 

"<?" { parse_token_text(); return MIN_OPERATOR; }
">?" { parse_token_text(); return MIN_OPERATOR; }
"<?=" { parse_token_text(); return MIN_OPERATOR_ASSIGN; }
">?=" { parse_token_text(); return MIN_OPERATOR_ASSIGN; }

{idnondigit}({idnondigit}|{digit})* {parse_token_text(); return IDENTIFIER; }


 /* A.2.17 */
{nonzero}({digit})*{integersuffix}?		{ parse_token_text(); return DECIMAL_LITERAL; }
 /*{nonzero}({digit})*		{ parse_token_text(); return DECIMAL_LITERAL; }*/

 /* A.2.18 */
0*{octaldigit}*{integersuffix}?			{ parse_token_text(); return OCTAL_LITERAL; }

 /* A.2.19 */
{hexprefix}{hexdigit}+{integersuffix}?	{ parse_token_text(); return HEXADECIMAL_LITERAL; }

 /* A.2.26 */
L?'{cchar}*'		{ parse_token_text(); return CHARACTER_LITERAL; }

 /* A.2.33 */
{fractconst}{exponent}?{floatingsuffix}?		{ parse_token_text();
return FLOATING_LITERAL; }

 /* A.2.39 */
L?\"{schar}*\"		{ parse_token_text(); return STRING_LITERAL; }


"["         { return '['; }
"]"         { return ']'; }
")"         { return ')'; }
"("         { return '('; }
"->"        { return PTR_OP; }
"->*"       { return PTR_OP_MUL; }
"++"        { return PLUSPLUS; }
"--"        { return MINUSMINUS; }
"+"         { return '+'; }
"-"         { return '-'; }
"~"         { return '~'; }
"!"         { return '!'; }
"/"         { return '/'; }
"%"         { return '%'; }
"<<"        { return LEFT; }
">>"        { return RIGHT; }
"<="        { return LESS_OR_EQUAL; }
">="        { return GREATER_OR_EQUAL; }
"=="        { return EQUAL; }
"!="        { return NOT_EQUAL; }
"^"         { return '^'; }
"|"         { return '|'; }
"&&"        { return ANDAND; }
"||"        { return OROR; }
"="         { return '='; }
"*="        { return MUL_ASSIGN; }
"/="        { return DIV_ASSIGN; }
"%="        { return MOD_ASSIGN; }
"+="        { return ADD_ASSIGN; }
"-="        { return SUB_ASSIGN; }
"<<="       { return LEFT_ASSIGN; }
">>="       { return RIGHT_ASSIGN; }
"&="        { return AND_ASSIGN; }
"^="        { return XOR_ASSIGN; }
"|="        { return OR_ASSIGN; }



";"			{ return ';'; } 
":"			{ return ':'; }
"."			{ return '.'; }
","			{ return ','; }
"?"			{ return '?'; }
"{"			{ return '{'; }
"}"			{ return '}'; } 
"<"			{ return '<'; }
">"			{ return '>'; } 
"&"			{ return '&'; }
"*"			{ return '*'; }
"\""			{ return '"'; }
"::"			{ return DOS_DOS_PUNTS; }
"..."			{ return TRES_PUNTS; }

[\n] 		{no_line++;}
[ \t\v\f]	{  }
.		{ /* ignore bad characters */ }


%%

int yywrap()
{
	return 1;
}

void parse_token()
{
	yylval.token_atrib.line = no_line;
}

void parse_token_text()
{
	parse_token();
	yylval.token_atrib.text = strdup(yytext);
}
