%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <gc.h>
#include "cxx-lexer.h"
#include "cxx-ast.h"
#include "cxx-parser.h"

int yywrap();

void parse_token_text();
int no_line=1;
int first = 0;

// For Garbage collection support
#define free(x)
#define malloc(x) GC_MALLOC(x)

%}

%option stack

 /* Lexical conventions A.2 */
 /* A.2.1 */
hexquad         {hexdigit}{hexdigit}{hexdigit}{hexdigit}
 /* A.2.2 */
uchar           (\\u{hexquad}|\\U{hexquad}{hexquad})
 /* A.2.12 */
nondigit        ([_a-zA-Z]|uchar)
 /* A.2.13 */
digit       [0-9]
 /* A.2.20 */
nonzero         [1-9]
 /* A.2.21 */
octaldigit      [0-7]
 /* A.2.22 */
hexdigit        [a-fA-F0-9]
 /* A.2.23 includes GNU extension of long long */
integersuffix   ({longsuffix}{unsignedsuffix}?|{unsignedsuffix}{longsuffix}?|{unsignedsuffix}{longlongsuffix}|{longlongsuffix}{unsignedsuffix}?)
 /* A.2.24 */
unsignedsuffix  [uU]
 /* A.2.25 */
longsuffix  [lL]

 /* GNU Extension */
longlongsuffix [lL][lL]


 /* A.2.28 */
cchar       ([^\\'\n]|{escape_seq})




 /* A.2.29 */
escape_seq      ((\\["abfnrtv])|(\\{octaldigit}{1,3})|((\\x)+{hexdigit}+))
 /* A.2.34 */
fractconst  ({digit}*\.{digit}+|{digit}+\.)
 /* A.2.35 */
exponent    ([eE][+-]?{digit}+)
 /* A.2.38 */
floatingsuffix  [flFL]
 /* A.2.40 */

schar       ([^\\"\n]|{escape_seq})

idnondigit  ({nondigit}|{uchar})
hexprefix   0[xX]


%x longcomment
%x linecomment
%x preprocess

%%
 /* preprocess */

"#"     { yy_push_state(preprocess); }
<preprocess>.       { /* ignore line */  }
<preprocess>\n  { no_line++; yy_pop_state(); }


 /* C comments ( 6.4.9 ) */

 /* // comments. // can be splitted across different lines */

"/"(\\\n)*"/"       { yy_push_state(linecomment); }

 /* escaped new line does not end comment */
<linecomment>\\\n   { no_line++; }
<linecomment>.      { /* ignore comment */  }
<linecomment>\n     { no_line++; yy_pop_state(); }

 /* long C comments. tags can be splitted across diferent lines */

"/"(\\\n)*"*"               { yy_push_state(longcomment); }
<longcomment>\n             { no_line++; }
<longcomment>.              { /* ignore comment */ }
<longcomment>"*"(\\\n)*"/"  { yy_pop_state(); }


 /* A.2.11 */
"char"          { parse_token_text(); return CHAR; }
"wchar_t"       { parse_token_text(); return WCHAR_T; }
"bool"          { parse_token_text(); return BOOL; }
"short"         { parse_token_text(); return SHORT; }
"int"           { parse_token_text(); return INT; }
"long"          { parse_token_text(); return LONG; }
"signed"        { parse_token_text(); return SIGNED; }
"unsigned"      { parse_token_text(); return UNSIGNED; }
"float"         { parse_token_text(); return FLOAT; }
"double"        { parse_token_text(); return DOUBLE; }
"void"          { parse_token_text(); return VOID; }

"typedef"       { parse_token_text(); return TYPEDEF; }
"class"         { parse_token_text(); return CLASS; }
"enum"          { parse_token_text(); return ENUM; } 
"template"      { parse_token_text(); return TEMPLATE; } 
"struct"        { parse_token_text(); return STRUCT; } 
"export"        { parse_token_text(); return EXPORT; } 
"friend"        { parse_token_text(); return FRIEND; } 
"auto"          { parse_token_text(); return AUTO; } 
"register"      { parse_token_text(); return REGISTER; } 
"static"        { parse_token_text(); return STATIC; } 
"extern"        { parse_token_text(); return EXTERN; } 
"mutable"       { parse_token_text(); return MUTABLE; } 
"inline"        { parse_token_text(); return INLINE; } 
"virtual"       { parse_token_text(); return VIRTUAL; } 
"explicit"      { parse_token_text(); return EXPLICIT; } 
"typename"      { parse_token_text(); return TYPENAME; } 
"const"         { parse_token_text(); return CONST; } 
"volatile"      { parse_token_text(); return VOLATILE; } 
"union"         { parse_token_text(); return UNION; } 
"using"         { parse_token_text(); return USING; } 
"namespace"     { parse_token_text(); return NAMESPACE; } 
"asm"           { parse_token_text(); return ASM; } 
"private"       { parse_token_text(); return PRIVATE; } 
"protected"     { parse_token_text(); return PROTECTED; } 
"public"        { parse_token_text(); return PUBLIC; } 
"throw"         { parse_token_text(); return THROW; } 
"operator"      { parse_token_text(); return OPERATOR; } 

"new"           { parse_token_text(); return NEW; } 
"delete"        { parse_token_text(); return DELETE; }


"this"          { parse_token_text(); return THIS; }
"dynamic_cast"      { parse_token_text(); return DYNAMIC_CAST; }
"static_cast"       { parse_token_text(); return STATIC_CAST; }
"reinterpret_cast"      { parse_token_text(); return REINTERPRET_CAST; }
"const_cast"        { parse_token_text(); return CONST_CAST; }
"typeid"        { parse_token_text(); return TYPEID; }
"sizeof"        { parse_token_text(); return SIZEOF; }
"case"          { parse_token_text(); return CASE; }
"default"       { parse_token_text(); return DEFAULT; }
"if"            { parse_token_text(); return IF; }
"else"          { parse_token_text(); return ELSE; }
"switch"        { parse_token_text(); return SWITCH; }
"while"         { parse_token_text(); return WHILE; }
"do"            { parse_token_text(); return DO; }
"for"           { parse_token_text(); return FOR; }
"break"         { parse_token_text(); return BREAK; }
"continue"      { parse_token_text(); return CONTINUE; }
"goto"          { parse_token_text(); return GOTO; }
"try"           { parse_token_text(); return TRY; }
"catch"         { parse_token_text(); return CATCH; }
"return"        { parse_token_text(); return RETURN; }

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
"_Complex" { parse_token_text(); return COMPLEX; }
"__typeof" { parse_token_text(); return TYPEOF; }
"__typeof__" { parse_token_text(); return TYPEOF; }
"__restrict__" { parse_token_text(); return RESTRICT; }
"__restrict" { parse_token_text(); return RESTRICT; }
"__const" { parse_token_text(); return CONST; }
"__const__" { parse_token_text(); return CONST; }
"__attribute__" { parse_token_text(); return ATTRIBUTE; }
"__thread" { parse_token_text(); return THREAD; } 
"__inline"        { parse_token_text(); return INLINE; } 
"__inline__"        { parse_token_text(); return INLINE; } 
"__asm__" { parse_token_text(); return ASM; }

"<?" { parse_token_text(); return MIN_OPERATOR; }
">?" { parse_token_text(); return MAX_OPERATOR; }
"<?=" { parse_token_text(); return MIN_OPERATOR_ASSIGN; }
">?=" { parse_token_text(); return MAX_OPERATOR_ASSIGN; }

{idnondigit}({idnondigit}|{digit})* {parse_token_text(); return IDENTIFIER; }


 /* A.2.17 */
{nonzero}({digit})*{integersuffix}?     { parse_token_text(); return DECIMAL_LITERAL; }

 /* A.2.18 */
0*{octaldigit}*{integersuffix}?         { parse_token_text(); return OCTAL_LITERAL; }

 /* A.2.19 */
{hexprefix}{hexdigit}+{integersuffix}?  { parse_token_text(); return HEXADECIMAL_LITERAL; }

 /* A.2.26 */
L?'{cchar}*'        { parse_token_text(); return CHARACTER_LITERAL; }

 /* A.2.33 */
{fractconst}{exponent}?{floatingsuffix}?        { parse_token_text();
return FLOATING_LITERAL; }

 /* A.2.39 */
L?\"{schar}*\"      { parse_token_text(); return STRING_LITERAL; }


"["         { parse_token_text(); return '['; }
"]"         { parse_token_text(); return ']'; }
")"         { parse_token_text(); return ')'; }
"("         { parse_token_text(); return '('; }
"->"        { parse_token_text(); return PTR_OP; }
"->*"       { parse_token_text(); return PTR_OP_MUL; }
"++"        { parse_token_text(); return PLUSPLUS; }
"--"        { parse_token_text(); return MINUSMINUS; }
"+"         { parse_token_text(); return '+'; }
"-"         { parse_token_text(); return '-'; }
"~"         { parse_token_text(); return '~'; }
"!"         { parse_token_text(); return '!'; }
"/"         { parse_token_text(); return '/'; }
"%"         { parse_token_text(); return '%'; }
"<<"        { parse_token_text(); return LEFT; }
">>"        { parse_token_text(); return RIGHT; }
"<="        { parse_token_text(); return LESS_OR_EQUAL; }
">="        { parse_token_text(); return GREATER_OR_EQUAL; }
"=="        { parse_token_text(); return EQUAL; }
"!="        { parse_token_text(); return NOT_EQUAL; }
"^"         { parse_token_text(); return '^'; }
"|"         { parse_token_text(); return '|'; }
"&&"        { parse_token_text(); return ANDAND; }
"||"        { parse_token_text(); return OROR; }
"="         { parse_token_text(); return '='; }
"*="        { parse_token_text(); return MUL_ASSIGN; }
"/="        { parse_token_text(); return DIV_ASSIGN; }
"%="        { parse_token_text(); return MOD_ASSIGN; }
"+="        { parse_token_text(); return ADD_ASSIGN; }
"-="        { parse_token_text(); return SUB_ASSIGN; }
"<<="       { parse_token_text(); return LEFT_ASSIGN; }
">>="       { parse_token_text(); return RIGHT_ASSIGN; }
"&="        { parse_token_text(); return AND_ASSIGN; }
"^="        { parse_token_text(); return XOR_ASSIGN; }
"|="        { parse_token_text(); return OR_ASSIGN; }



";"         { parse_token_text(); return ';'; } 
":"         { parse_token_text(); return ':'; }
"."         { parse_token_text(); return '.'; }
","         { parse_token_text(); return ','; }
"?"         { parse_token_text(); return '?'; }
"{"         { parse_token_text(); return '{'; }
"}"         { parse_token_text(); return '}'; } 
"<"         { parse_token_text(); return '<'; }
">"         { parse_token_text(); return '>'; } 
"&"         { parse_token_text(); return '&'; }
"*"         { parse_token_text(); return '*'; }
"\""        { parse_token_text(); return '"'; }
"::"        { parse_token_text(); return DOS_DOS_PUNTS; }
"..."       { parse_token_text(); return TRES_PUNTS; }

[\n]        { no_line++; }
[ \t\v\f]   {  }
.           { 
    /* ignore bad characters */ 
    if (isgraph(*yytext))
    {
        fprintf(stderr, "Unknown character '%c' (%#x). Ignoring it.\n", *yytext, *yytext);
    }
    else
    {
        fprintf(stderr, "Unknown character %#x. Ignoring it.\n", *yytext);
    }
}


%%

int yywrap()
{
    return 1;
}

void parse_token_text()
{
    yylval.token_atrib.token_line = no_line;
    yylval.token_atrib.token_text = strdup(yytext);
}
