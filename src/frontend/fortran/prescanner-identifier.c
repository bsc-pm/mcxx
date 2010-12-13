#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef USE_BUNDLED_REGEX
  #include "mf95-regex.h"
#else
  #include <regex.h>
#endif

#include "prescanner-identifier.h"

#include <signal.h>

#include "cxx-utils.h"

enum prescanner_lex_tokens
{
 KW_REAL = 1,
 KW_INTEGER,
 KW_LOGICAL,
 KW_COMPLEX,
 KW_DOUBLEPRECISION,
 KW_CHARACTER,
 KW_PREFIX,
 KW_FUNCTION,
 KW_SUBROUTINE,
 KW_END_APPEND,
 KW_END_COMMENT,
 KW_END,
 KW_MODULE_PROC,
 KW_OMP_DIRECTIVE,
 KW_OMP_CLAUSE,
 KW_OMP_CLAUSE_EXPR,
 KW_OMP_EXPR_TOK,
 KW_OMP_UNKNOWN_DIRECTIVE,
 KW_OTHER
};

#define ECHO fprintf(stderr, "Error unknown token: -%s-\n", yytext);
// Totally undesirable but makes things a lot easier
#include <prescanner-scanner.h>

struct sentence_information_tag
{
	char has_free_assign;
	char has_free_comma;
	char has_free_doublecolon;
	// Does not includes label
	char* statement;
};

struct line_information_tag
{
	int num_line;
	char has_label;
	char is_comment;
	char label[6];
	int num_of_statements;
	int room_for_statements;
	struct sentence_information_tag* statement_list;

	// Only for comments
	char* comment_text;
};

struct statements_information_tag
{
	char is_declaration;
	char needs_space;
};

typedef struct statements_information_tag statements_information_t;

#define STATEMENT_INFO(statement, is_decl, needs_space, keyword) \
	[statement] = {is_decl, needs_space}

statements_information_t statements_info[] =
{
	STATEMENT_INFO(ST_ASSIGNMENT, 0, 0, NULL),
	STATEMENT_INFO(ST_TYPESPEC, 1, 1, NULL),
	STATEMENT_INFO(ST_INITIALIZATION, 1, 0, NULL),
	STATEMENT_INFO(ST_TYPEDECL, 1, 1, NULL),
	STATEMENT_INFO(ST_PROGRAM, 1, 1, "program"),
	STATEMENT_INFO(ST_SUBROUTINE, 1, 1, "subroutine"),
	STATEMENT_INFO(ST_FUNCTION, 1, 1, "function"),
	STATEMENT_INFO(ST_MODULE, 1, 1, "module"),
	STATEMENT_INFO(ST_MODULE_PROCEDURE, 1, 1, "module procedure"),
	STATEMENT_INFO(ST_BLOCKDATA, 1, 1, "blockdata"),
	STATEMENT_INFO(ST_USE, 1, 1, "use"),
	STATEMENT_INFO(ST_IMPLICIT, 1, 1, "implicit"),
	STATEMENT_INFO(ST_PARAMETER, 1, 0, "parameter"),
	STATEMENT_INFO(ST_FORMAT, 1, 0, "format"),
	STATEMENT_INFO(ST_ENTRY, 1, 1, "entry"),
	STATEMENT_INFO(ST_ACCESS, 1, 1, "access"),
	STATEMENT_INFO(ST_ALLOCATABLE, 1, 1, "allocatable"),
	STATEMENT_INFO(ST_COMMON, 1, 1, "common"),
	STATEMENT_INFO(ST_DATA, 1, 1, "data"),
	STATEMENT_INFO(ST_DIMENSION, 1, 1, "dimension"),
	STATEMENT_INFO(ST_EQUIVALENCE, 1, 0, "equivalence"),
	STATEMENT_INFO(ST_EXTERNAL, 1, 1, "external"),
	STATEMENT_INFO(ST_INTENT, 1, 0, "intent"),
	STATEMENT_INFO(ST_INTRINSIC, 1, 1, "intrinsic"),
	STATEMENT_INFO(ST_NAMELIST, 1, 0, "namelist"),
	STATEMENT_INFO(ST_OPTIONAL, 1, 1, "optional"),
	STATEMENT_INFO(ST_POINTER, 1, 1, "pointer"),
	STATEMENT_INFO(ST_SAVE, 1, 1, "save"),
	STATEMENT_INFO(ST_VALUE, 1, 1, "value"),
	STATEMENT_INFO(ST_VOLATILE, 1, 1, "volatile"),
	STATEMENT_INFO(ST_TARGET, 1, 1, "target"),
	STATEMENT_INFO(ST_DO, 0, 1, "do"),
	STATEMENT_INFO(ST_LABELED_DO, 0, 1, "do"),
	STATEMENT_INFO(ST_FORALL, 0, 0, "forall"),
	STATEMENT_INFO(ST_ARITHMETIC_IF, 0, 0, "if"),
	STATEMENT_INFO(ST_IF, 0, 0, "if"),
	STATEMENT_INFO(ST_WHERE, 0, 0, "where"),
	STATEMENT_INFO(ST_ALLOCATE, 0, 0, "allocate"),
	STATEMENT_INFO(ST_BACKSPACE, 0, 1, "backspace"),
	STATEMENT_INFO(ST_CALL, 0, 1, "call"),
	STATEMENT_INFO(ST_CLOSE, 0, 0, "close"),
	STATEMENT_INFO(ST_CONTINUE, 0, 0, "continue"),
	STATEMENT_INFO(ST_CYCLE, 0, 0, "cycle"),
	STATEMENT_INFO(ST_DEALLOCATE, 0, 0, "deallocate"),
	STATEMENT_INFO(ST_ENDFILE, 0, 1, "endfile"),
	STATEMENT_INFO(ST_EXIT, 0, 0, "exit"),
	STATEMENT_INFO(ST_GOTO, 0, 1, "goto"),
	STATEMENT_INFO(ST_LABEL_ASSIGN, 0, 1, "assign"),
	STATEMENT_INFO(ST_INQUIRE, 0, 0, "inquire"),
	STATEMENT_INFO(ST_NULLIFY, 0, 0, "nullify"),
	STATEMENT_INFO(ST_OPEN, 0, 0, "open"),
	STATEMENT_INFO(ST_PRINT, 0, 1, "print"),
	STATEMENT_INFO(ST_READ, 0, 0, "read"),
	STATEMENT_INFO(ST_RETURN, 0, 1, "return"),
	STATEMENT_INFO(ST_REWIND, 0, 1, "rewind"),
	STATEMENT_INFO(ST_PAUSE, 0, 1, "pause"),
	STATEMENT_INFO(ST_STOP, 0, 1, "stop"),
	STATEMENT_INFO(ST_WRITE, 0, 0, "write"),
	STATEMENT_INFO(ST_INTERFACE, 0, 1, "interface"),
	STATEMENT_INFO(ST_ELSE, 0, 0, "else"),
	STATEMENT_INFO(ST_ELSEIF, 0, 0, "elseif"),
	STATEMENT_INFO(ST_SELECTCASE, 0, 0, "selectcase"),
	STATEMENT_INFO(ST_CASE, 0, 1, "case"),
	STATEMENT_INFO(ST_END, 0, 1, "end"),
	STATEMENT_INFO(ST_IF_STMT, 0, 1, "if"),
	STATEMENT_INFO(DC_INCLUDE, 0, 1, "include")
};

typedef struct line_information_tag line_information_t;

static int i_isblank(int c);

static line_information_t* get_information_from_line(prescanner_t* prescanner, char* c);
static language_level identify_and_convert_line(prescanner_t* prescanner, language_level previous, line_information_t* li, int sentence_index);
static void identify_and_convert_omp_directive(line_information_t* li);

static void remove_all_spaces(char** line);

static void add_blank(char** line, char* keyword);
static void add_blank_function(char** line);
static void add_blank_subroutine(char** line);
static void add_blank_end(char** line);
static void add_blank_if_statement(prescanner_t* prescanner, char** line, int num_line);
static void add_blank_labeled_do(char** line);
static void add_blank_module_procedure(char** line);
static void add_blank_label_assign_statement(char** line);
static void add_blank_entry_statement(char** line, char* keyword);


// Continuation lines have already been joined
// non-pragmatic comments trimmed and tab-started lines normalized

/*
   This function is the unique available interface. 
   
   It converts one full fortran 95 fixed form statement into something that can
   be parsed in a free form way
 */
language_level convert_line(prescanner_t* prescanner, language_level previous, char** line, int num_line)
{
	int i;
	int original_size;
	line_information_t* li;
	language_level next;

	original_size = strlen(*line);
	li = get_information_from_line(prescanner, *line);
	li->num_line = num_line;

	next = previous;
	if (!li->is_comment)
	{
		for (i = 0;  i < li -> num_of_statements; i++)
		{
			next = identify_and_convert_line(prescanner, next, li, i);
		}
	}
	else if (prescanner->openmp_processing)
	{
		identify_and_convert_omp_directive(li);
	}

	free(*line);

	// Let's make enough room
	*line = calloc(strlen(li->label) + 1 + original_size*2, sizeof(char));

	if (li->is_comment)
	{
		strcat(*line, li->comment_text);
		free(li->comment_text);
	}
	else
	{
		if (li->has_label)
		{
			strcat(*line, li->label);
			strcat(*line, " ");
		}

		for (i = 0; i < li->num_of_statements; i++)
		{
			if (i > 0) strcat(*line, "; ");
			strcat(*line, li->statement_list[i].statement);
			free(li->statement_list[i].statement);
		}

		free(li->statement_list);
	}

	return next;
}


/*
   This function gets the information of the line.  It stores the label (if
   any) in information_line.label.  The rest of the line is stored in
   information_line.statement_list and then all spaces are removed.
 */
static line_information_t* get_information_from_line(prescanner_t* prescanner, char* c)
{
	line_information_t* li;

	li = (line_information_t*)calloc(1, sizeof(line_information_t));

	// First check if this is a comment
	char* t = c;
	while (i_isblank(*t)) t++;
	if (*t == '!')
	{
		// Is this a coco line ?
		if (prescanner->openmp_processing && (strncmp(c, "!$ ", 3) == 0))
		{
			// Replace sentinel with spaces
			c[0] = ' ';
			c[1] = ' ';
		}
		else
		{
			// This must be a saved comment
			li->is_comment = 1;
			li->comment_text = strdup(c);
			// Nothing more to do 
			return li;
		}
	}

	// We read the label
	char* p = c;
	int i, j = 0;
	for(i = 0; i < 5; i++)
	{
		if (!i_isblank(*p)) 
		{
			li->has_label = 1;
			li->label[j] = *p;
			j++;
		}
		p++;
	}
	li->label[j] = '\0';


	// Once read the label we can advance to detect free operators
	// Let's make room for 5 sentences
	li->room_for_statements = 5;
	li->statement_list = (struct sentence_information_tag*) calloc(li->room_for_statements, sizeof(struct sentence_information_tag));

	// There are 0 *completed* sentences now
	li->num_of_statements = 0;

	// Skip continuation column as lines have already been joined
	p = &c[6];

	int parenthesis_level = 0;
	char in_string = 0, delim;

	// We start working in the first statement
	int current_sentence = 0;
	char* start_current_sentence = p;

	while (*p != '\0')
	{
		if (!in_string)
		{
			if ((*p == '\"') || (*p == '\''))
			{
				delim = *p;
				in_string = 1;
			}
			else if (*p == '(')
			{
				parenthesis_level++;
			}
			else if (*p == ')')
			{
				parenthesis_level--;
			}
			// semicolon marks the end of a statement
			else if ((*p == ';') && (parenthesis_level == 0))
			{
				// We know there was a ";", it is not necessary to save it
				*p = '\0';

				// Save this statement
				li->statement_list[current_sentence].statement = strdup(start_current_sentence);

				// Restore character
				*p = ';';

				// We have completed a statement
				li->num_of_statements++;

				current_sentence++;

				// We have to realloc
				if (current_sentence >= li->room_for_statements)
				{
					li->statement_list = realloc(li->statement_list, 2 * li->room_for_statements * sizeof(struct sentence_information_tag));

					// Clear new alloc
					memset(&li->statement_list[li->room_for_statements], 0, sizeof(*li->statement_list)*li->room_for_statements);

					li->room_for_statements *= 2;
				}

				start_current_sentence = p + 1;
			}
			// comma is an easy one as there are no problems
			else if ((*p == ',') && (parenthesis_level == 0))
			{
				li->statement_list[current_sentence].has_free_comma = 1;
			}
			else if ((*p == '=') && (parenthesis_level == 0))
			{
				// is this really an assignment ?
				// We will advance through blanks
				char* q = p + 1;
				while ((*q != '\0') && i_isblank(*q)) q++;

				if ((*q != '\0') && (*q != '=') ) // we found something that was not blank
				{
					// Great, it was a true assignment
					li->statement_list[current_sentence].has_free_assign = 1;
				}
				else if ((*q != '\0') && (*q == '='))
				{
					p = q;
				}
			}
			else if ((*p == ':') && (parenthesis_level == 0))
			{
				// This could be the first of a double colon
				// let's seek the second
				char* q = p + 1;
				while ((*q != '\0') && i_isblank(*q)) q++;

				if ((*q != '\0') && (*q == ':'))
				{
					// Great, we hunted a double colon that is free
					li->statement_list[current_sentence].has_free_doublecolon = 1;
					p = q;
				}
			}
		}
		else // in_string
		{
			if (*p == delim)
			{
				if (*(p+1) != delim)
				{
					in_string = 0;
				}
				else 
				{
					// We skip the next delimiter
					p++;
				}
			}
		}
		p++;
	}

	// If this happens this means that there were things between the previous
	// (possibly none) completed statement and the end of the line. So we have
	// here another completed statement
	// Note that start_current_sentence == p would happen when "A=3;"
	if (start_current_sentence < p)
	{
		li->statement_list[current_sentence].statement = strdup(start_current_sentence);
		// Another completed statement
		li->num_of_statements++;
	}

	// Now we remove spaces
	for (i = 0; i < li->num_of_statements; i++)
	{
		remove_all_spaces(&li->statement_list[i].statement);
	}

	return li;
}

/*
   This function removes all non significative whitespace 
 */
static void remove_all_spaces(char** line)
{
	char* newline = calloc(strlen(*line) + 1, sizeof(char));
	char in_string = 0, inlined_comment = 0, delim;
	char *p, *q;

	q = newline;
	p = *line;

	while (*p != '\0')
	{
		if (!in_string)
		{
			if (inlined_comment || (*p != ' ' && *p != '\t' && *p != '!'))
			{
				*q = *p;
				q++;
			}

			if (!inlined_comment && (*p == '\'' || *p == '"'))
			{
				delim = *p;
				in_string = 1;
			}
			else if (*p == '!')
			{
				// Add a space to make it more readable
				*q = ' '; q++;
				*q = *p; q++;
				inlined_comment = 1;
			}
		}
		else
		{
			*q = *p;
			q++;

			if ((*p == delim) && (*(p+1) != delim))
			{
				in_string = 0;
			}
		}
		p++;
	}

	*q = '\0';

	free(*line);
	*line = newline;
}

/*
   This function tries to identify according to the language_level 
   the starting keyword of the statement (if the statement is not an assigment).

 */
static language_level identify_and_convert_line(prescanner_t* prescanner, 
        language_level previous, line_information_t* li, int statement_index)
{
	language_level next = previous;
	int statement;

	// Nothing to do if this is an empty string
	if (strlen(li->statement_list[statement_index].statement) == 0)
	{
		return next;
	}

	if (li->statement_list[statement_index].has_free_assign &&
			!li->statement_list[statement_index].has_free_comma)
	{
		/* 
		   This statement can be either an assignment
		   or an initialized type declaration (it cannot
		   be a DO statement since it would have a free coma)

		   INTEGERA=3   ! This is an assignment
		   INTEGER::A=3 ! This is a type declaration

		   Fortunately, Fortran 95 standard dictates that a type declaration
		   with initializer must have two colons
		 */
		if (li->statement_list[statement_index].has_free_doublecolon)
		{
			// This is a type declaration
			statement = ST_INITIALIZATION;
		}
		else
		{
			// This is an assignment
			statement = ST_ASSIGNMENT;
		}
	}
	else
	{
		/* 
		   This statement cannot be an assigment
		   We must see what kind of statement is
		 */
		yy_flush_buffer(YY_CURRENT_BUFFER);
		yy_delete_buffer(YY_CURRENT_BUFFER);

		YY_BUFFER_STATE yybuf = yy_scan_string(li->statement_list[statement_index].statement);

		yy_flex_debug = 0;
		yy_switch_to_buffer(yybuf);

		// Only useful for type_specs
		statement = yylex();

		if (!statement)
		{
			fprintf(stderr, "Could not classify statement at %s:%d\n", prescanner->input_filename, li->num_line);
			fprintf(stderr, "'%s'\n\n", li->statement_list[statement_index].statement);
		}
	}

	/*
	   The finite state machine is in general very simple.
	   When we see something that only can be a declaration or an executable
	   statement we change the state (we may remain in the same). Why, then
	   do we have into account this two, apparently, unnecessary states ? 

	   When reading something that looks like a ST_FUNCTION we must check with the previous state
	   because we want to distinguish between as they can be interpreted in several funny ways

		  REALFUNCTIONA    --> REAL FUNCTIONA
		     Variable 'functiona' declaration (this would be the unique interpretation
			 dictated by the standard but check the following case)

	      REALFUNCTIONA    --> REAL FUNCTION A    
			 Function 'a' declaration. Technically standard dictates
			 parentheses after the function name but many compilers allow this
			 construction when we are in a unit program context.
			 
		  REALFUNCTIONA(B) --> REAL FUNCTION A(B)
		     Function 'a' declaration with a unique parameter 'b'. This is the case
			 you will see only after an instruction (e.g. after an 'END').

		  REALFUNCTIONA(B) --> REAL FUNCTIONA (B)
		     Variable 'functiona' declaration, that is an unidimensional array of B-length.
			 This only can happen after another declaration (e.g. after a SUBROUTINE, or
			 another variable declaration).

		  Some cases might be misdetected, specially when statement function statements are involved.
	   */

	// Includes cannot go beyond here
	if (statement == DC_INCLUDE)
	{
		// We add the blank (not necesary)
		add_blank(&li->statement_list[statement_index].statement, yytext);
		return next;
	}

	if (statement != ST_DUBIOUS_FUNCTION)
	{
		// No problem now
		if (statements_info[statement].is_declaration)
		{
			if (prescanner->debug)
			{
				fprintf(stderr, "Switching to LANG_DECLARATION_PART (%s) (statement = %d)\n", li->statement_list[statement_index].statement, statement);
			}
			next = LANG_DECLARATION_PART;
		}
		else
		{
			if (prescanner->debug)
			{
				fprintf(stderr, "Switching to LANG_INSTRUCTION_PART (%s) (statement = %d)\n", li->statement_list[statement_index].statement, statement);
			}
			next = LANG_INSTRUCTION_PART;
		}

		if (statements_info[statement].needs_space)
		{
			if (statement != ST_TYPESPEC && 
					statement != ST_FUNCTION &&
					statement != ST_SUBROUTINE &&
					statement != ST_END &&
					statement != ST_IF_STMT && 
					statement != ST_LABELED_DO &&
					statement != ST_MODULE_PROCEDURE &&
					statement != ST_LABEL_ASSIGN &&
					statement != ST_ENTRY)
			{
				// add_blank(&li->statement_list[statement_index].statement, statements_info[statement].keyword);
				add_blank(&li->statement_list[statement_index].statement, yytext);
			}
			else if (statement == ST_FUNCTION)
			{
				add_blank_function(&li->statement_list[statement_index].statement);
			}
			else if (statement == ST_SUBROUTINE)
			{
				add_blank_subroutine(&li->statement_list[statement_index].statement);
			}
			else if (statement == ST_TYPESPEC)
			{
				// Space is only needed for INTEGERA cases
				if (prescanner->debug)
				{
					fprintf(stderr, "'%s' We have to add spaces to this type declaration (1)\n", li->statement_list[statement_index].statement);
				}
				add_blank(&li->statement_list[statement_index].statement, yytext);
			}
			else if (statement == ST_IF_STMT)
			{
				if (prescanner->debug)
				{
					fprintf(stderr, "'%s' is an IF statement\n", li->statement_list[statement_index].statement);
				}
				add_blank_if_statement(prescanner, &li->statement_list[statement_index].statement, li->num_line);
			}
			else if (statement == ST_END)
			{
				add_blank_end(&li->statement_list[statement_index].statement);
			}
			else if (statement == ST_LABELED_DO)
			{
				if (prescanner->debug)
				{
					fprintf(stderr, "'%s' is a labeled do\n", li->statement_list[statement_index].statement);
				}
				add_blank_labeled_do(&li->statement_list[statement_index].statement);
			}
			else if (statement == ST_MODULE_PROCEDURE)
			{
				add_blank_module_procedure(&li->statement_list[statement_index].statement);
			}
			else if (statement == ST_LABEL_ASSIGN)
			{
				add_blank_label_assign_statement(&li->statement_list[statement_index].statement);
			}
			else if (statement == ST_ENTRY)
			{
				add_blank_entry_statement(&li->statement_list[statement_index].statement, yytext);
			}
		}
	}
	else
	{
		if (next == LANG_TOP_LEVEL || next == LANG_INSTRUCTION_PART)
		{
			// This is a true function
			if (prescanner->debug)
			{
				fprintf(stderr, "'%s' is a function\n", li->statement_list[statement_index].statement);
			}
			add_blank_function(&li->statement_list[statement_index].statement);
			next = LANG_DECLARATION_PART;
		}
		else
		{
			// This is a declaration
			if (prescanner->debug)
			{
				fprintf(stderr, "'%s' We have to add spaces to this type declaration (2)\n", li->statement_list[statement_index].statement);
			}

			// We have to get the type
			yy_flush_buffer(YY_CURRENT_BUFFER);
			yy_delete_buffer(YY_CURRENT_BUFFER);

			YY_BUFFER_STATE yybuf = yy_scan_string(li->statement_list[statement_index].statement);

			yy_flex_debug = 0;
			yy_switch_to_buffer(yybuf);

			BEGIN(PREFIX_SPEC);
			statement = yylex();
			BEGIN(0);

			add_blank(&li->statement_list[statement_index].statement, yytext);
			next = LANG_DECLARATION_PART;
		}
	}

	return next;
}


/**
  Adds a blank just after the keyword. The keyword must be
  in the first position as we'll use its length to put
  the blank.
 */
static void add_blank(char** line, char* keyword)
{
	char* new = NULL;

	new = calloc(1 + strlen(*line) + 1, sizeof(char));

	strncat(new, *line, strlen(keyword));
	// strcat(new, keyword);
	strcat(new, " ");
	strcat(new, ((*line) + strlen(keyword)));

	free(*line);
	*line = new;
}

/**
  Adds necessary space for function statements (similar approach will be needed
  for subroutine statement)
 */
static void add_blank_function(char** line)
{
	char* temp;
	int keyword, scanned_length = 0;
	int in_parenthesis = 0;

	temp = (char*)calloc(strlen(*line)*2, sizeof(char));

	yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_delete_buffer(YY_CURRENT_BUFFER);

	YY_BUFFER_STATE yybuf = yy_scan_string(*line);

	yy_flex_debug = 0;
	yy_switch_to_buffer(yybuf);

	// This state is intended for recognizing prefixes
	BEGIN(PREFIX_SPEC);

	// Only useful for type_specs
	keyword = yylex();
	scanned_length += strlen(yytext);
	while (keyword != KW_FUNCTION)
	{
		switch (keyword)
		{
			case KW_PREFIX:
				{
					strcat(temp, yytext);
					if (!in_parenthesis)
					{
						strcat(temp, " ");
					}
					break;
				}
			case KW_OTHER :
				{
					strcat(temp, yytext);
					if (*yytext == '(')
					{	
						in_parenthesis++;
					}
					else if (in_parenthesis && *yytext == ')')
					{	
						in_parenthesis--;
						if (!in_parenthesis) strcat(temp, " ");
					}
					break;
				}
		}
		keyword = yylex();
		scanned_length += strlen(yytext);
	}

	strcat(temp, yytext);
	strcat(temp, " ");
	strcat(temp, &(*line)[scanned_length]);

	free(*line);
	*line = temp;

	BEGIN(0);
}

/*
   Add blank for subroutine declarations
 */
static void add_blank_subroutine(char** line)
{
	char* temp;
	int  keyword, scanned_length = 0;

	temp = (char*)calloc(strlen(*line)*2, sizeof(char));

	yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_delete_buffer(YY_CURRENT_BUFFER);

	YY_BUFFER_STATE yybuf = yy_scan_string(*line);

	yy_flex_debug = 0;
	yy_switch_to_buffer(yybuf);

	// This state is intended for recognizing prefixes
	BEGIN(PREFIX_SPEC);

	// Only useful for type_specs
	keyword = yylex();
	scanned_length += strlen(yytext);
	while (keyword != KW_SUBROUTINE)
	{
		switch (keyword)
		{
			case KW_PREFIX : 
				{
					strcat(temp, yytext);
					strcat(temp, " ");
					break;
				}
			case KW_OTHER :
				{
					strcat(temp, yytext);
					break;
				}
		}
		keyword = yylex();
		scanned_length += strlen(yytext);
	}

	strcat(temp, yytext);
	strcat(temp, " ");
	strcat(temp, &(*line)[scanned_length]);

	free(*line);
	*line = temp;

	BEGIN(0);
}

/*
   Add nice blank after END

   Note. When reading END INTERFACE inside_interface is set to zero. This
   is used for MODULE PROCEDURE in the flex scanner
 */
static void add_blank_end(char** line)
{
	// While give next token != FUNCTION
	// strcat(result, token);
	// strcat(result, " ");
	// Give next token FUNCTION
	// We will use flex again
	char* temp;
	char keyword_after_end = 0;
	int  keyword, scanned_length = 0;

	temp = (char*)calloc(strlen(*line)*2, sizeof(char));

	yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_delete_buffer(YY_CURRENT_BUFFER);

	YY_BUFFER_STATE yybuf = yy_scan_string(*line);

	yy_flex_debug = 0;
	yy_switch_to_buffer(yybuf);

	// This state is intended for recognizing end suffixes
	BEGIN(END_APPENDINGS);

	// Only useful for type_specs
	keyword = yylex();
	while (keyword != 0 && !keyword_after_end)
	{
		switch (keyword)
		{
			case KW_END_APPEND :
				{
					keyword_after_end = 1;
					strcat(temp, yytext);
					strcat(temp, " ");
					break;
				}
			case KW_END :
				{
					strcat(temp, yytext);
					strcat(temp, " ");
					break;
				}
			case KW_END_COMMENT :
				{
					keyword_after_end = 1;
					strcat(temp, yytext);
					break;
				}
		}
		scanned_length += strlen(yytext);
		if (!keyword_after_end)
			keyword = yylex();
	}

	strcat(temp, &(*line)[scanned_length]);

	free(*line);
	*line = temp;

	BEGIN(0);
}

/**
  Adds blank after MODULE and after PROCEDURE in a module_procedure_statement.
  Note that flex will return ST_MODULE_PROCEDURE only when the value of
  inside_interface is 1
 */
static void add_blank_module_procedure(char** line)
{
	char* temp;
	int  keyword, scanned_length = 0;

	temp = (char*)calloc(strlen(*line)*2, sizeof(char));

	yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_delete_buffer(YY_CURRENT_BUFFER);

	YY_BUFFER_STATE yybuf = yy_scan_string(*line);

	yy_flex_debug = 0;
	yy_switch_to_buffer(yybuf);

	// This state is intended for recognizing prefixes
	BEGIN(MODULE_PROC);

	// This will be MODULE
	keyword = yylex();
	scanned_length += strlen(yytext);
	strcat(temp, yytext);
	strcat(temp, " ");

	// This will be PROCEDURE
	keyword = yylex();
	scanned_length += strlen(yytext);
	strcat(temp, yytext);
	strcat(temp, " ");
	strcat(temp, &(*line)[scanned_length]);

	free(*line);
	*line = temp;

	BEGIN(0);
}

/**
  Adds blanks in the statement following an IF statement
 */
static void add_blank_if_statement(prescanner_t* prescanner, char** line, int num_line)
{
	// We have to jump the IF(..) and identify the rest of the statement
	char level = 1, delim;
	char in_string = 0;
	char* p = &(*line)[3]; // We ignore "IF("

	while ((*p != '\0') && (level > 0))
	{
		if (!in_string && (*p == ')'))
		{
			level--;
		}
		else if (!in_string && (*p == '('))
		{
			level++;
		}
		else if ((*p == '\'') || (*p == '"'))
		{
			if (!in_string)
			{
				in_string = 1;
				delim = *p;
			}
			else if (*(p+1) != delim)
			{
				in_string = 0;
			}
			else if (*(p+1) == delim)
			{
				// We have to advance one more character
				p++;
			}
		}
		p++;
	}

	if (level > 0)
	{
		fprintf(stderr, "Malformed IF statement. Rejecting to modify this line.\n'%s'\n", *line);
	}
	else
	{
		// The rest of the line must be identified properly
		// char* c = strdup(p);
		char* c = calloc(6 + strlen(p) + 1, sizeof(char));
		strcat(c, "      ");
		strcat(c, p);
		if (prescanner->debug)
		{
			fprintf(stderr, "Before converting '%s'\n", c);
		}

		convert_line(prescanner, LANG_INSTRUCTION_PART, &c, num_line);

		if (prescanner->debug)
		{
			fprintf(stderr, "After converting '%s'\n", c);
		}

		char* temp = (char*)calloc(strlen(*line)*2, sizeof(char));
		*p = '\0';
		strcat(temp, *line);
		strcat(temp, c);

		free(c);
		free(*line);
		*line = temp;
		
	}
}

/*
   Add blanks after the label of DO
 */
static void add_blank_labeled_do(char** line)
{
	int i;
	char ending;
	char* temp = calloc(strlen(*line)*2, sizeof(char));

	// "DO "
	ending = (*line)[2];
	(*line)[2] = '\0';
	strcat(temp, (*line));
	strcat(temp, " ");

	(*line)[2] = ending;

	i = 2;
	while (isdigit((*line)[i])) i++;
	
	ending = (*line)[i];
	(*line)[i] = '\0';

	// The numeric label
	strcat(temp, &(*line)[2]);
	strcat(temp, " ");

	(*line)[i] = ending;	

	// The remaining characters of this statement
	strcat(temp, &(*line)[i]);

	free(*line);
	*line = temp;
}


/*
   Add blanks to a F77/F90 (but not F95) ASSIGN statement
*/
static void add_blank_label_assign_statement(char** line)
{
	int i;
	char ending;
	char* temp = calloc(strlen(*line)*2, sizeof(char));
	// "ASSIGN"
	ending = (*line)[6];
	(*line)[6] = '\0';

	strcat(temp, (*line));
	strcat(temp, " ");

	(*line)[6] = ending;
	i = 6;
	while (isdigit((*line)[i])) i++;
	
	ending = (*line)[i];
	(*line)[i] = '\0';
	
	// The numeric label of the assign
	strcat(temp, &(*line)[6]);
	strcat(temp, " ");

	(*line)[i] = ending;

	// "TO"
	ending = (*line)[i+2];
	(*line)[i+2] = '\0';

	strcat(temp, &(*line)[i]);
	strcat(temp, " ");

	(*line)[i+2] = ending;

	strcat(temp, &(*line)[i+2]);

	free(*line);
	*line = temp;
}

static void add_blank_entry_statement(char** line, char* keyword)
{
	regex_t match_problematic;
	regmatch_t sub_matching[2];
	int code;
	char* temp;

	temp = calloc(sizeof(temp), strlen(*line)*2);

	if ((code = regcomp(&match_problematic, "^ENTRY([A-Z][0-9A-Z]*)RESULT[(][A-Z][0-9A-Z]*[)]$", REG_EXTENDED | REG_ICASE)) != 0)
	{
		char error_message[120];
		regerror(code, &match_problematic, error_message, 120);
		internal_error("Error when compiling regular expression (%s)\n", error_message);
	}

	if (regexec(&match_problematic, *line, 2, sub_matching, 0) == 0)
	{
		// This is a case we have to deal specially
		strcat(temp, yytext);
		strcat(temp, " ");

		char mark = (*line)[sub_matching[1].rm_eo];

		(*line)[sub_matching[1].rm_eo] = '\0';

		strcat(temp,&((*line)[sub_matching[1].rm_so]));

		(*line)[sub_matching[1].rm_eo] = mark;

		strcat(temp, " ");
		strcat(temp, &((*line)[sub_matching[1].rm_eo]));

		free(*line);
		*line = temp;
	}
	else // generic handling is enough otherwise
	{
		add_blank(line, keyword);
	}

	regfree(&match_problematic);
}

static void identify_and_convert_omp_directive(line_information_t* li)
{
	// Check if this is an omp directive
	if ((strncasecmp(li->comment_text, "!$omp ", 6) != 0))
	{
		return;
	}

	// Discard !$OMP
	char* c = strdup(&li->comment_text[6]);
	char* result = calloc(strlen(c)*2, sizeof(char));

	// Add "!$OMP " 
	strncat(result, li->comment_text, 6);

	remove_all_spaces(&c);

	yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_delete_buffer(YY_CURRENT_BUFFER);

	YY_BUFFER_STATE yybuf = yy_scan_string(c);

	yy_flex_debug = 0;
	yy_switch_to_buffer(yybuf);

	BEGIN(OMP_DIRECTIVE);

	int lex = yylex();

	if (lex == KW_OMP_UNKNOWN_DIRECTIVE)
	{
		// Cowardly refuse to do anything else when the directive is unknown
		BEGIN(0);
		free(c);
		return;
	}

	int parenthesis_level;

	// The directive
	strcat(result, yytext);

	while (YYSTATE == OMP_DIRECTIVE)
	{
		lex = yylex();

		strcat(result, " ");
		strcat(result, yytext);
	}

	lex = yylex();
	while (lex != 0)
	{
		// The clauses
		switch (lex)
		{
			case KW_OMP_CLAUSE :
				{
					strcat(result, " ");
					strcat(result, yytext);
					break;
				}
			case KW_OMP_CLAUSE_EXPR : 
				{
					// This has changed the state of the lexer to OMP_EXPR
					// (see prescanner.l)
					strcat(result, " ");
					strcat(result, yytext);
					parenthesis_level = 0;
					break;
				}
			case KW_OMP_EXPR_TOK :
				{
					if (strlen(yytext) == 1)
					{
						switch (yytext[0])
						{
							case '(' :
								{
									parenthesis_level++;
									break;
								}
							case ')' :
								{
									parenthesis_level--;
									// The expression enclosed in parenthesis has finished
									if (parenthesis_level == 0)
									{
										// Come back to OMP_CLAUSE (see prescanner.l)
										BEGIN(OMP_CLAUSE);
									}
									break;
								}
							default:
								break;
						}
					}
					strcat(result, yytext);
					break;
				}
			default: 
				break;
		}

		lex = yylex();
	}

	BEGIN(0);

	free(li->comment_text);
	li->comment_text = result;

	free(c);
}

/*
   Tests if a character is blank.
   This function is defined in <ctype.h> in C99.
 */
static int i_isblank(int c)
{
	return (c == ' ' || c == '\t');
}
