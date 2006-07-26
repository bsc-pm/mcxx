#ifndef CXX_DRIVER_H
#define CXX_DRIVER_H

#include <cxx-ast.h>
#include <cxx-scope.h>
#include <getopt.h>

typedef enum 
{
	OPTION_UNDEFINED = 1024,
	OPTION_VERSION
} COMMAND_LINE_OPTIONS;

typedef enum source_kind_tag
{
	SOURCE_KIND_UNKNOWN = 0,
	SOURCE_KIND_C_NOT_PREPROCESSED,
	SOURCE_KIND_CXX_NOT_PREPROCESSED,
	SOURCE_KIND_C_PREPROCESSED,
	SOURCE_KIND_CXX_PREPROCESSED
} source_kind_t;

struct extensions_table_t
{
	char* name;
	source_kind_t source_kind;
};

// Parameters
extern struct option getopt_long_options[];

typedef struct translation_unit_tag
{
	char* input_filename;
	char* output_filename;

	AST parsed_tree;
	scope_t* global_scope;
} translation_unit_t;

typedef struct compilation_options_tag
{
	// For further use
	int argc;
	char** argv;

	// Options
	char verbose;
	char keep_files;
	char check_dates;
	char do_not_link;
	char debug_level;

	// List of translation units
	translation_unit_t** translation_units;

	// Result of the execution
	int execution_result;

	// This makes things non reentrant
	scope_t* global_scope;
} compilation_options_t;


extern compilation_options_t compilation_options;

extern int yydebug;
extern int mcxx_flex_debug;

extern int yyparse(AST* parsed_tree);

struct extensions_table_t *
fileextensions_lookup (register const char *str, 
		register unsigned int len);

#endif // CXX_DRIVER_H
