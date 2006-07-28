#ifndef CXX_DRIVER_H
#define CXX_DRIVER_H

#include <cxx-ast.h>
#include <cxx-scope.h>
#include <getopt.h>

// Options for command line arguments
typedef enum 
{
	OPTION_UNDEFINED = 1024,
	OPTION_VERSION
} COMMAND_LINE_OPTIONS;

// Kind of source 
typedef enum source_kind_tag
{
	SOURCE_KIND_UNKNOWN = 0,
	SOURCE_KIND_NOT_PREPROCESSED,
	SOURCE_KIND_PREPROCESSED
} source_kind_t;

typedef enum source_language_tag
{
	SOURCE_LANGUAGE_UNKNOWN = 0,
	SOURCE_LANGUAGE_C,
	SOURCE_LANGUAGE_CXX,
	SOURCE_LANGUAGE_LINKER_DATA
} source_language_t;

extern char* source_language_names[];

// File extensions table
struct extensions_table_t
{
	char* name;
	source_language_t source_language;
	source_kind_t source_kind;
};

// Valid command line parameters
extern struct option getopt_long_options[];

// Represents one translation unit
typedef struct translation_unit_tag
{
	char* input_filename;
	char* output_filename;

	AST parsed_tree;
	scope_t* global_scope;
} translation_unit_t;

// Configuration file directives
struct configuration_directive_t
{
	char* name;
	int (*funct)(char* value);
};

// Global compiler options
typedef struct compilation_options_tag
{
	// For further use
	int argc;
	char** argv;
	char* exec_basename;

	// Options
	char verbose;
	char keep_files;
	char check_dates;
	char do_not_link;
	char debug_level;
	
	// Source language information
	source_language_t source_language;

	// List of translation units
	translation_unit_t** translation_units;
	int num_translation_units;

	// Result of the execution
	int execution_result;

	// Config file
	char* config_file;

	// This makes things non reentrant
	scope_t* global_scope;

	char* linked_output_filename;
	// Toolchain information
	char* preprocessor_name;
	char** preprocessor_options;

	char* native_compiler_name;
	char** native_compiler_options;

	char* linker_name;
	char** linker_options;
} compilation_options_t;

extern compilation_options_t compilation_options;

extern int yydebug;
extern int mcxx_flex_debug;

extern int yyparse(AST* parsed_tree);

struct extensions_table_t*
fileextensions_lookup (register const char *str, 
		register unsigned int len);

struct configuration_directive_t*
configoptions_lookup (register const char *str, 
		register unsigned int len);

void parse_arguments(int argc, char* argv[]);

extern int num_seen_file_names;
extern char** seen_file_names;


#endif // CXX_DRIVER_H
