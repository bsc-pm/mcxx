#ifndef PRESCANNER_PROCESS_DECLS_H
#define PRESCANNER_PROCESS_DECLS_H

#include <stdio.h>

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef
struct prescanner_tag {
	FILE* output_file;
	FILE* input_file;
	char* output_filename;
	char* input_filename;
	int width;
	char debug;
	char append;
	char pad_strings;
	char openmp_processing;
    char quiet;

	int num_include_directories;
	char** include_directories;

	char* output_include_directory;
} prescanner_t;

MCXX_END_DECLS

#endif // PRESCANNER_PROCESS_DECLS_H
