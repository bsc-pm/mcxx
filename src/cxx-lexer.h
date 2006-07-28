#ifndef CXX_LEXER_H
#define CXX_LEXER_H

typedef struct token_atrib_tag 
{
	char* token_text;
	int token_line;
} token_atrib_t;

struct scan_file_descriptor 
{
	char in_include_file;
	char* filename;

	// Current filename due to include lines
	char* current_filename;
	int line_number;
	FILE* file_descriptor;
	struct yy_buffer_state* scanning_buffer;

};

extern struct scan_file_descriptor scanning_now;

int open_file_for_scanning(char* scanned_filename, char* input_filename);

#endif // CXX_LEXER_H
