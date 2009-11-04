#ifndef CXX_CONFIGFILE_LEXER_H
#define CXX_CONFIGFILE_LEXER_H

int open_configuration_file_for_scan(const char* scanned_filename);
void close_configuration_file_for_scan(void);

int configfilelex(void);

#endif // CXX_CONFIGFILE_LEXER_H
