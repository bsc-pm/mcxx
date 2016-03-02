
 At the moment the branch `logger` implements the following interface to be used from C.


        
        locus_t locus_file(const char* filename);
        locus_t locus_file_line(const char *filename, int line);
        locus_t locus_file_line_col(const char* filename, int line, int col);
        locus_t locus_none(void);
        
        locus_t locus_of_ast(AST a);
        
        void log_info(locus_t, const char* message, ...);
        void log_warn(locus_t, const char* message, ...);
        void log_error(locus_t, const char* message, ...) NORETURN;
        
        void log_push_context(locus_t, const char* message, ...);
        void log_pop_context(void);
        
        char log_is_top_level(void);
        
        void log_flush_to_stderr(void);
        
        const char* locus_print(locus_t l);