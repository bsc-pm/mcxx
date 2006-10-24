#ifndef CXX_DYNINIT
#define CXX_DYNINIT

typedef void (*dynamic_initializer_t)(void);

void register_dynamic_initializer(dynamic_initializer_t dynamic_initializer);
void run_dynamic_initializers(void);

#endif // CXX_DYNINIT
