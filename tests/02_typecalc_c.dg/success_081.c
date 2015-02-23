/*
<testinfo>
test_generator="config/mercurium run"
test_CFLAGS="-std=c99 -O0 -fno-inline"
</testinfo>
*/

void nonexplicit_extern_def(void);
inline void nonexplicit_extern_def(void)
{
}

extern void explicit_extern_def(void);
inline void explicit_extern_def(void)
{
}

inline void nonexplicit_extern_fwd_decl(void);

int main(int argc, char *argv[])
{
    nonexplicit_extern_def();
    explicit_extern_def();
    nonexplicit_extern_fwd_decl();
    return 0;
}

void nonexplicit_extern_fwd_decl(void)
{
}
