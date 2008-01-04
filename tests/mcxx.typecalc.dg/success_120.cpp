typedef __builtin_va_list va_list;

void f(char * __fmt, ...)
{
    va_list __args;
    __builtin_va_start(__args, __fmt);
}
