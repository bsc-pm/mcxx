/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
class File;

class String
{
    public:
     String(const String& f);
     String(const File& f);
};

void kk(const String& a);
class File
{
    public:

    operator String() const;

    void f() const
    {
        // Copy-initialization is ambiguous!!
        String a = *this;
    }
};
