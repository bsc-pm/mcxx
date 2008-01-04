typedef unsigned int size_t;

void* operator new(size_t t);

void f()
{
    void * k = ::operator new(10);
}
