/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <class T> T* f(T (*p)(T));

int g(int);
float g(char);

int* pi = f(g); 
