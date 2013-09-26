/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename ...T>
void f(T...);

template <typename ...T>
void f(T... t);

template <typename T>
void g(T...);

template <typename T>
void g(T t, ...);
