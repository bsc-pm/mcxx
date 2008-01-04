template <typename _T>
void f(_T t);

template <typename _T>
void f(_T* t);

template <>
void f(int) { }

template <>
void f(int*) { }

template <>
void f<int*>(int*) { }
