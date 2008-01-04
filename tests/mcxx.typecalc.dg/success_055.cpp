template<typename _ForwardIterator, typename _Generator>
    void
generate(_ForwardIterator __first, _ForwardIterator __last,
        _Generator __gen)
{
    ;
    for ( ; __first != __last; ++__first)
        *__first = __gen();
}
