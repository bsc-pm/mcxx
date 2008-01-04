template <typename _CharT, typename _Traits = _CharT*, typename _Alloc = _Traits**>
struct basic_istream
{
};

template <typename _CharT, typename _Traits = _CharT*, typename _Alloc = _Traits**>
struct basic_string
{
};

template<typename _CharT, typename _Traits, typename _Alloc>
  basic_istream<_CharT, _Traits>&
  operator>>(basic_istream<_CharT, _Traits>& __is,
      basic_string<_CharT, _Traits, _Alloc>& __str);

template<>
  basic_istream<char>&
  operator>>(basic_istream<char>& __is, basic_string<char>& __str);
