template <typename _CharT>
struct char_traits
{
	typedef _CharT char_type;
	typedef _CharT *double_pointer;

	static const char_type* find();
};

template<typename _CharT>
const typename char_traits<_CharT>::char_type* char_traits<_CharT>::find()
{
}
