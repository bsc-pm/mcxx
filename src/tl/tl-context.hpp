#ifndef TL_CONTEXT_HPP
#define TL_CONTEXT_HPP

#include <string>

namespace TL
{
	class Context
	{
		private:
			bool _in_function;
			bool _function_is_member;
			bool _function_is_template;
			int _line;
		public:
			bool in_function();
			bool function_is_member();
			bool function_is_template();

			std::string unqualified_function_name();
			std::string qualification_function_name();

			std::string function_name();

			std::string file_name();
			int line();
	};
}

#endif // TL_CONTEXT_HPP
