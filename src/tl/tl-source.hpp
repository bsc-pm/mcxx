#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include <string>
#include "tl-object.hpp"

namespace TL
{
	class Source : public Object
	{
		private:
			std::string _code;
		public :

			virtual bool is_source() const
			{
				return true;
			}
	};
}

#endif // TL_SOURCE_T_HPP
