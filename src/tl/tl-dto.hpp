#ifndef TL_DTO_HPP
#define TL_DTO_HPP

#include <string>
#include <map>
#include "tl-object.hpp"

namespace TL
{
	class DTO 
	{
		private:
			typedef std::map<std::string, Object*> DTO_inner;
			DTO_inner _dto;
            Undefined _undefined;
		public :
			Object& operator[](const std::string& str)
			{
				DTO_inner::iterator it = _dto.find(str);
				if (it == _dto.end())
				{
					return _undefined;
				}
				else
				{
					return *(it->second);
				}
			}

			void set_object(const std::string& str, Object& obj)
			{
				_dto[str] = &obj;
			}
	};
}

#endif // TL_DTO_HPP
