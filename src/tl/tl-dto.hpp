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
		public :
			Object* operator[](const std::string& str) const
			{
				DTO_inner::const_iterator it = _dto.find(str);
				if (it == _dto.end())
				{
					return NULL;
				}
				else
				{
					return it->second;
				}
			}

			void set_object(const std::string& str, Object* obj)
			{
				_dto[str] = obj;
			}
	};
}

#endif // TL_DTO_HPP
