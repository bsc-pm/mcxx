#ifndef TL_TYPE_HPP
#define TL_TYPE_HPP

#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "cxx-scope.h"

namespace TL
{
	class Symbol;
	class Type : public Object
	{
		private:
			type_t* _type_info;
		public :
			Type(type_t* type_info)
				: _type_info(type_info)
			{
			}

			virtual Object* attributes(const std::string& name) const
			{
				return NULL;
			}

			virtual ~Type()
			{
			}

			virtual bool is_type()
			{
				return true;
			}
	};
}

#endif // TL_TYPE_HPP
