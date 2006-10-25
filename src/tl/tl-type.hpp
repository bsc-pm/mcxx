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
			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}
		public :
			Type(type_t* type_info)
				: _type_info(type_info)
			{
			}

			virtual ~Type()
			{
			}

			virtual bool is_type() const
			{
				return true;
			}
	};
}

#endif // TL_TYPE_HPP
