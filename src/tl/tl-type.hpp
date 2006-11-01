#ifndef TL_TYPE_HPP
#define TL_TYPE_HPP

#include <string>
#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "cxx-scope.h"

namespace TL
{
    class Scope;
	class Type : public Object
	{
		private:
			type_t* _type_info;
			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}

			static std::string get_type_name_str(type_t* type, const std::string& symbol_name);
            static void get_type_name_str_internal(type_t* type_info, 
                    const std::string &symbol_name, std::string& left, std::string& right);
            static std::string get_cv_qualifier_str(type_t* type_info);
			static std::string get_simple_type_name_str_internal(simple_type_t* simple_type);
			static std::string get_simple_type_name_str(type_t* simple_type);
            static bool declarator_needs_parentheses(type_t* type_info);
            static std::string get_declaration_str_internal(type_t* type_info, 
                    const std::string& symbol_name, bool semicolon);

		public:
			Type(type_t* type_info)
				: _type_info(type_info)
			{
			}

			// Type(Type& type)
			// 	: _type_info(type._type_info)
			// {
			// }

			virtual ~Type()
			{
			}

			virtual bool is_type() const
			{
				return true;
			}

			std::string get_simple_declaration_str(const std::string& symbol_name) const;
			std::string get_parameter_declaration_str(const std::string& symbol_name) const;

			Type duplicate();
			Type get_pointer_to();
			Type get_array_to(AST_t expression_array, Scope scope);

			bool operator==(Type t);
			Type& operator=(Type t);
			bool operator<(Type t);

			friend class Symbol;
	};
}

#endif // TL_TYPE_HPP
