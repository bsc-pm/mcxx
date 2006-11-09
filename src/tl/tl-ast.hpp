#ifndef TL_AST_HPP
#define TL_AST_HPP

#include <typeinfo>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include "cxx-ast.h"
#include "cxx-prettyprint.h"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"

namespace TL
{
	class ScopeLink;

	class AST_t : public Object
	{
		protected:
			AST _ast;
			static void tree_iterator(const AST_t& a, ObjectList<AST_t>& result);
			tl_type_t* get_extended_attribute(const std::string& name) const;

			static AST get_translation_unit(AST node);
			static void prepend_list(AST orig_list, AST prepended_list);
			static void append_list(AST orig_list, AST appended_list);
			static void relink_parent(AST previous_child, AST new_child);

			static bool is_extensible_block(AST node);
			static AST get_list_of_extensible_block(AST node);
		public:
			/*
			 * Constructor
			 */
			AST_t()
				: _ast(NULL)
			{
			}

			AST_t(AST _wrapped_tree)
				: _ast(_wrapped_tree)
			{
			}

            AST_t(const Object& obj)
			{
				const AST_t* cast = dynamic_cast<const AST_t*>(&obj);

				if (cast == NULL)
				{
					if (typeid(obj) != typeid(const Undefined&))
					{
						std::cerr << "Bad initialization of AST_t" << std::endl;
					}

					this->_ast = NULL;
				}
				else
				{
					this->_ast = cast->_ast;
				}
			}

			AST_t(const AST_t& ast)
				: _ast(ast._ast)
			{
			}


			/*
			 * Destructor
			 */
			virtual ~AST_t()
			{
			}

			bool operator<(AST_t n) const;
			bool operator==(AST_t n) const;
			bool operator!=(AST_t n) const;
			AST_t& operator=(AST_t n);

			std::string prettyprint() const;

			void replace_with(AST_t ast);

			AST_t duplicate() const;

			ObjectList<AST_t> depth_subtrees();

			std::string internal_ast_type() const;

			virtual bool is_ast() const
			{
				return true;
			}

			void prepend_to_translation_unit(AST_t t);
			void append_to_translation_unit(AST_t t);

			void append(AST_t t);
			void prepend(AST_t t);

			AST_t get_enclosing_block();
			AST_t get_enclosing_function_definition();

			void prepend_sibling_function(AST_t t);
			void append_sibling_function(AST_t t);

			void replace_text(const std::string& str);

			friend class Type;
            friend class Scope;
            friend class ScopeLink;
            friend class DepthTraverse;
	};
}

#endif // TL_AST_HPP
