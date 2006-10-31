#ifndef TL_AST_HPP
#define TL_AST_HPP

#include <string>
#include <set>
#include <map>
#include <vector>
#include "cxx-ast.h"
#include "cxx-prettyprint.h"
#include "tl-object.hpp"
#include "tl-predicate.hpp"

namespace TL
{
	class ScopeLink;

	class AST_t;

	class AST_list_t : public std::vector<AST_t*>
	{
		public:
	};

	class AST_set_t : public std::set<AST_t*>
	{
		private:
			void initialize_from_list(const AST_list_t& list)
			{
				for (AST_list_t::const_iterator it = list.begin();
						it != list.end();
						it++)
				{
					this->insert(*it);
				}
			}
		public:
			AST_set_t& operator=(const AST_list_t& list)
			{
				initialize_from_list(list);
				return (*this);
			}

			AST_set_t(const AST_list_t& list)
			{
				initialize_from_list(list);
			}
	};

	class AST_t : public Object
	{
		private:
			/*
			 * Constructor
			 */
			AST_t(AST _wrapped_tree)
				: _ast(_wrapped_tree)
			{
			}

			static AST get_translation_unit(AST node);
			static void prepend_list(AST orig_list, AST prepended_list);
			static void append_list(AST orig_list, AST appended_list);
			static void relink_parent(AST previous_child, AST new_child);

			static bool is_extensible_block(AST node);
			static AST get_list_of_extensible_block(AST node);

			static std::map<AST, AST_t*> ast_cache;
			static AST_t* wrap_ast(AST ast);
		protected:
			AST _ast;
			static void tree_iterator(const AST_t& a, const Predicate& p, AST_list_t& result);
			tl_type_t* get_extended_attribute(const std::string& name) const;
		public:

			/*
			 * Destructor
			 */
			virtual ~AST_t()
			{
			}

			std::string prettyprint() const;

			void replace_with(AST_t* ast);

			AST_t* duplicate() const;

			AST_list_t get_all_subtrees_predicate(const Predicate& p) const;

			std::string internal_ast_type() const;

			virtual bool is_ast() const
			{
				return true;
			}

			void prepend_to_translation_unit(AST_t* t);
			void append_to_translation_unit(AST_t* t);

			void append(AST_t* t);
			void prepend(AST_t* t);

			AST_t* get_enclosing_block();
			AST_t* get_enclosing_function_definition();

            friend class Type;
			friend class Object;
			friend class Source;
			friend class Scope;
			friend class ScopeLink;
			friend class CompilerPhaseRunner;
			friend class Traverse;
	};

}

#endif // TL_AST_HPP
