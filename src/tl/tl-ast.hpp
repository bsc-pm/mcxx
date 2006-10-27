#ifndef TL_AST_HPP
#define TL_AST_HPP

#include <string>
#include <vector>
#include "cxx-ast.h"
#include "cxx-prettyprint.h"
#include "tl-object.hpp"
#include "tl-predicate.hpp"

namespace TL
{
	class ScopeLink;

	class AST_t;
	typedef std::vector<AST_t*> AST_list_t;

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
			static void append_list(AST orig_list, AST appended_list);
			static void relink_parent(AST previous_child, AST new_child);
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

			void append_to_translation_unit(AST_t* t);

            friend class Type;
			friend class Object;
			friend class Source;
			friend class Scope;
			friend class ScopeLink;
			friend class CompilerPhaseRunner;
	};

}

#endif // TL_AST_HPP
