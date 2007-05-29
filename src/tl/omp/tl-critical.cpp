#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::critical_postorder(OpenMP::CriticalConstruct critical_construct)
    {
        Source critical_source;

        OpenMP::Directive directive = critical_construct.directive();
        Statement critical_body = critical_construct.body();
        ScopeLink scope_link = critical_construct.get_scope_link();

        OpenMP::Clause region_name = directive.parameter_clause();

        std::string mutex_variable;

        if (!region_name.is_defined())
        {
            mutex_variable = "_nthf_unspecified_critical";
        }
        else
        {
            ObjectList<IdExpression> id_expressions = region_name.id_expressions(TL::ALL_FOUND_SYMBOLS);
            IdExpression head = id_expressions[0];

            mutex_variable = "_nthf_"  + head.prettyprint();
        }

        critical_source
            << "{"
            //                    <<   "extern void nthf_spin_lock_(void*);"
            //                    <<   "extern void nthf_spin_unlock_(void*);"
            <<   "nthf_spin_lock_(&" << mutex_variable << ");"
            <<   critical_body.prettyprint()
            <<   "nthf_spin_unlock_(&" << mutex_variable << ");"
            << "}"
            ;

        define_global_mutex(mutex_variable, critical_construct.get_ast(),
                critical_construct.get_scope_link());

        AST_t critical_tree = critical_source.parse_statement(critical_construct.get_ast(),
                critical_construct.get_scope_link());

        critical_construct.get_ast().replace(critical_tree);
    }

    void OpenMPTransform::define_global_mutex(std::string mutex_variable, AST_t ref_tree, ScopeLink sl)
    {
        if (criticals_defined.find(mutex_variable) == criticals_defined.end())
        {
            // Now declare, if not done before
            Source critical_mutex_def_src, weak_attr;

            critical_mutex_def_src <<
                "nth_word_t " << weak_attr << " " << mutex_variable << " = 0;"
                ;

            CXX_LANGUAGE()
            {
                // We need this because of the One Definition Rule
                weak_attr 
                    << "__attribute__((weak))"
                    ;
            }

            // AST_t translation_unit = critical_construct.get_ast().get_translation_unit();
            // Scope scope_translation_unit = scope_link.get_scope(translation_unit);

            AST_t critical_mutex_def_tree = critical_mutex_def_src.parse_global(ref_tree, sl);

            ref_tree.prepend_sibling_function(critical_mutex_def_tree);

            criticals_defined.insert(mutex_variable);
        }
    }
}
