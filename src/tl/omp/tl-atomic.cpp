#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::atomic_postorder(OpenMP::AtomicConstruct atomic_construct)
    {
        // TODO - An atomic can be implemented better
        Source critical_source;

        Statement critical_body = atomic_construct.body();

        critical_source
            << "{"
            <<   "static nth_word_t default_mutex_var;"
            //                    <<   "extern void nthf_spin_lock_(void*);"
            //                    <<   "extern void nthf_spin_unlock_(void*);"
            <<   "nthf_spin_lock_(&default_mutex_var);"
            <<   critical_body.prettyprint()
            <<   "nthf_spin_unlock_(&default_mutex_var);"
            << "}"
            ;

        AST_t atomic_tree = critical_source.parse_statement(atomic_construct.get_ast(),
                atomic_construct.get_scope_link());

        atomic_construct.get_ast().replace(atomic_tree);
    }
}
