#include "tl-omp-nanox.hpp"

namespace TL { namespace Nanox {

    void OMPTransform::single_postorder(PragmaCustomConstruct ctr)
    {
        Source transform_code;

        Source bool_type;
        C_LANGUAGE()
        {
            bool_type << "_Bool";
        }
        CXX_LANGUAGE()
        {
            bool_type << "bool";
        }

        transform_code
            << "{"
            << bool_type << " single_guard;"
            << "nanos_err_t err = nanos_single_guard(&single_guard);"
            << "if (err != NANOS_OK) nanos_handle_error(err);"

            << "if (single_guard)"
            << "{"
            <<     ctr.get_statement()
            << "}"

            // Final barrier of the whole team
            << "err = nanos_team_barrier();"

            << "if (err != NANOS_OK) nanos_handle_error(err);"
            << "}"
            ;

        AST_t transform_tree 
            = transform_code.parse_statement(ctr.get_ast(), ctr.get_scope_link());

        ctr.get_ast().replace(transform_tree);
    }

}}

