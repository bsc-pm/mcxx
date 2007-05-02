#include "tl-omptransform.hpp"

namespace TL
{
    IdExpression OpenMPTransform::warn_unreferenced_data(IdExpression id_expr)
    {
        std::cerr << "Warning: Entity '" << id_expr.prettyprint() << "' in " << id_expr.get_ast().get_locus() 
            << " is not referenced in the body of the construct" << std::endl;
        return id_expr;
    }

    IdExpression OpenMPTransform::warn_no_data_sharing(IdExpression id_expr)
    {
        std::cerr << "Warning: '" << id_expr.prettyprint() << "' in " << id_expr.get_ast().get_locus() 
            << " does not have a data sharing attribute and 'default(none)' was specified. "
            << "It will be considered shared." << std::endl;
        return id_expr;
    }
}
