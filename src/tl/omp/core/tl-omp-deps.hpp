#ifndef TL_OMP_DEPS_HPP
#define TL_OMP_DEPS_HPP

// This header does not contain anything at the moment, it is just here for
// consistency with the other files

#define BITMAP(x) (1<<x)

namespace TL { namespace OpenMP {

enum DependencyDirection
{
    DEP_DIR_UNDEFINED = 0,
    // Input dependence
    DEP_DIR_INPUT = BITMAP(1),
    // Output dependence
    DEP_DIR_OUTPUT = BITMAP(2),
    // Inout dependence
    DEP_DIR_INOUT = DEP_DIR_INPUT | DEP_DIR_OUTPUT,
    // Firstprivatized dependence
    DEP_FIRSTPRIVATE = BITMAP(3),
    // Reduction dependences
    DEP_REDUCTION = BITMAP(4),
};

} }

#undef BITMAP

#endif // TL_OMP_DEPS_HPP
