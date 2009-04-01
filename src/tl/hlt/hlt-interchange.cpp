#include "hlt-interchange.hpp"
#include "hlt-exception.hpp"
#include <algorithm>

using namespace TL::HLT;

LoopInterchange::LoopInterchange(ForStatement for_stmt, ObjectList<int> permutation)
     : _for_nest(for_stmt), _permutation(permutation), _is_identity(false)
{
    if (!is_valid_permutation(permutation, _is_identity)
            || (permutation.size() != _for_nest.get_nest_list().size()))
    {
        throw HLTException(for_stmt, 
                "invalid permutation specification");
    }

    // We could do sinking to achieve perfection
    if (!_for_nest.is_perfect())
    {
        throw HLTException(for_stmt, 
                "interchange can only be applied in perfect loop nests");
    }
}

TL::Source LoopInterchange::get_source()
{
    return do_interchange();
}

TL::Source LoopInterchange::do_interchange()
{
    Source result;

    Source* current = new Source();

    result
        << (*current)
        ;

    ObjectList<ForStatement> loop_nest_list = _for_nest.get_nest_list();

    for (ObjectList<int>::iterator it = _permutation.begin();
            it != _permutation.end();
            it++)
    {
        ForStatement& current_for_stmt = loop_nest_list[(*it)];

        Source* inner = new Source();

        (*current)
            << "for("
            << current_for_stmt.get_iterating_init().prettyprint() 
            << current_for_stmt.get_iterating_condition() << ";"
            << current_for_stmt.get_iterating_expression()
            << ")"
            << (*inner)
            ;

        // This is not wrong, every current is referenced in another current or in result
        delete current;
        current = inner;
    }

    // Now add the innermost loop body
    ForStatement& innermost_stmt = loop_nest_list[loop_nest_list.size() - 1];
    (*current)
        << innermost_stmt.get_loop_body()
        ;

    // See above why this is not wrong
    delete current;
    return result;
}

struct EpsilonGenerator
{
    private:
        int _n;
    public:
        EpsilonGenerator(int n)
            : _n(n) { }

        int operator()()
        {
            return (_n++);
        }
};

bool LoopInterchange::is_valid_permutation(ObjectList<int> permutation, bool &identity)
{
    identity = false;

    // Create a range
    ObjectList<int> range;
    std::generate_n(std::back_inserter(range), permutation.size(), EpsilonGenerator(1));

    if (std::equal(range.begin(), range.end(), permutation.begin()))
    {
        identity = true;
        return true;
    }

    std::sort(permutation.begin(), permutation.end());

    return std::equal(range.begin(), range.end(), permutation.begin());
}
