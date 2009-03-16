#include "hlt-transform.hpp"

using namespace TL::HLT;

BaseTransform::operator Source()
{
    return this->get_source();
}

BaseTransform::operator std::string()
{
    return this->get_source();
}
