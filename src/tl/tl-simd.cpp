#include "tl-simd.hpp"


using namespace TL;
using namespace TL::SIMD;

namespace TL
{
    namespace SIMD
    {
        GenericFunctions generic_functions;
    }
}

const char* ReplaceSrcGenericFunction::prettyprint_callback (AST a, void* data)
{
    return ReplaceSrcIdExpression::prettyprint_callback(a, data);
}

void ReplaceSrcGenericFunction::add_replacement(Symbol sym, const std::string& str)
{
    ReplaceSrcIdExpression::add_replacement(sym, str);
}

void ReplaceSrcGenericFunction::add_this_replacement(const std::string& str)
{
    ReplaceSrcIdExpression::add_this_replacement(str);
}

Source ReplaceSrcGenericFunction::replace(AST_t a) const
{
    return ReplaceSrcIdExpression::replace(a);
}

std::string ReplaceSrcGenericFunction::get_device_name()
{
    return _device_name;
}

void ReplaceSrcGenericFunction::set_width(int width)
{
    _width = width;
}


Source GenericFunctions::get_pending_specific_functions(
        ReplaceSrcGenericFunction& replace)
{
    Source result;

    for (function_map_t::iterator it = _function_map.begin();
            it != _function_map.end();
            it ++)
    {
        result 
            << it->second->get_all_pend_spec_func_def(replace);
    }

    return result;
}

void GenericFunctions::add_simd(Symbol scalar_func_sym, Symbol generic_func_sym)
{
    _function_map[scalar_func_sym] = new GenericSimdFunctionInfo(scalar_func_sym, generic_func_sym);
}

void GenericFunctions::add_naive(Symbol func_sym)
{
    _function_map[func_sym] = new GenericNaiveFunctionInfo(func_sym);
}

void GenericFunctions::add_specific_definition(
        Symbol func_sym, 
        std::string device_name, 
        int width, 
        bool prettyprinted)
{
    function_map_t::iterator it = _function_map.find(func_sym);

    if (it == _function_map.end())
    {
        running_error("GenericFunctionInfo is missing");
    }

    it->second->add_specific_function_definition(device_name, width, prettyprinted);
}

bool GenericFunctions::contains_generic_definition(Symbol func_sym)
{
    return _function_map.find(func_sym) != _function_map.end();
}

bool GenericFunctions::contains_specific_definition(Symbol func_sym, std::string device_name, int width)
{
    function_map_t::iterator it = _function_map.find(func_sym);

    if (it == _function_map.end())
        return false;

    GenericFunctionInfo* gen_func = it->second;

    return gen_func->has_specific_definition(device_name, width);
}


SpecificFunctionInfo::SpecificFunctionInfo(int width, bool prettyprinted)
{
    _width = width;
    _prettyprinted = prettyprinted;
}

bool SpecificFunctionInfo::is_width(int width)
{
    return _width == width;
}

int SpecificFunctionInfo::get_width()
{
    return _width;
}

bool SpecificFunctionInfo::is_prettyprinted()
{
    return _prettyprinted;
}

void SpecificFunctionInfo::set_prettyprinted(bool prettyprinted)
{
    _prettyprinted = prettyprinted;
}


GenericFunctionInfo::GenericFunctionInfo(Symbol func_sym)
    : _func_sym(func_sym)
{
    if (_func_sym.is_invalid())
    {
        running_error("Expected a valid Symbol");
    }

    if (!_func_sym.is_function())
    {
        running_error("Expected a function Symbol");
    }
}


bool GenericFunctionInfo::has_specific_definition(
        std::string device_name, int width)
{
    for (specific_functions_t::iterator it = _specific_functions.find(device_name);
            it != _specific_functions.end();
            it++)
    {
        if (it->second.is_width(width))
            return true;
    }

    return false;
}

void GenericFunctionInfo::add_specific_function_definition(
        std::string device_name,
        int width,
        bool prettyprinted)
{
    SpecificFunctionInfo spec_func(width, prettyprinted);
    _specific_functions.insert(
            std::pair<std::string, SpecificFunctionInfo> (
                device_name, SpecificFunctionInfo(width, prettyprinted)));
}

Source GenericFunctionInfo::get_all_pend_spec_func_def(
        ReplaceSrcGenericFunction& replace)
{
    Source result;

    for (specific_functions_t::iterator it = _specific_functions.find(replace.get_device_name());
            it != _specific_functions.end();
            it++)
    {
        SpecificFunctionInfo& spec_fun = it->second;
        if (!spec_fun.is_prettyprinted())
        {
            spec_fun.set_prettyprinted(true);
            result << this->get_specific_function_definition(spec_fun, replace);    
        }
    }

    return result;
}


GenericSimdFunctionInfo::GenericSimdFunctionInfo(Symbol scalar_func_sym, Symbol generic_func_sym)
    : GenericFunctionInfo(scalar_func_sym), _generic_func_sym(generic_func_sym)
{
}

Source GenericSimdFunctionInfo::get_specific_function_definition(
        SpecificFunctionInfo& spec_func,
        ReplaceSrcGenericFunction& replace)
{
    std::stringstream spec_func_name;

    spec_func_name
        << _generic_func_sym.get_name()
        << replace.get_device_name()
        << "_"
        << spec_func.get_width()
        ;

    replace.set_width(spec_func.get_width());
    replace.add_replacement(_generic_func_sym, spec_func_name.str());
    return replace.replace_simd_function(_generic_func_sym, replace.get_scope_link());    
}


GenericNaiveFunctionInfo::GenericNaiveFunctionInfo(Symbol func_sym)
    : GenericFunctionInfo(func_sym)
{
}

Source GenericNaiveFunctionInfo::get_specific_function_definition(
        SpecificFunctionInfo& spec_func,
        ReplaceSrcGenericFunction& replace)
{
    replace.set_width(spec_func.get_width());
    return replace.replace_naive_function(_func_sym, replace.get_scope_link());
}


