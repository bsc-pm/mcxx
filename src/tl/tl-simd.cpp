#include "tl-simd.hpp"
#include "cxx-utils.h"
#include <algorithm>


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

std::string ReplaceSrcGenericFunction::get_device_name() const
{
    return _device_name;
}

int ReplaceSrcGenericFunction::get_width() const
{
    return _width;
}

void ReplaceSrcGenericFunction::set_width(const int width)
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
            << it->second.get_all_pend_spec_func_def(replace);
    }

    return result;
}

Source GenericFunctions::get_pending_specific_declarations(
        ReplaceSrcGenericFunction& replace)
{
    Source result;

    for (function_map_t::iterator it = _function_map.begin();
            it != _function_map.end();
            it ++)
    {
        result 
            << it->second.get_all_pend_spec_func_decl(replace);
    }

    return result;
}

//Using add_generic_function you indicate that scalar_func_sym is actually in the code
void GenericFunctions::add_generic_function(const Symbol& scalar_func_sym)
{
    DEBUG_CODE()
    {
        std::cerr << "SIMD: Adding generic function '" 
            << scalar_func_sym.get_name()
            << "'"
            << std::endl;
    }

    function_map_t::iterator it =  _function_map.find(scalar_func_sym);

    //If Generic Function exists then error
    if (it == _function_map.end())
    {
        _function_map.insert(std::make_pair(scalar_func_sym, GenericFunctionInfo(scalar_func_sym)));
    }
    else
    {
        DEBUG_CODE()
        {
            std::cerr << "SIMD: Ignoring generic function for scalar function '"
                << scalar_func_sym.get_name()
                << "'. Another generic function was added before." 
                << std::endl;
        }
    }
}

void GenericFunctions::add_generic_function(const Symbol& scalar_func_sym, const Symbol& simd_func_sym)
{
    DEBUG_CODE()
    {
        std::cerr << "SIMD: Adding generic function '" 
            << scalar_func_sym.get_name()
            << "', simd_func_name: '"
            << simd_func_sym.get_name()
            << "'" 
            <<std::endl;
    }

    function_map_t::iterator it =  _function_map.find(scalar_func_sym);

    if (it == _function_map.end())
    {
        _function_map.insert(std::make_pair(scalar_func_sym, GenericFunctionInfo(scalar_func_sym, simd_func_sym)));
    }
}

void GenericFunctions::add_specific_definition(
        const Symbol& scalar_func_sym, 
        specific_function_kind_t spec_func_kind,
        const std::string& device_name, 
        const int width, 
        const bool needs_prettyprint,
        const bool needs_def_decl,
        const std::string default_func_name)
{
    function_map_t::iterator it = _function_map.find(scalar_func_sym);

    //If Generic Function does not exist we create it
    if (it == _function_map.end())
    {
        this->add_generic_function(scalar_func_sym);
        it = _function_map.find(scalar_func_sym);
    }
    
    GenericFunctionInfo &gen_func_info(it->second);

    //needs_prettyprint == true means that the symbol is in the source code.
    //Then we activate prettyprint in all specific versions
    if (needs_prettyprint == true)
    {
        gen_func_info.activate_prettyprint(device_name, width);
    }

    if (spec_func_kind == AUTO)
    {
        if(gen_func_info.has_simd_symbol())
        {
            spec_func_kind = SIMD;
        }
        else
        {
            spec_func_kind = NAIVE;
        }
    }

    if (!contains_specific_definition(scalar_func_sym, spec_func_kind, device_name, width))
    {
        if ((spec_func_kind == ARCH_DEFAULT))
        {
            gen_func_info.add_specific_function_definition( 
                    default_func_name, spec_func_kind, device_name, width, needs_prettyprint, needs_def_decl);
        }
        else if (spec_func_kind == COMPILER_DEFAULT)
        {
            gen_func_info.add_specific_function_definition( 
                    default_func_name, spec_func_kind, device_name, width, needs_prettyprint, needs_def_decl);
        }
        else
        {
            std::stringstream func_name;
            func_name
                << "_"
                << scalar_func_sym.get_name()
                << "_"
                << device_name
                << "_"
                << width
                ;

            gen_func_info.add_specific_function_definition(
                    func_name.str(), spec_func_kind, device_name, width, needs_prettyprint, needs_def_decl);
        }
    }
}

void GenericFunctions::add_specific_definition(
        const Symbol& scalar_func_sym, 
        const Symbol& simd_func_sym, 
        specific_function_kind_t spec_func_kind,
        const std::string& device_name, 
        const int width, 
        const bool needs_prettyprint,
        const bool needs_def_decl,
        const std::string default_func_name)
{
    add_generic_function(scalar_func_sym, simd_func_sym);

    add_specific_definition(
            scalar_func_sym,
            spec_func_kind,
            device_name,
            width,
            needs_prettyprint,
            needs_def_decl,
            default_func_name);
}


bool GenericFunctions::contains_generic_definition(const Symbol& scalar_func_sym) const
{
    return _function_map.find(scalar_func_sym) != _function_map.end();
}

bool GenericFunctions::contains_specific_definition(
        const Symbol& scalar_func_sym, 
        const specific_function_kind_t spec_func_kind, 
        const std::string& device_name, 
        const int width) const
{
    function_map_t::const_iterator it = _function_map.find(scalar_func_sym);

    if (it == _function_map.end())
        return false;

    const GenericFunctionInfo& gen_func(it->second);

    return gen_func.has_specific_definition(spec_func_kind, device_name, width);
}

std::string GenericFunctions::get_specific_func_name(
        const Symbol& scalar_func_sym, 
        const std::string& device_name,
        const int width)
{
    DEBUG_CODE()
    {
        std::cerr << "SIMD: Getting specific function name for '" << scalar_func_sym.get_name()
            << ", device: " << device_name
            << ", width: " << width
            << std::endl;
    }

    function_map_t::iterator it = _function_map.find(scalar_func_sym);
    if (it == _function_map.end())
    {
        running_error("error: function Symbol is not a generic function.");
    }


    return it->second.get_better_specific_function(device_name, width).get_name();
}


SpecificFunctionInfo::SpecificFunctionInfo(
        const std::string& spec_func_name, 
        const specific_function_kind_t spec_func_kind, 
        const int width, 
        const bool needs_prettyprint,
        const bool needs_def_decl) 
: _spec_func_name(spec_func_name), _spec_func_kind(spec_func_kind), _width(width),
    _needs_prettyprint(needs_prettyprint), _needs_definition(needs_def_decl), _needs_declaration(needs_def_decl)
{
}

std::string SpecificFunctionInfo::get_name() const
{
    return _spec_func_name;
}

bool SpecificFunctionInfo::is_width(const int width) const
{
    return _width == width;
}

int SpecificFunctionInfo::get_width() const
{
    return _width;
}

bool SpecificFunctionInfo::is_kind(const specific_function_kind_t spec_func_kind) const
{
    return spec_func_kind == _spec_func_kind;
}

bool SpecificFunctionInfo::needs_prettyprint() const
{
    return _needs_prettyprint;
}

bool SpecificFunctionInfo::needs_definition() const
{
    return _needs_definition;
}

bool SpecificFunctionInfo::needs_declaration() const
{
    return _needs_declaration;
}

void SpecificFunctionInfo::set_prettyprint(const bool needs_prettyprint)
{
    _needs_prettyprint = needs_prettyprint;
}

void SpecificFunctionInfo::set_definition(const bool needs_definition)
{
    _needs_definition = needs_definition;
}

void SpecificFunctionInfo::set_declaration(const bool needs_declaration)
{
    _needs_declaration = needs_declaration;
}

Source SpecificFunctionInfo::get_definition(
        const Symbol& scalar_func_sym,
        const Symbol& simd_func_sym,
        ReplaceSrcGenericFunction& replace) const
{
    replace.set_width(_width);

    if (_spec_func_kind == ARCH_DEFAULT)
    {
        return Source(); //ARCH_DEFAULT specific function is already defined");
    }
    else if (_spec_func_kind == COMPILER_DEFAULT)
    {
        replace.add_replacement(simd_func_sym, _spec_func_name);
        return replace.replace(simd_func_sym.get_point_of_definition());
    }
    else if (_spec_func_kind == SIMD)
    {
        return replace.replace_simd_function(simd_func_sym, _spec_func_name);    
    }
    else if (_spec_func_kind == NAIVE)
    {
        return replace.replace_naive_function(scalar_func_sym, _spec_func_name);
    }

    running_error("error: specific function definition has a invalid kind.");
}

Source SpecificFunctionInfo::get_declaration(
        const Symbol& scalar_func_sym,
        const Symbol& simd_func_sym) const
{
    Source func_decl_src, parameter_decl_list;

    if (_spec_func_kind == ARCH_DEFAULT)
    {
        return func_decl_src; //ARCH_DEFAULT specific function is already declared");
    }
    else //COMPILER_DEFAULT || SIMD || NAIVE
    {
        Type func_type = scalar_func_sym.get_type();

        if (!func_type.is_function())
        {
            running_error("error: expected function Symbol");
        }

        ObjectList<Type> type_param_list = func_type.parameters();

        Type func_ret_type = func_type.returns()
            .basic_type()
            .get_vector_to(_width);

        Source static_inline_spec;

        if (_spec_func_kind == NAIVE //NAIVE functions are always static
                || (_spec_func_kind == SIMD && simd_func_sym.is_static())
                || (_spec_func_kind == COMPILER_DEFAULT && simd_func_sym.is_static()))
        {
            static_inline_spec << "static ";
        }

        if (((_spec_func_kind == NAIVE) && scalar_func_sym.is_inline())
                || ((_spec_func_kind == SIMD) && simd_func_sym.is_inline())
                || ((_spec_func_kind == COMPILER_DEFAULT) && simd_func_sym.is_inline()))
        {
            static_inline_spec << "inline ";
        }

        //Ret Type
        func_decl_src
            << static_inline_spec
            << func_ret_type.get_declaration(
                    scalar_func_sym.get_scope(), _spec_func_name)
            << "(" << parameter_decl_list << ");"
            ;

        //Function arguments 
        ObjectList<Type>::iterator it;
        for (it = type_param_list.begin();
                it != type_param_list.end();
                it++)
        {
            if ((_spec_func_kind == NAIVE) || (_spec_func_kind == SIMD))
            {
                Type param_vec_type = it->basic_type()
                .get_vector_to(_width);

                parameter_decl_list.append_with_separator(
                        param_vec_type.get_declaration(
                            scalar_func_sym.get_scope(), ""), ",");
            }
            else
            {
                Type param_vec_type = *it;

                parameter_decl_list.append_with_separator(
                        param_vec_type.get_declaration(
                            scalar_func_sym.get_scope(), ""), ",");
            }
        }   
    }        

    return func_decl_src;
}

bool SpecificFunctionInfo::operator< (const SpecificFunctionInfo& spec_func_info) const
{
    return this->_spec_func_kind < spec_func_info._spec_func_kind;
}


GenericFunctionInfo::GenericFunctionInfo(const Symbol& scalar_func_sym)
    : _scalar_func_sym(scalar_func_sym), _simd_func_sym(NULL)
{
    if (_scalar_func_sym.is_invalid())
    {
        running_error("error: expected a valid scalar Symbol");
    }

    if (!_scalar_func_sym.is_function())
    {
        running_error("error: expected a function Symbol");
    }
}

GenericFunctionInfo::GenericFunctionInfo(
        const Symbol& scalar_func_sym, 
        const Symbol& simd_func_sym)
    : _scalar_func_sym(scalar_func_sym), _simd_func_sym(simd_func_sym)
{
    if (_scalar_func_sym.is_invalid())
    {
        running_error("error: expected a valid scalar Symbol from a scalar function");
    }

    if (!_scalar_func_sym.is_function())
    {
        running_error("error: expected function Symbol from a scalar function");
    }

    if (simd_func_sym.is_invalid())
    {
        running_error("error: expected a valid SIMD Symbol from a generic function");
    }

    if (!_simd_func_sym.is_function())
    {
        running_error("error: expected function Symbol from a generic function");
    }
}


bool GenericFunctionInfo::has_specific_definition(
        const specific_function_kind_t spec_func_kind,
        const std::string& device_name, 
        const int width) const
{
    //Looking for device specific functions (first map)
    for (device_specific_map_t::const_iterator it = _specific_functions.find(device_name);
            it != _specific_functions.end();
            it++)
    {
        //Looking for width specific functions (nested map)
        for (width_specific_map_t::const_iterator it2 = it->second.find(width);
                it2 != it->second.end();
                it2++)
        {
            if ((it2->second.is_kind(spec_func_kind)))
                return true;
        }
    }

    return false;
}

void GenericFunctionInfo::activate_prettyprint(const std::string device_name, const int width) 
{
    //Looking for device specific functions (first map)
    for (device_specific_map_t::iterator it = _specific_functions.find(device_name);
            it != _specific_functions.end();
            it++)
    {
        //Looking for width specific functions (nested map)
        for (width_specific_map_t::iterator it2 = it->second.find(width);
                it2 != it->second.end();
                it2++)
        {
            it2->second.set_prettyprint(true);
        }
    }
}

bool GenericFunctionInfo::has_simd_symbol() const
{
    return _simd_func_sym.is_valid();
}

std::string GenericFunctionInfo::get_simd_func_name() const
{
    return _simd_func_sym.get_name();
}

void GenericFunctionInfo::add_specific_function_definition(
        const std::string scalar_func_name,
        const specific_function_kind_t spec_func_kind,
        const std::string& device_name,
        const int width,
        const bool needs_prettyprint,
        const bool needs_def_decl)
{
    DEBUG_CODE()
    {
        std::cerr << "SIMD: Adding specific function '" << scalar_func_name
            << "', device: '" << device_name
            << "', width: '" << width
            << "', kind: '" << spec_func_kind
            << "', needs_prettyprint: '" << needs_prettyprint
            << "', needs_def_decl: '" << needs_def_decl
            << "'" 
            << std::endl;
    }

    SpecificFunctionInfo spec_func(scalar_func_name, spec_func_kind, width, needs_prettyprint, needs_def_decl);

    device_specific_map_t::iterator it = _specific_functions.find(device_name);

    //Nested Map does not exist (width).
    if (it == _specific_functions.end())
    {
        width_specific_map_t width_specific_map;
        //nested map (width)
        width_specific_map.insert(
                std::pair<int, SpecificFunctionInfo> (
                    width, spec_func));
        //main map (device)
        _specific_functions.insert(
                std::pair<std::string, width_specific_map_t>(
                    device_name, width_specific_map));
    }
    else    //Nested Map exists (width)
    {
        //nested map (width)
        it->second.insert(
                std::make_pair(width, spec_func));
    }
}

//Get all specific function definitions that have not been printed yet.
Source GenericFunctionInfo::get_all_pend_spec_func_def(
        ReplaceSrcGenericFunction& replace)
{
    Source result;

    DEBUG_CODE()
    {
        std::cerr << "SIMD: Getting all pending specific function definition for '" 
            << _scalar_func_sym.get_name() 
            << "'" 
            << std::endl;
    }

    //FIXME? Would this function have to print several widths at a time?
    SpecificFunctionInfo& spec_fun = get_better_specific_function(
            replace.get_device_name(),
            replace.get_width());

    if (spec_fun.needs_prettyprint()
            && spec_fun.needs_definition())
    {
        spec_fun.set_definition(false);
        result << spec_fun.get_definition(_scalar_func_sym, _simd_func_sym, replace);    
    }

    return result;
}

//Get all specific functions declaration that have not been printed yet.
Source GenericFunctionInfo::get_all_pend_spec_func_decl(
        ReplaceSrcGenericFunction& replace)
{
    Source result;

    DEBUG_CODE()
    {
        std::cerr << "SIMD: Getting all pending specific function declarations for '" 
            << _scalar_func_sym.get_name() 
            << "'" 
            << std::endl;
    }

    //FIXME? Would this function have to print several widths at a time?
    SpecificFunctionInfo& spec_fun = get_better_specific_function(
            replace.get_device_name(),
            replace.get_width());

    if (spec_fun.needs_prettyprint()
            && spec_fun.needs_declaration())
    {
        spec_fun.set_declaration(false);
        result << spec_fun.get_declaration(_scalar_func_sym, _simd_func_sym);    
    }

    return result;
}

namespace 
{
    struct compare_pairs
    {
        bool operator()(
                const width_specific_map_t::value_type& t1, 
                const width_specific_map_t::value_type& t2)
        {
            return t1.second < t2.second;
        }
    };
}

SpecificFunctionInfo& GenericFunctionInfo::get_better_specific_function(
        const std::string device_name,
        const int width) 
{
    DEBUG_CODE()
    {
        std::cerr << "SIMD: Getting better specific function for device '"
            << device_name 
            << "' and width '"
            << width << "'" << std::endl;
    }

    //Looking for device specific functions
    device_specific_map_t::iterator it = _specific_functions.find(device_name);
    if (it == _specific_functions.end())
    {
        running_error("error: specific function definition is missing in device %s.", device_name.c_str());
    }

    width_specific_map_t& width_spec_map = it->second;

    width_specific_map_t::iterator it2 = width_spec_map.find(width);
    if (it2 == width_spec_map.end())
    {
        running_error("error: specific function definition is missing in device %s with width %d.", device_name.c_str(), width);
    }

    it2 = std::max_element(it2, width_spec_map.end(), compare_pairs());

    return it2->second;
}

