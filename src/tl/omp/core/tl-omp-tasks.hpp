/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#ifndef TL_OMP_TASKS_HPP
#define TL_OMP_TASKS_HPP

#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-omp-deps.hpp"
#include "tl-langconstruct.hpp"
#include <map>

namespace TL
{
    namespace OpenMP
    {
        class FunctionTaskDependency
        {
            private:
                DependencyDirection _direction;
                Expression _expr;
            public:
                FunctionTaskDependency(Expression expr, DependencyDirection direction);
                DependencyDirection get_direction() const; 
                Expression get_expression() const;
        };

        class FunctionTaskTargetInfo
        {
            private:
                ObjectList<CopyItem> _copy_in;
                ObjectList<CopyItem> _copy_out;
                ObjectList<CopyItem> _copy_inout;

                ObjectList<std::string> _device_list;

                bool _copy_deps;
            public:

                bool can_be_ommitted()
                {
                    return _copy_in.empty()
                        && _copy_out.empty()
                        && _copy_inout.empty()
                        && (_device_list.empty()
                                || ((_device_list.size() == 1)
                                    && (_device_list[0] == "smp")));
                }

                FunctionTaskTargetInfo()
                    : _copy_in(),
                    _copy_out(),
                    _copy_inout(),
                    _device_list(),
                    _copy_deps()
                {
                }

                void set_copy_in(const ObjectList<CopyItem>& copy_items)
                {
                    _copy_in = copy_items;
                }

                void set_copy_out(const ObjectList<CopyItem>& copy_items)
                {
                    _copy_out = copy_items;
                }

                void set_copy_inout(const ObjectList<CopyItem>& copy_items)
                {
                    _copy_inout = copy_items;
                }

                ObjectList<CopyItem> get_copy_in() const
                {
                    return _copy_in;
                }

                ObjectList<CopyItem> get_copy_out() const
                {
                    return _copy_out;
                }

                ObjectList<CopyItem> get_copy_inout() const
                {
                    return _copy_inout;
                }

                void set_copy_deps(bool b)
                {
                    _copy_deps = b;
                }

                bool has_copy_deps() const
                {
                    return _copy_deps;
                }

                void set_device_list(const ObjectList<std::string>& device_list)
                {
                    _device_list = device_list;
                }

                ObjectList<std::string> get_device_list() const
                {
                    return _device_list;
                }
        };

        class FunctionTaskInfo 
        {
            private:
                Symbol _sym;
                ObjectList<FunctionTaskDependency> _parameters;

                typedef std::map<std::string, Symbol> implementation_table_t;
                implementation_table_t _implementation_table;

                FunctionTaskTargetInfo _target_info;
            public:
                FunctionTaskInfo(Symbol sym,
                        ObjectList<FunctionTaskDependency> parameter_info,
                        FunctionTaskTargetInfo target_info);

                ObjectList<FunctionTaskDependency> get_parameter_info() const;

                ObjectList<Symbol> get_involved_parameters() const;

                FunctionTaskTargetInfo get_target_info() const;

                void add_device(const std::string& device_name);
                void add_device_with_implementation(
                        const std::string& device_name,
                        Symbol implementor_symbol);

                ObjectList<std::string> get_all_devices();

                typedef std::pair<std::string, Symbol> implementation_pair_t;

                ObjectList<implementation_pair_t> get_devices_with_implementation();
        };

        class FunctionTaskSet : public TL::Object
        {
            private:
                std::map<Symbol, FunctionTaskInfo> _map;
            public:
                FunctionTaskSet();

                bool is_function_task(Symbol sym) const;

                FunctionTaskInfo& get_function_task(Symbol sym);
                const FunctionTaskInfo& get_function_task(Symbol sym) const;
                bool add_function_task(Symbol sym, const FunctionTaskInfo&);

                bool empty() const;
        };

    }
}

#endif // TL_OMP_TASKS_HPP
