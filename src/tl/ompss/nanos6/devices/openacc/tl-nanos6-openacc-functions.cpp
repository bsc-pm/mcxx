/*--------------------------------------------------------------------
  (C) Copyright 2020-2020 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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

#include "tl-nanos6-openacc-functions.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-omp-core.hpp"

namespace TL
{
namespace Nanos6
{

OpenACCTasks::OpenACCTasks()
{
    set_phase_name("Nanos6/OpenACC specific pass");
    set_phase_description(
        "This pass makes sure all the device specific information that Nanos6 "
        "has is properly passed to OpenACC directives.");
}

class FunctionDefinitionsVisitor : public Nodecl::ExhaustiveVisitor<void>
{
  private:
    TL::OmpSs::FunctionTaskSet &ompss_task_functions;
    TL::ObjectList<TL::Symbol> openacc_functions;

  public:
    FunctionDefinitionsVisitor(TL::OmpSs::FunctionTaskSet &ompss_task_functions_)
        : ompss_task_functions(ompss_task_functions_)
    {
    }

    virtual void visit(const Nodecl::FunctionCode &node)
    {
        TL::Symbol sym = node.get_symbol();
        if (!ompss_task_functions.is_function_task(sym))
            return;

        TL::OmpSs::FunctionTaskInfo &task_info
            = ompss_task_functions.get_function_task(sym);

        TL::OmpSs::TargetInfo &target_info = task_info.get_target_info();
        TL::ObjectList<std::string> devices = target_info.get_device_list();

        if (devices.contains("openacc"))
        {
            info_printf_at(node.get_locus(),
                           "function definition of OpenACC function task '%s'",
                           sym.get_qualified_name().c_str());
            if (devices.size() == 1)
            {
                openacc_functions.append(sym);
            }
            else
            {
                error_printf_at(
                    node.get_locus(),
                    "OpenACC function task is using more than one device");
            }
        }
    }

    TL::ObjectList<TL::Symbol> get_openacc_functions_definitions() const
    {
        return openacc_functions;
    }
};

class FunctionCallsVisitor : public Nodecl::ExhaustiveVisitor<void>
{
  private:
    TL::OmpSs::FunctionTaskSet &ompss_task_functions;
    TL::ObjectList<TL::Symbol> openacc_functions;

  public:
    FunctionCallsVisitor(TL::OmpSs::FunctionTaskSet &ompss_task_functions_)
        : ompss_task_functions(ompss_task_functions_)
    {
    }

    virtual void visit(const Nodecl::FunctionCall &node)
    {
        Nodecl::NodeclBase called = node.get_called();
        if (!called.is<Nodecl::Symbol>())
            return;

        // Note: no need to walk the argument expressions because taks functions
        // can only be void so a call to one will always be a top-level
        // expression.

        TL::Symbol sym = called.get_symbol();
        if (!ompss_task_functions.is_function_task(sym))
            return;

        TL::OmpSs::FunctionTaskInfo &task_info
            = ompss_task_functions.get_function_task(sym);

        TL::OmpSs::TargetInfo &target_info = task_info.get_target_info();
        TL::ObjectList<std::string> devices = target_info.get_device_list();

        if (devices.contains("openacc"))
        {
            info_printf_at(node.get_locus(),
                           "function call to OpenACC function task '%s'",
                           sym.get_qualified_name().c_str());
            if (devices.size() == 1)
            {
                openacc_functions.append(sym);
            }
            else
            {
                error_printf_at(
                    node.get_locus(),
                    "OpenACC function task is using more than one device");
            }
        }
    }

    TL::ObjectList<TL::Symbol> get_openacc_functions_called() const
    {
        return openacc_functions;
    }
};

void OpenACCTasks::run(DTO &dto)
{
    Nodecl::NodeclBase translation_unit
        = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

    std::shared_ptr<TL::OmpSs::FunctionTaskSet> ompss_task_functions
        = std::static_pointer_cast<TL::OmpSs::FunctionTaskSet>(
            dto["openmp_task_info"]);
    ERROR_CONDITION(
        !ompss_task_functions, "OmpSs Task Functions not in the DTO", 0);

    FunctionDefinitionsVisitor functions_definition_visitor(
        *ompss_task_functions);
    functions_definition_visitor.walk(translation_unit);
    // functions_definition_visitor.get_openacc_functions_definitions()

    FunctionCallsVisitor function_calls_visitor(
        *ompss_task_functions);
    function_calls_visitor.walk(translation_unit);
    // functions_calls_visitor.get_openacc_functions_called()
}

} // namespace Nanos6
} // namespace TL

EXPORT_PHASE(TL::Nanos6::OpenACCTasks)
