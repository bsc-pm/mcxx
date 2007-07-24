/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef TL_COMPILER_PHASE_HPP
#define TL_COMPILER_PHASE_HPP

#include <string>
#include <vector>
#include "tl-object.hpp"
#include "tl-dto.hpp"
#include "tl-handler.hpp"

namespace TL
{
    struct CompilerPhaseParameter
    {
        private:
            std::string _name;
            std::string _description;
            std::string& _reference;
        public:
            Signal1<std::string> on_change;

            CompilerPhaseParameter(const std::string& name,
                    const std::string& description,
                    std::string& ref,
                    std::string default_value = "")
                : _name(name),
                _description(description),
                _reference(ref)
            {
                _reference = default_value;
            }

            const std::string& name() const
            {
                return _name;
            }

            const std::string& description() const
            {
                return _description;
            }

            const std::string& get_value() const
            {
                return _reference;
            }

            void set_value(const std::string& value)
            {
                _reference = value;
                on_change.signal(value);
            }
    };

    class CompilerPhase : Object
    {
        private :
            std::string _phase_name;
            std::string _phase_description;
            std::vector<CompilerPhaseParameter*> _parameters;
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
            {
                return NULL;
            }
        public:
            virtual ~CompilerPhase();

            virtual void run(DTO& data_flow) = 0;

            void set_phase_name(const std::string& phase_name);
            const std::string& get_phase_name() const;

            void set_phase_description(const std::string& phase_description);
            const std::string& get_phase_description() const;

            Signal1<std::string>& register_parameter(const std::string& parameter_name, 
                    const std::string& parameter_description,
                    std::string &parameter_reference,
                    const std::string& default_value = "");

            // Used for help only
            std::vector<CompilerPhaseParameter*> get_parameters() const;
    };
}

#define EXPORT_PHASE(ClassName) \
extern "C"  \
{ \
    extern TL::CompilerPhase* give_compiler_phase_object(void) \
    { \
        return new ClassName(); \
    } \
}

#endif // TL_COMPILER_PHASE_HPP
