/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
    //! Represents an external compiler phase parameter
    struct CompilerPhaseParameter
    {
        private:
            //! Stores the name of the parameter
            std::string _name;
            //! Stores the description of the parameter
            std::string _description;
            //! Stores a reference to be updated with the actual value before running the phase
            std::string& _reference;
        public:
            //! Signal fired when the parameter value changes (just before running the phase)
            Signal1<std::string> on_change;

            //! Creates a compiler phase parameter
            /*!
             * \param name Name of the parameter
             * \param description The description of the parameter
             * \param ref Reference where the value will be updated
             * \param default_value Default value of this parameter used to initialize it
             */
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

            //! Returns name of the parameter
            const std::string& name() const
            {
                return _name;
            }

            //! Returns description of the parameter
            const std::string& description() const
            {
                return _description;
            }

            //! Returns the value of the parameter
            const std::string& get_value() const
            {
                return _reference;
            }

            //! Sets the value of the parameter and then signals on_change
            void set_value(const std::string& value)
            {
                _reference = value;
                on_change.signal(value);
            }
    };

    //! Base class for any compiler phase
    class CompilerPhase : public Object
    {
        public:
            //! Resulting status value of the phase
            enum PhaseStatus
            {
                //! Everything went fine
                PHASE_STATUS_OK = 0,
                //! Something failed and compiler pipeline must stop after this one
                PHASE_STATUS_ERROR
            };
        private :
            //! The name of the phase
            std::string _phase_name;
            //! Description of the phase
            std::string _phase_description;
            //! Status result of the phase
            PhaseStatus _phase_status;
            //! List of phase parameters
            std::vector<CompilerPhaseParameter*> _parameters;
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public:
            CompilerPhase();
            virtual ~CompilerPhase();

            //! Entry point of the phase before parsing and typechecking
            /*!
             * This is the entry point of any phase before the actual parsing and typechecking
             * of a translation unit.
             * \param data_flow The data transfer object along the compiler phase pipeline
             */
            virtual void pre_run(DTO& data_flow) { }

            //! Entry point of the phase after parsing and typechecking
            /*!
             * \param data_flow The data transfer object along the compiler phase pipeline
             */
            virtual void run(DTO& data_flow) = 0;

            //! Sets the phase name
            /*!
             * \param phase_name The phase name set
             */
            void set_phase_name(const std::string& phase_name);
            //! Gets the phase name
            const std::string& get_phase_name() const;

            //! Sets the phase description
            /*!
             * \param phase_description The phase description set
             */
            void set_phase_description(const std::string& phase_description);
            //! Gets the phase description
            const std::string& get_phase_description() const;

            //! Registers a parameter of the phase
            /*!
             * \param parameter_name The name of the parameter
             * \param parameter_description A description of the meaning of the parameter
             * \param parameter_reference A reference where the value will be stored
             * \param default_value A default value used when initializing the template
             */
            Signal1<std::string>& register_parameter(const std::string& parameter_name, 
                    const std::string& parameter_description,
                    std::string &parameter_reference,
                    const std::string& default_value = "");

            //! Sets the phase status result
            void set_phase_status(PhaseStatus status);
            //! Gets the phase status result
            PhaseStatus get_phase_status() const;

            //! Returns a list of parameters.
            std::vector<CompilerPhaseParameter*> get_parameters() const;
    };

    //! Conveninence function for parsing boolean options
    /*!
     * \param option_name Name of the parameter
     * \param str_value Value to be parsed as a boolean.
     * \param bool_value Boolean value updated after parsing.
     * \param error_message Error mesage displayed when parsing fails.
     *
     * This function assumes that "0", "no", "false" mean false and "0", "yes",
     * "true" mean true. This function is currently case sensitive (but it
     * might stop being in the future).
     */
    void parse_boolean_option(
            const std::string &option_name,
            const std::string &str_value, 
            bool &bool_value, 
            const std::string &error_message);
}

/*!
 * \def EXPORT_PHASE(ClassName)
 *
 * Creates the factory symbol of this compiler phase.
 *
 * \param ClassName Is the fully qualified name of the class implementing the exported compiler phase
 */

#define EXPORT_PHASE(ClassName) \
extern "C"  \
{ \
    extern TL::CompilerPhase* give_compiler_phase_object(void) \
    { \
        return new ClassName(); \
    } \
}

#endif // TL_COMPILER_PHASE_HPP
