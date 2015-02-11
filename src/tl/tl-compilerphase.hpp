/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#ifndef TL_COMPILER_PHASE_HPP
#define TL_COMPILER_PHASE_HPP

#include "tl-common.hpp"
#include <string>
#include <vector>
#include "tl-object.hpp"
#include "tl-dto.hpp"
#include "tl-handler.hpp"

namespace TL
{
    //! Represents an external compiler phase parameter
    struct LIBTL_CLASS CompilerPhaseParameter
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
             * \param name_ Name of the parameter
             * \param descr The description of the parameter
             * \param ref Reference where the value will be updated
             * \param default_value Default value of this parameter used to initialize it
             */
            CompilerPhaseParameter(const std::string& name_,
                    const std::string& descr,
                    std::string& ref,
                    std::string default_value = "")
                : _name(name_),
                _description(descr),
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
    class LIBTL_CLASS CompilerPhase : public Object
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

        protected:
            //! The name of the phase
            std::string _phase_name;
            //! Description of the phase
            std::string _phase_description;

            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }

        private :
            //! Status result of the phase
            PhaseStatus _phase_status;
            //! List of phase parameters
            std::vector<CompilerPhaseParameter*> _parameters;

        public:
            //! Constructor of the phase
            /*!
              The constructor is called when the phase is loaded. This happens
              when the compiler starts, after having read the configuration
              files

              You can set the initial state of a phase in this constructor. If you
              need to reset your state, override CompilerPhase::phase_cleanup()
              or CompilerPhase::phase_cleanup_end_of_pipeline(). All
              information that is not reset will be shared among executions of this
              phase (this allows some sort of interfile/interprocedural information
              processing provided you invoke the compiler with several files at a time)
              */
            CompilerPhase();

            //! Destructor of the phase
            /*!
              This destructor is called when the phase is unloaded, just before
              ending the compiler.
              */
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

            //! Phase cleanup
            /*!
              This function is called _after_ the phase has been run. Override
              this function to reset all data of the phase that is not meant to
              last after the execution of the phase.

              \note If you already do this in the run member function, move
              the code to this function so the compiler can reset your phase,
              even if run is not called.

              \note Do not use this function to clear data that should last during
              all the pipeline but not between files. Use
              CompilerPhase::phase_cleanup_end_of_pipeline instead.

              \see CompilerPhase::phase_cleanup_end_of_pipeline
             */
            virtual void phase_cleanup(DTO& data_flow) { }

            //! Phase cleanup after the end of the pipeline
            /*!
              This function is called after the all phases have been run for a
              single file.  Override this function to reset all the data of
              that is not meant to last between files.

              \note In contrast to CompilerPhase::phase_cleanup this function can
              be used to clear data that should last between phases but not between files.

              \see CompilerPhase::phase_cleanup
             */
            virtual void phase_cleanup_end_of_pipeline(DTO& data_flow) { }

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
    LIBTL_EXTERN void parse_boolean_option(
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
    LIBTL_ALWAYS_EXPORT TL::CompilerPhase* give_compiler_phase_object(void) \
    { \
        return new ClassName(); \
    } \
}

#endif // TL_COMPILER_PHASE_HPP
