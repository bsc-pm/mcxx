/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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


#ifndef NANOX_MPI_HPP
#define NANOX_MPI_HPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

#define TAG_MAIN_OMPSS "__ompss_mpi_daemon" 

namespace TL
{

    namespace Nanox
    {

        class DeviceMPI : public DeviceProvider
        {
            private:

                  Nodecl::List _cuda_file_code;
                  bool _mpi_task_processed;
                  Source _mpiDaemonMain;
                  Nodecl::NodeclBase _root;
                  int _currTaskId;

//                std::string _cudaFilename;
//                std::string _cudaHeaderFilename;
//                AST_t _root;
//                std::set<Symbol> _taskSymbols;
//                std::set<Symbol> _fwdSymbols;
//                std::set<type_tag *> _localDecls;
//
//                // Multimap containing identifier and two Sources:
//                // - first is the ndrange part of a cuda call
//                // - second is the kernell call
//                // Translation code (if exists) will be between them
//                std::multimap<std::string, std::pair<Source,Source> > _implementCalls;
//
//                RefPtr<OpenMP::FunctionTaskSet> _function_task_set;
//
//
//                bool is_wrapper_needed(PragmaCustomConstruct ctr);
//
//                bool is_wrapper_needed(const Symbol& symbol);
//
//                bool is_wrapper_needed(Symbol& symbol);
//
//                bool is_wrapper_needed(PragmaCustomConstruct ctr, Symbol& symbol);
//
//                std::string get_header_macro();
//
//                void char_replace_all_occurrences(std::string &str, std::string original, std::string replaced);
//
//                void replace_all_kernel_configs(AST_t& function_code_tree, ScopeLink sl);
//
//                void replace_kernel_config(AST_t &kernel_call, ScopeLink sl);
//
//                void generateNDrangeCode(
//                        Declaration &kernel_decl,
//                        Source &code_ndrange,
//                        std::string &ndrange,
//                        std::map<std::string, int> &param_positions,
//                        ObjectList<std::string> &param_names);
//
//                void generateParametersCall(
//                        Source &cuda_call,
//                        Declaration& kernel_decl,
//                        std::string calls,
//                        std::map<std::string, int> &param_positions,
//                        ObjectList<std::string> &param_names,
//                        ObjectList<ParameterDeclaration> &parameters_impl,
//                        Declaration &implements_decl);
//
//                void generate_wrapper_code(
//                        Declaration implements_decl,
//                        Declaration kernel_decl,
//                        std::string ndrange,
//                        std::string calls);
//
//                void do_cuda_inline_get_addresses(
//                        const Scope& sc,
//                        const DataEnvironInfo& data_env_info,
//                        Source &copy_setup,
//                        ReplaceSrcIdExpression& replace_src,
//                        bool &err_declared);
//
//                void do_cuda_outline_replacements(
//                        AST_t body,
//                        ScopeLink scope_link,
//                        const DataEnvironInfo& data_env_info,
//                        Source &initial_code,
//                        Source &replaced_outline);
//
//                void get_output_file(std::ofstream& cudaFile);
//
//                void get_output_file(std::ofstream& cudaFile, std::ofstream& cudaHeaderFile);
//
//                void process_wrapper(
//                        PragmaCustomConstruct ctr,
//                        AST_t& decl,
//                        bool& needs_device,
//                        bool& needs_global,
//                        bool& is_global_defined,
//                        bool& needs_extern_c);
//
//                void check_needs_device(
//                		AST_t& decl,
//                		ScopeLink sl,
//                		bool& needs_device);
//
//                void check_global_kernel(AST_t& decl, ScopeLink sl, bool& needs_global, bool& is_global_defined);
//
//                void insert_declaration_device_side(
//                        AST_t& decl,
//                        bool needs_device,
//                        bool needs_global,
//                        bool is_global_defined,
//                        bool needs_extern_c);
//
//                void insert_instrumentation_code(Symbol function_symbol,
//                        const FunctionDefinition& enclosing_function,
//                        const std::string& task_name,
//                        Source& outline_name,
//                        const OutlineFlags& outline_flags,
//                        AST_t& reference_tree,
//                        Source& instrument_before,
//                        Source& instrument_after);
//
//                void create_wrapper_code(
//                        PragmaCustomConstruct pragma_construct,
//                        const OutlineFlags &outline_flags,
//                        ScopeLink sl,
//                        std::string& implemented_name);
//
//                void process_local_symbols(AST_t& decl, ScopeLink sl, Source& forward_declaration);
//
//                void process_extern_symbols(AST_t& decl, ScopeLink sl, Source& forward_declaration);
//
//                void process_outline_task(const OutlineFlags& outline_flags, AST_t& function_tree, ScopeLink& sl,
//                		Source& forward_declaration);
//
//                void do_wrapper_code_replacements(
//                        std::string implemented_name,
//                        DataEnvironInfo& data_environ,
//                        const OutlineFlags& outline_flags,
//                        Source& initial_setup,
//                        Source& implements);
//
//                AST_t generate_task_code(Source& outline_name, const std::string& task_name,
//                        const std::string& struct_typename, Source& parameter_list, DataEnvironInfo& data_environ,
//                        const OutlineFlags& outline_flags, Source& initial_setup, Source& outline_body,
//                        AST_t& reference_tree, ScopeLink& sl);
//
//                void process_device_side_code(Source &outline_name, const std::string& task_name,
//                        const std::string& struct_typename, Source& parameter_list, DataEnvironInfo& data_environ,
//                        const OutlineFlags& outline_flags, Source& initial_setup, Source& outline_body, AST_t& reference_tree, ScopeLink& sl);
//
//                void insert_device_side_code(Source &forward_declaration);
//
//                void insert_device_side_code(AST_t &code_tree);
//
//                void insert_device_side_code(
//                        Source &forward_declaration,
//                        AST_t& outline_code_tree);
//
//                void insert_host_side_code(Source &outline_name, const OutlineFlags& outline_flags,
//                        const std::string& struct_typename, Source &parameter_list, AST_t &reference_tree,
//                        ScopeLink &sl);


                  void generate_additional_mpi_code(
                          const TL::Symbol& called_task,
                          const TL::Symbol& unpacked_function,
                          const TL::ObjectList<Nodecl::NodeclBase>& onto_clause,
                          const Nodecl::Utils::SimpleSymbolMap& param_to_args_map,
                          const TL::Symbol& struct_args,
                          TL::Source& code_host,
                          TL::Source& code_device_pre,                          
                          TL::Source& code_device_post);

//                  void add_included_cuda_files(FILE* file);                  
                  std::string get_ompss_mpi_type(Type type);
                  
            public:

                // This phase does nothing
                virtual void pre_run(DTO& dto);
                virtual void run(DTO& dto);

                DeviceMPI();

                virtual ~DeviceMPI() { }

             virtual void create_outline(CreateOutlineInfo &info,
                     Nodecl::NodeclBase &outline_placeholder,
                     Nodecl::NodeclBase &output_statements,
                     Nodecl::Utils::SymbolMap* &symbol_map);

             virtual void get_device_descriptor(DeviceDescriptorInfo& info,
                     Source &ancillary_device_description,
                     Source &device_descriptor,
                     Source &fortran_dynamic_init);

             virtual void copy_stuff_to_device_file(
                     const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied);

             bool allow_mandatory_creation();

              //  virtual void create_outline(
              //          const std::string& task_name,
              //          const std::string& struct_typename,
              //          DataEnvironInfo &data_environ,
              //          const OutlineFlags& outline_flags,
              //          AST_t reference_tree,
              //          ScopeLink sl,
              //          Source initial_setup,
              //          Source outline_body);

              //  virtual void do_replacements(DataEnvironInfo& data_environ,
              //          AST_t body,
              //          ScopeLink scope_link,
              //          Source &initial_setup,
              //          Source &replace_src);

              //  virtual void get_device_descriptor(const std::string& task_name,
              //          DataEnvironInfo &data_environ,
              //          const OutlineFlags& outline_flags,
              //          AST_t reference_tree,
              //          ScopeLink sl,
              //          Source &ancillary_device_description,
              //          Source &device_descriptor);

              //  virtual std::string get_outline_name_for_instrumentation(const std::string & name,
              //          bool is_template_specialized, const FunctionDefinition& enclosing_function) const;

               virtual void phase_cleanup(DTO& data_flow);

              //  virtual void insert_function_definition(PragmaCustomConstruct, bool);
              //  virtual void insert_declaration(PragmaCustomConstruct, bool);
        };

    }

}

#endif // NANOX_CUDA_HPP
