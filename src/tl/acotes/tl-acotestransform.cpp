#include "tl-acotestransform.hpp"

#include <list>
#include <iostream>
#include <sstream>
#include <stack>
#include <vector>

#include "tl-ast.hpp"
#include "tl-compilerphase.hpp"
#include "tl-functor.hpp"
#include "tl-pragmasupport.hpp"
#include "tl-scopelink.hpp"
#include "tl-source.hpp"
#include "tl-traverse.hpp"

// #define foreach(SET,IT) 
//     for (__typeof__((SET).begin()) IT = (SET).begin(); IT != (SET).end(); IT++) 

#define foreach(TYPE,IT,SET) \
	for  (ObjectList< TYPE >::iterator IT=(SET).begin();IT!=(SET).end();IT++)

#if 0	
#define FOREACH_BEGIN(TYPE,VAR,SET) \
	foreach(TYPE,__it__##__LINE__,SET) \
	{ TYPE& VAR= *__it__##__LINE__;
#define FOREACH_FINISH() \
	}
#endif

namespace TL 
{
	static const std::string ARG_NAME("acolib__arg");
	static const std::string STATE_NAME("acolib__state");
	static const std::string ERROR_NAME("acolib__error");
	static const std::string THREAD_NAME("acolib__thread");
	
	class StreamInfo
	{
		private:
		int          _id;
		IdExpression _variable;
		int          _task_id;
		int          _input_task_id;
		int          _output_task_id;
		std::string  _name;
		
		public:
		StreamInfo(int id, const IdExpression& variable, 
				int task_id, int input_task_id, int output_task_id)
			: _id(id)
			, _variable(variable)
			, _task_id(task_id)
			, _input_task_id(input_task_id)
			, _output_task_id(output_task_id)
		{
			_name= _variable.prettyprint();
		}
		
		const IdExpression& get_variable() const
		{
			return _variable;
		}
		int get_task_id() const
		{
			return _task_id;
		}
		int get_input_task_id() const
		{
			return _input_task_id;
		}
		int get_output_task_id() const
		{
			return _output_task_id;
		}
		std::string get_name() const 
		{
			return _name;
		}
		std::string get_var_name() const
		{
			return get_name();
		}
		std::string get_istream_name() const
		{
			std::stringstream ss;
			
			ss << "is_" << get_name() << "_" << _task_id;
			
			return ss.str();
		}
		std::string get_ostream_name() const
		{
			std::stringstream ss;
			
			ss << "os_" << get_name() << "_" << _task_id;
			
			return ss.str();
		}
		std::string get_istream_declare_code() const
		{
			std::stringstream code;
			
			code
				<< "istream_t " << get_istream_name() << ";"
				;
			
			return code.str();
		}
		std::string get_ostream_declare_code() const
		{
			std::stringstream code;
			
			code
				<< "ostream_t " << get_ostream_name() << ";"
				;
			
			return code.str();
		}
		std::string get_pop_code() const
		{
			std::stringstream code;
			
			code
				<< "istream_pop"
				<< "( " << get_istream_name()
				<< ", &" << get_var_name()
				<< ");" 
				;
			
			return code.str();
		}
		std::string get_push_code() const
		{
			std::stringstream code;
			
			code
				<< "ostream_push"
				<< "( " << get_ostream_name()
				<< ", &" << get_var_name()
				<< ");" 
				;
			
			return code.str();
		}
		std::string get_istream_close_code() const
		{
			std::stringstream code;
			
			code
				<< "istream_close"
				<< "( " << get_istream_name()
				<< ");" 
				;
			
			return code.str();
		}
		std::string get_ostream_close_code() const
		{
			std::stringstream code;
			
			code
				<< "ostream_close"
				<< "( " << get_ostream_name()
				<< ");" 
				;
			
			return code.str();
		}
		std::string get_istream_declare_from_state_code() const
		{		
			std::stringstream code;
			
			code
				<< "istream_t* " << get_istream_name() 
				<< "= "
				<< "&" << STATE_NAME << "->" << get_istream_name()
				<< ";"
				;
			
			return code.str();
		}
		std::string get_ostream_declare_from_state_code() const
		{		
			std::stringstream code;
			
			code
				<< "ostream_t* " << get_ostream_name() 
				<< "= "
				<< "&" << STATE_NAME << "->" << get_ostream_name()
				<< ";"
				;
			
			return code.str();
		}
		
	};
	
	
	class TaskInfo 
	{
		private:
		int _id;
		ObjectList<int>          _inputs;
		ObjectList<int>          _outputs;
		ObjectList<IdExpression> _state;
		
		ObjectList<int> _nested_inputs;
		ObjectList<int> _nested_outputs;
		
		public:
		TaskInfo(int id)
			: _id(id)
		{
		}

		int get_id() const
		{
			return _id;
		}	
		std::string get_name()
		{
			std::stringstream ss;
			
			ss << "acolib__task_" << _id;
			
			return ss.str();
		}
		std::string get_state_name()
		{
			std::stringstream ss;
			
			ss << "acolib__state_" << _id;
			
			return ss.str();
		}
		std::string get_struct_name()
		{
			std::stringstream ss;
			
			ss << "struct " << get_state_name();
			
			return ss.str();
		}
		
		void add_input(int input)
		{
			_inputs.push_back(input);
		}
		ObjectList<int> get_inputs() const
		{
			return _inputs;
		}
		void add_output(int output)
		{
			_outputs.push_back(output);
		}
		ObjectList<int> get_outputs() const
		{
			return _outputs;
		}
		void add_nested_input(int input)
		{
			_nested_inputs.push_back(input);
		}
		ObjectList<int> get_nested_inputs() const
		{
			return _nested_inputs;
		}
		void add_nested_output(int output)
		{
			_nested_outputs.push_back(output);
		}
		ObjectList<int> get_nested_outputs() const
		{
			return _nested_outputs;
		}
		void add_state(const IdExpression& var)
		{
			_state.push_back(var);
		}
		ObjectList<IdExpression> get_state() const
		{
			return _state;
		}
	};
	
	class TaskGroupInfo 
	{
		private:
		int              _id;
		ObjectList<int>  _tasks;
		ObjectList<int>  _streams;
		
		public:
		TaskGroupInfo(int id)
			: _id(id)
		{
		}

		int get_id() const
		{
			return _id;
		}	
		
		void add_task(int task_id)
		{
			_tasks.push_back(task_id);
		}
		ObjectList<int> get_tasks()
		{
			return _tasks;
		}
		void add_stream(int stream_id)
		{
			_streams.push_back(stream_id);
		}
		ObjectList<int> get_streams()
		{
			return _streams;
		}
	};
	
	class AcotesTransform : public PragmaCustomCompilerPhase 
	{
		private:
		std::stack<int>            _taskgroup_stack;
		std::stack<int>            _task_stack;
		
		std::vector<TaskGroupInfo> _taskgroup_info_vector;
		std::vector<TaskInfo>      _task_info_vector;
		std::vector<StreamInfo>    _stream_info_vector;

		private:
		int create_taskgroup_info()
		{
			int id= _taskgroup_info_vector.size();
			_taskgroup_info_vector.push_back(id);
			
			return id;
		}
		
		int create_task_info()
		{
			int id= _task_info_vector.size();
			_task_info_vector.push_back(id);
			
			return id;
		}
		
		int create_stream_info(const IdExpression& var, int task_id, int input_task_id, int output_task_id)
		{
			int id= _stream_info_vector.size();

			StreamInfo stream_info(id, var, task_id, input_task_id, output_task_id);
			_stream_info_vector.push_back(stream_info);
			
			return id;
		}
		
		public:
		AcotesTransform()
		: PragmaCustomCompilerPhase("acotes")
		{
			on_directive_pre["taskgroup"].connect(
				functor(&AcotesTransform::taskgroup_preorder, *this)
			);
			on_directive_pre["task"].connect(
				functor(&AcotesTransform::task_preorder, *this)
			);
			on_directive_post["task"].connect(
				functor(&AcotesTransform::task_postorder, *this)
			);
			on_directive_post["taskgroup"].connect(
				functor(&AcotesTransform::taskgroup_postorder, *this)
			);
		}
		
		void taskgroup_preorder(PragmaCustomConstruct pragma_custom_construct)
		{
			// Push a new task group info and the implicit task_info
			_taskgroup_stack.push(create_taskgroup_info());
			_task_stack.push(create_task_info());
		}

		void task_preorder(PragmaCustomConstruct pragma_custom_construct)
		{
			// Push a new task info, and obtain current taskgroup info
			_task_stack.push(create_task_info());
			
		}

		void task_postorder(PragmaCustomConstruct pragma_custom_construct)
		{
			// - Obtain environment  
			// Obtain current taskgroup id and task id
			int taskgroup_id= _taskgroup_stack.top();
			int task_id= _task_stack.top();
			
			// Pop & Obtain parent task id
			_task_stack.pop();
			int task_parent_id= _task_stack.top();
			
			// Obtain tasks and taskgroup instances
			TaskGroupInfo& taskgroup_info= _taskgroup_info_vector.at(taskgroup_id);
			TaskInfo&      task_info= _task_info_vector.at(task_id);
			TaskInfo&      task_parent_info= _task_info_vector.at(task_parent_id);
			
			ObjectList<int> nested_input_ids= task_info.get_nested_inputs();
			ObjectList<int> nested_output_ids= task_info.get_nested_outputs();
			
			
			// - Obtain data from source  
			// Register task info at taskgroup
			taskgroup_info.add_task(task_id);

			// Look for the clauses
			PragmaCustomClause input_clause= 
					pragma_custom_construct.get_clause("input");
			PragmaCustomClause output_clause= 
					pragma_custom_construct.get_clause("output");
			
			
			// Look for referenced variables at input and outputs
			ObjectList<IdExpression> inputs_vars= input_clause.id_expressions();
			ObjectList<IdExpression> outputs_vars= output_clause.id_expressions();

			// Obtaing task body
			Statement task_body= pragma_custom_construct.get_statement();
			 
			// list of the variables used at body task 
			// not declared inside the task 
			ObjectList<IdExpression> task_state_refs= 
					task_body.non_local_symbol_occurrences(LangConstruct::ONLY_OBJECTS);
			
			// Remove repeated IdExpression's that refer to the same symbol
			ObjectList<IdExpression> task_state;
			task_state.insert(task_state_refs, functor(&IdExpression::get_symbol));
					
			
			// - Generate sources, a little map here :-)
			Source task_add_src;

			Source task_struct_src;
			Source task_struct_vars_src;
			Source task_struct_iss_src;
			Source task_struct_oss_src;
			Source task_struct_nested_src;
			Source task_struct_nested_iss_src;
			Source task_struct_nested_oss_src;

			Source task_outline_src;
			Source task_outline_initialization_src;
			Source task_outline_initialization_parameter_src;
			Source task_outline_initialization_trace_src;
			Source task_outline_initialization_state_src;
			Source task_outline_initialization_state_vars_src;
			Source task_outline_initialization_state_iss_src;
			Source task_outline_initialization_state_oss_src;
			Source task_outline_initialization_state_nested_src;
			Source task_outline_initialization_state_nested_iss_src;
			Source task_outline_initialization_state_nested_oss_src;
			Source task_outline_initialization_state_error_src;
			Source task_outline_loop_src;
			Source task_outline_loop_condition_src;
			Source task_outline_loop_pops_src;
			Source task_outline_loop_pushes_src;
			Source task_outline_finalization_src;
			Source task_outline_finalization_closes_src;
			Source task_outline_finalization_closes_iss_src;
			Source task_outline_finalization_closes_oss_src;
			Source task_outline_finalization_closes_nested_src;
			Source task_outline_finalization_closes_nested_iss_src;
			Source task_outline_finalization_closes_nested_oss_src;
			Source task_outline_finalization_trace_src;

			
			Source task_replace_src;
			Source task_replace_pushes_src;
			Source task_replace_pops_src;
			
			
	
			// - Generation commons sources
			std::string task_name= task_info.get_name();
			std::string struct_name= task_info.get_struct_name();


			// task_struct_nested_iss_src
			// task_outline_initialization_state_nested_iss_src
			// task_outline_finalization_closes_nested_iss_src
			foreach (int,it,nested_input_ids)
			{
				int stream_id= *it;
				StreamInfo& stream_info= _stream_info_vector.at(stream_id);
				
				// Declares the reverse end streams
				task_struct_nested_iss_src
					<< "ostream_t " << stream_info.get_ostream_name() << ";"
					;
					
				// Sets the value for the stream
				task_outline_initialization_state_nested_iss_src
					<< "ostream_t* " << stream_info.get_ostream_name() 
					<< "= " 
					<< "&" << STATE_NAME << "->" << stream_info.get_ostream_name() 
					<< ";"
					;
					
				// Closes the used stream
				task_outline_finalization_closes_nested_iss_src
					<< "ostream_close"
					<< "( " << stream_info.get_ostream_name()
					<< ");"
					;
			}


			// task_struct_nested_oss_src
			// task_outline_initialization_state_nested_oss_src
			// task_outline_finalization_closes_nested_oss_src
			foreach (int,it,nested_output_ids)
			{
				int stream_id= *it;
				StreamInfo& stream_info= _stream_info_vector.at(stream_id);
				
				// Declares the reverse end streams
				task_struct_nested_oss_src
					<< "istream_t* " << stream_info.get_istream_name() << ";"
					;
					
				// Sets the value for the stream state value
				task_outline_initialization_state_nested_oss_src
					<< "istream_t* " << stream_info.get_istream_name() 
					<< "= " 
					<< "&" << STATE_NAME << "->" << stream_info.get_istream_name() 
					<< ";"
					;				
					
				// Closes the used stream
				task_outline_finalization_closes_nested_oss_src
					<< "istream_close"
					<< "( " << stream_info.get_istream_name()
					<< ");"
					;
			}
			
			
			// task_struct_vars
			// task_outline_initialization_state_vars_src
			foreach(IdExpression,it,task_state)
			{
				// Retrieve used variable and type
				IdExpression& var= *it;
				Symbol sym= var.get_symbol();
				Type type= sym.get_type();
				
				// Add variable to state
				task_info.add_state(var);
				
				// Declare variable at struct
				task_struct_vars_src
					<< type.get_declaration(var.get_scope(), var.prettyprint())
					<< ";"
					;
					
				// Initializate values for state vars at task
				task_outline_initialization_state_vars_src
					<< type.get_declaration(var.get_scope(), var.prettyprint())
					<< "= "
					<< STATE_NAME << "->" << var.prettyprint()
					<< ";"
					;
			}
			
			// task_struct_iss
			// task_outline_initialization_state_vars_src
			// task_outline_loop_pops_src
			// task_outline_finalization_closes_iss_src
			// task_replace_pushes_src
			foreach(IdExpression,it,inputs_vars)
			{
				// Retreieve input stream variable
				IdExpression& var= *it;
				
				// Create stream
				int stream_id= create_stream_info(var, task_id, task_id, task_parent_id);
				StreamInfo stream_info= _stream_info_vector.at(stream_id);
				std::string stream_name= stream_info.get_name();
				std::string istream_name= stream_info.get_istream_name();
				std::string var_name= stream_name;
				std::string ostream_name= stream_info.get_ostream_name();
				
				// Add streams to task 
				task_info.add_input(stream_id);
				task_parent_info.add_nested_input(stream_id);
				taskgroup_info.add_stream(stream_id);
				
				// Declare variable 
				task_struct_iss_src
					<< stream_info.get_istream_declare_code()
					;

				// Initializate values for the streams at task
				task_outline_initialization_state_iss_src
					<< stream_info.get_istream_declare_from_state_code() 
					;

				// Pop for the stream at the variable named by the stream
				task_outline_loop_pops_src
					<< stream_info.get_pop_code()
					;
					
				// Closes the stream
				task_outline_finalization_closes_iss_src
					<< stream_info.get_istream_close_code()
					;
					
				// Stream pushes to this task
				task_replace_pushes_src
					<< stream_info.get_push_code()
					;
			}
			
			// task_struct_oss
			// task_outline_initialization_state_oss_src
			// task_outline_loop_pushes_src
			// task_outline_finalization_closes_oss_src
			// task_replace_pops_src
			foreach(IdExpression,it,outputs_vars)
			{
				// Retreieve input stream variable
				IdExpression& var= *it;
				
				// Create stream
				int stream_id= create_stream_info(var, task_id, task_parent_id, task_id);
				StreamInfo stream_info= _stream_info_vector.at(stream_id);
				std::string stream_name= stream_info.get_name();
				std::string ostream_name= stream_info.get_ostream_name();
				std::string var_name= stream_name;
				std::string istream_name= stream_info.get_istream_name();
				
				// Add streams to task 
				task_info.add_output(stream_id);
				task_parent_info.add_nested_output(stream_id);
				taskgroup_info.add_stream(stream_id);

				// Declare variable 
				task_struct_oss_src
					<< stream_info.get_ostream_declare_code()
					;

				// Initializate values for the streams at task
				task_outline_initialization_state_oss_src
					<< stream_info.get_ostream_declare_from_state_code()
					;

				// Pop for the stream at the variable named by the stream
				task_outline_loop_pushes_src
					<< stream_info.get_push_code() 
					;
					
				// Closes the stream
				task_outline_finalization_closes_oss_src
					<< stream_info.get_ostream_close_code()
					;

				// Stream pops to this task
				task_replace_pops_src
					<< stream_info.get_pop_code()
					;
			}
			
			
			// task_struct_nested_src
			task_struct_nested_src
				<< task_struct_nested_iss_src
				<< task_struct_nested_oss_src
				;			
			
			// task_struct_src
			task_struct_src
				<< struct_name 
				<< "{"
				<<   task_struct_vars_src
				<<   task_struct_iss_src
				<<   task_struct_oss_src
				<<   task_struct_nested_src
				<<   "stream_error_t "<< ERROR_NAME <<";"
				<< "};"
				;
            
            // task_outline_initialization_parameter_src
            task_outline_initialization_parameter_src
            	<< struct_name << "* " << STATE_NAME
            	<< "= "
            	<< " ("<< struct_name <<"*)" << ARG_NAME 
            	<< ";"
            	;
            	
            // task_outline_initialization_trace_src
            task_outline_initialization_trace_src
            	<< "acotrace_init(1," << task_id << ");"
            	; 
            
            // task_outline_initialization_state_error_src
            task_outline_initialization_state_error_src
            	<< "stream_error_t* " << ERROR_NAME 
            	<< "= " 
            	<< "&" << STATE_NAME << "->" << ERROR_NAME
            	<< ";"
            	;
            
            // task_outline_initialization_state_nested_src
            task_outline_initialization_state_nested_src
            	<< task_outline_initialization_state_nested_iss_src
            	<< task_outline_initialization_state_nested_oss_src
            	;            
            
            // task_outline_initialization_state_src
            task_outline_initialization_state_src
            	<< task_outline_initialization_state_vars_src
				<< task_outline_initialization_state_iss_src
				<< task_outline_initialization_state_oss_src
				<< task_outline_initialization_state_nested_src
				<< task_outline_initialization_state_error_src
				;
				
            
            // task_outline_initialization_src
            task_outline_initialization_src 
            	<< task_outline_initialization_parameter_src
            	<< task_outline_initialization_trace_src
            	<< task_outline_initialization_state_src
            	;

			// task_outline_loop_condition_src
			task_outline_loop_condition_src
				<< "!stream_error_eos(" << ERROR_NAME << ")"
				;
			
			// task_outline_loop_src
			task_outline_loop_src
				<< task_outline_loop_pops_src
				<< "while(" << task_outline_loop_condition_src << ")"
				<< "{"
				<<   "acotrace_iteration_begin();"
				<<   "acotrace_run();"
				<<   task_body.prettyprint()
				<<   "acotrace_iteration_end();"
				<<   task_outline_loop_pushes_src
				<<   task_outline_loop_pops_src
				<< "}"
				<< "acotrace_fini();"
				;

			// task_outline_finalization_closes_nested_src
			task_outline_finalization_closes_nested_src
				<< task_outline_finalization_closes_nested_iss_src
				<< task_outline_finalization_closes_nested_oss_src
				;

			// task_outline_finalization_closes_src
			task_outline_finalization_closes_src
				<< task_outline_finalization_closes_iss_src
				<< task_outline_finalization_closes_oss_src
				<< task_outline_finalization_closes_nested_src
				;

			// task_outline_finalization_trace_src
			task_outline_finalization_trace_src
				<< "acotrace_stop();"
				;
			
			// task_outline_finalization_src
			task_outline_finalization_src
				<< task_outline_finalization_closes_src
				<< task_outline_finalization_trace_src
				;			
			
            
            // task_outline_src
			task_outline_src
				<< "void* " << task_name << "(void*"<< ARG_NAME <<")"
				<< "{"
				<<   task_outline_initialization_src
				<<   task_outline_loop_src
				<<   task_outline_finalization_src
				<<   "return (void*)0;"   
				<< "}"
				;


			// task_add_src
			task_add_src
				<< task_struct_src
				<< task_outline_src; 
						
						
			// task_replace_src
			task_replace_src
				<< "{"
				<<   task_replace_pushes_src
				<<   task_replace_pops_src
				<<   "acotrace_run();"
				<< "}"
				;
						

			// - Code placement
			// Pragma code add
			FunctionDefinition function_definition 
				= pragma_custom_construct.get_enclosing_function();
				
			Scope function_scope = function_definition.get_scope();
			ScopeLink function_scope_link = function_definition.get_scope_link();
			
			AST_t task_add_tree = task_add_src.parse_global(function_scope,
				function_scope_link);
				
			function_definition.get_ast().prepend_sibling_function(task_add_tree);


			// Pragma substition			
			AST_t task_replace_tree= task_replace_src.parse_statement
				( pragma_custom_construct.get_scope()
				, pragma_custom_construct.get_scope_link()
				);
			pragma_custom_construct.get_ast().replace(task_replace_tree);

		}

		void taskgroup_postorder(PragmaCustomConstruct pragma_custom_construct)
		{
			// Obtain current taskgroup id and implicit task id
			int taskgroup_id= _taskgroup_stack.top();
			int task_implicit_id= _task_stack.top();
			
			// Pop current task info			
			_taskgroup_stack.pop();
			_task_stack.pop();
			
			// Obtain task and taskgroup instances
			TaskGroupInfo&  taskgroup_info= _taskgroup_info_vector.at(taskgroup_id);
			TaskInfo&       task_implicit_info= _task_info_vector.at(task_implicit_id);
			ObjectList<int> task_ids= taskgroup_info.get_tasks();
						
	
			// Obtain nested streams
			ObjectList<int> nested_input_ids= task_implicit_info.get_nested_inputs();
			ObjectList<int> nested_output_ids= task_implicit_info.get_nested_outputs();
			ObjectList<int> streams_ids= taskgroup_info.get_streams();
			
			

			// - Generate sources, a little map here :-)
			Source taskgroup_replace_src;
			Source taskgroup_replace_initialization_src;
			Source taskgroup_replace_initialization_declare_src;
			Source taskgroup_replace_initialization_declare_iss_src;
			Source taskgroup_replace_initialization_declare_oss_src;
			Source taskgroup_replace_initialization_declare_error_src;
			Source taskgroup_replace_initialization_create_src;
			Source taskgroup_replace_initialization_create_iss_src;
			Source taskgroup_replace_initialization_create_oss_src;
			Source taskgroup_replace_initialization_create_error_src;
			Source taskgroup_replace_initialization_alltask_src;
			Source taskgroup_replace_initialization_connect_src;
			Source taskgroup_replace_start_src;
			Source taskgroup_replace_body_src;
			Source taskgroup_replace_closes_src;
			Source taskgroup_replace_closes_iss_src;
			Source taskgroup_replace_closes_oss_src;
			Source taskgroup_replace_join_src;
			Source taskgroup_replace_finalization_src;
			Source taskgroup_replace_finalization_streams_src;
			Source taskgroup_replace_finalization_streams_nested_src;
			Source taskgroup_replace_finalization_streams_nested_iss_src;
			Source taskgroup_replace_finalization_streams_nested_oss_src;
			Source taskgroup_replace_finalization_streams_tasks_src;
			Source taskgroup_replace_finalization_streams_tasks_iss_src;
			Source taskgroup_replace_finalization_streams_tasks_oss_src;
			Source taskgroup_replace_finalization_streams_error_src;
			Source taskgroup_replace_finalization_streams_tasks_error_src;
			

			// taskgroup_replace_initialization_declare_iss_src
			// taskgroup_replace_initialization_create_iss_src
			// taskgroup_replace_closes_iss_src
			// taskgroup_replace_finalization_streams_nested_iss_src
			foreach (int,it,nested_input_ids)
			{
				int stream_id= *it;
				StreamInfo& stream_info= _stream_info_vector.at(stream_id);
				
				// Declares the reverse end streams
				taskgroup_replace_initialization_declare_iss_src
					<< "ostream_t " << stream_info.get_ostream_name() << "_object;"
					<< "ostream_t* " << stream_info.get_ostream_name() 
					<< "= " 
					<< "&" << stream_info.get_ostream_name() << "_object" 
					<< ";"
					;
					
				// Constructs the reverse end streams
				taskgroup_replace_initialization_create_iss_src
					<< "ostream_create"
					<< "( " << stream_info.get_ostream_name()
					<< ", sizeof(" << stream_info.get_var_name() << ")"
					<< ", " << ERROR_NAME
					<< ");"
					;
					
				// Closes the used stream
				taskgroup_replace_closes_iss_src
					<< "ostream_close"
					<< "( " << stream_info.get_ostream_name()
					<< ");"
					;
					
				// Destroys the stream
				taskgroup_replace_finalization_streams_nested_iss_src
					<< "ostream_destroy"
					<< "( " << stream_info.get_ostream_name()
					<< ");"
					;
			}


			// taskgroup_replace_initialization_declare_oss_src
			// taskgroup_replace_initialization_create_oss_src
			// taskgroup_replace_closes_oss_src
			// taskgroup_replace_finalization_streams_nested_oss_src
			foreach (int,it,nested_output_ids)
			{
				int stream_id= *it;
				StreamInfo& stream_info= _stream_info_vector.at(stream_id);
				
				// Declares the reverse end streams
				taskgroup_replace_initialization_declare_oss_src
					<< "istream_t " << stream_info.get_istream_name() << "_object;"
					<< "istream_t* " << stream_info.get_istream_name() 
					<< "= " 
					<< "&" << stream_info.get_istream_name() << "_object" 
					<< ";"
					;
					
				// Constructs the reverse end streams
				taskgroup_replace_initialization_create_oss_src
					<< "istream_create"
					<< "( " << stream_info.get_istream_name()
					<< ", sizeof(" << stream_info.get_var_name() << ")"
					<< ", " << ERROR_NAME
					<< ");"
					;
					
				// Closes the used stream
				taskgroup_replace_closes_oss_src
					<< "istream_close"
					<< "( " << stream_info.get_istream_name()
					<< ");"
					;
										
				// Destroys the streams
				taskgroup_replace_finalization_streams_nested_oss_src
					<< "istream_destroy"
					<< "( " << stream_info.get_istream_name()
					<< ");"
					;
			}

			// taskgroup_replace_initialization_connect_src
			foreach (int,it,streams_ids)
			{
				int stream_id= *it;
				StreamInfo& stream_info= _stream_info_vector.at(stream_id);
				
				int input_task_id= stream_info.get_input_task_id(); 
				int output_task_id= stream_info.get_output_task_id();
				TaskInfo& input_task_info= _task_info_vector.at(input_task_id); 
				TaskInfo& output_task_info= _task_info_vector.at(output_task_id); 
				
				std::string istream_name;
				std::string ostream_name;
				
				if (input_task_id == task_implicit_id)
				{
					istream_name= stream_info.get_istream_name();
				}
				else
				{
					istream_name= std::string("&")
						+ input_task_info.get_state_name()
						+ std::string(".")
						+ stream_info.get_istream_name();
				}

				if (output_task_id == task_implicit_id)
				{
					ostream_name= stream_info.get_ostream_name();
				}
				else
				{
					ostream_name= std::string("&")
						+ output_task_info.get_state_name()
						+ std::string(".")
						+ stream_info.get_ostream_name();
				}

				// Connect this stream
				taskgroup_replace_initialization_connect_src
					<< "ostream_connect"
					<< "( " << ostream_name
					<< ", " << istream_name 
					<< ");"
					;
			}

			// taskgroup_replace_initialization_alltask_src 
			// taskgroup_replace_start_src
			// taskgroup_replace_join_src
			// taskgroup_replace_finalization_streams_tasks_error_src
			// ~ // taskgroup_replace_finalization_streams_tasks_iss_src
			// ~ // taskgroup_replace_finalization_streams_tasks_oss_src
			foreach(int,it,task_ids)
			{
				Source task_initialization_src;
				Source task_initialization_declare_src;
				Source task_initialization_declare_state_src;
				Source task_initialization_declare_thread_src;
				Source task_initialization_state_src;
				Source task_initialization_state_vars_src;
				Source task_initialization_state_error_src;
				Source task_initialization_state_iss_src;
				Source task_initialization_state_oss_src;
				Source task_initialization_state_nested_src;
				Source task_initialization_state_nested_iss_src;
				Source task_initialization_state_nested_oss_src;
				
				int task_id= *it;
				TaskInfo& task_info= _task_info_vector.at(task_id);
				std::string task_name= task_info.get_name();
				std::string state_name= task_info.get_state_name();
				std::string struct_name= task_info.get_struct_name();
				ObjectList<IdExpression> vars= task_info.get_state();
				ObjectList<int>          inputs= task_info.get_inputs();
				ObjectList<int>          outputs= task_info.get_outputs();
				ObjectList<int>          nested_inputs= task_info.get_nested_inputs();
				ObjectList<int>          nested_outputs= task_info.get_nested_outputs();
				
				Source thread_name;
				
				thread_name	<<  THREAD_NAME << "_" << task_id;  
				
				
				// task_initialization_state_vars_src
				foreach(IdExpression,it2,vars)
				{
					IdExpression& var= *it2;
					std::string var_name= var.prettyprint();
					
					// Sets the value for the variable
					task_initialization_state_vars_src
						<< state_name << "." << var_name
						<< "= "
						<< var_name
						<< ";"
						;
				}

				
				// task_initialization_state_iss_src
				// taskgroup_replace_initialization_connect_src
				// taskgroup_replace_finalization_streams_tasks_iss_src
				foreach(int,it2,inputs)
				{
					int stream_id= *it2;
					StreamInfo& stream_info= _stream_info_vector.at(stream_id);
					
					std::string var_name= stream_info.get_var_name();
					std::string istream_name= stream_info.get_istream_name();
					std::string ostream_name= stream_info.get_ostream_name();

					// Adds state stream creation
					task_initialization_state_iss_src
						<< "istream_create"
						<< "( &" << state_name << "." << istream_name
						<< ", sizeof(" << var_name << ")"
						<< ", &" << state_name << "." << ERROR_NAME
						<< ");"
						;
						
					// Destroy the created stream
					taskgroup_replace_finalization_streams_tasks_iss_src
						<< "istream_destroy"
						<< "( &" << state_name << "." << istream_name
						<< ");"
						;
				}

				
				// task_initialization_state_oss_src
				// taskgroup_replace_finalization_streams_tasks_oss_src
				foreach(int,it2,outputs)
				{
					int stream_id= *it2;
					StreamInfo& stream_info= _stream_info_vector.at(stream_id);
					
					std::string var_name= stream_info.get_var_name();
					std::string istream_name= stream_info.get_istream_name();
					std::string ostream_name= stream_info.get_ostream_name();
					
					// Adds state stream creation
					task_initialization_state_oss_src
						<< "ostream_create"
						<< "( &" << state_name << "." << ostream_name
						<< ", sizeof(" << var_name << ")"
						<< ", &" << state_name << "." << ERROR_NAME
						<< ");"
						;
						
					// Destroy the created stream
					taskgroup_replace_finalization_streams_tasks_oss_src
						<< "ostream_destroy"
						<< "( &" << state_name << "." << ostream_name
						<< ");"
						;
				}

				// task_initialization_state_iss_src
				foreach(int,it2,nested_inputs)
				{
					int stream_id= *it2;
					StreamInfo& stream_info= _stream_info_vector.at(stream_id);

					std::string var_name= stream_info.get_var_name();
					std::string istream_name= stream_info.get_istream_name();
					std::string ostream_name= stream_info.get_ostream_name();
					
					// Adds state stream creation
					task_initialization_state_iss_src
						<< "ostream_create"
						<< "( &" << state_name << "." << ostream_name
						<< ", sizeof(" << var_name << ")"
						<< ", &" << state_name << "." << ERROR_NAME
						<< ");"
						;
						
					// Destroy the created stream
					taskgroup_replace_finalization_streams_tasks_iss_src
						<< "ostream_destroy"
						<< "( &" << state_name << "." << ostream_name
						<< ");"
						;
				}

				// task_initialization_state_oss_src
				foreach(int,it2,nested_outputs)
				{
					int stream_id= *it2;
					StreamInfo& stream_info= _stream_info_vector.at(stream_id);
					
					std::string var_name= stream_info.get_var_name();
					std::string istream_name= stream_info.get_istream_name();
					std::string ostream_name= stream_info.get_ostream_name();
					
					// Adds state stream creation
					task_initialization_state_oss_src
						<< "istream_create"
						<< "( &" << state_name << "." << istream_name
						<< ", sizeof(" << var_name << ")"
						<< ", &" << state_name << "." << ERROR_NAME
						<< ");"
						;

					// Destroy the created stream
					taskgroup_replace_finalization_streams_tasks_oss_src
						<< "istream_destroy"
						<< "( &" << state_name << "." << istream_name
						<< ");"
						;
				}


				// task_initialization_declare_state_src
				task_initialization_declare_state_src
					<< struct_name << " " << state_name << ";"
					; 

				// task_initialization_declare_thread_src
				task_initialization_declare_thread_src
					<< "pthread_t " << thread_name << ";"
					;

				// task_initialization_declare_src
				task_initialization_declare_src
					<< task_initialization_declare_state_src
					<< task_initialization_declare_thread_src
					;
					
				// task_initialization_state_error_src
				task_initialization_state_error_src
					<< "stream_error_create"
					<< "( &" << state_name << "." << ERROR_NAME 
					<< ");"
					;
					
				// task_initialization_state_nested_src
				task_initialization_state_nested_src
					<< task_initialization_state_nested_iss_src
					<< task_initialization_state_nested_oss_src
					;

				// task_initialization_state_src
				task_initialization_state_src
					<< task_initialization_state_vars_src
					<< task_initialization_state_error_src
					<< task_initialization_state_iss_src
					<< task_initialization_state_oss_src
					<< task_initialization_state_nested_src
					;
				
				// task_src
				task_initialization_src
					<< task_initialization_declare_src
					<< task_initialization_state_src
					;
					
				// Add task initialization to taskgroup replace
				taskgroup_replace_initialization_alltask_src
					<< task_initialization_src
					;
				
				// Add thread start to replace code
				taskgroup_replace_start_src
					<< "pthread_create"
					<< "( &" << thread_name
					<< ", (void*)0"
					<< ", " << task_name
					<< ", &" << state_name
					<< ");"
					;
					
				// Add thread join code
				taskgroup_replace_join_src
					<< "pthread_join"
					<< "( " << thread_name
					<< ", (void*)0"
					<< ");"
					;
					
				// Add destroy for the error handler
				taskgroup_replace_finalization_streams_tasks_error_src
					<< "stream_error_destroy"
					<< "( &" << state_name << "." << ERROR_NAME 
					<< ");"
					;
			}

			
						
			// taskgroup_replace_initialization_declare_error_src
			taskgroup_replace_initialization_declare_error_src
				<< "stream_error_t " << ERROR_NAME << "_object"
				<< ";"
				<< "stream_error_t* " << ERROR_NAME
				<< "= "
				<< "&" << ERROR_NAME << "_object"
				<< ";"
				;
			
			// taskgroup_replace_initialization_declare_src
			taskgroup_replace_initialization_declare_src
				<< taskgroup_replace_initialization_declare_iss_src
				<< taskgroup_replace_initialization_declare_oss_src
				<< taskgroup_replace_initialization_declare_error_src
				;

			// taskgroup_replace_initialization_create_error_src
			taskgroup_replace_initialization_create_error_src
				<< "stream_error_create" 
				<< "("
				<< ERROR_NAME
				<< ")"
				<< ";"
				; 
				
			// taskgroup_replace_initialization_create_src
			taskgroup_replace_initialization_create_src
				<< taskgroup_replace_initialization_create_iss_src
				<< taskgroup_replace_initialization_create_oss_src
				<< taskgroup_replace_initialization_create_error_src
				;
			
			// taskgroup_replace_initialization_src
			taskgroup_replace_initialization_src
				<< taskgroup_replace_initialization_declare_src 
				<< taskgroup_replace_initialization_create_src
				<< taskgroup_replace_initialization_alltask_src
				<< taskgroup_replace_initialization_connect_src 
				;
								
			// taskgroup_replace_closes_src
			taskgroup_replace_closes_src
				<< taskgroup_replace_closes_iss_src
				<< taskgroup_replace_closes_oss_src
				;
			
			// taskgroup_replace_body_src
			taskgroup_replace_body_src
				<< "acotrace_iteration_begin();"
				<< "acotrace_run();"
				<< pragma_custom_construct.get_statement().prettyprint()
				<< "acotrace_iteration_end();"
				<< "acotrace_fini();"
				;

			
			// taskgroup_replace_finalization_streams_nested_src
			taskgroup_replace_finalization_streams_nested_src
				<< taskgroup_replace_finalization_streams_nested_iss_src
				<< taskgroup_replace_finalization_streams_nested_oss_src
				;

			// taskgroup_replace_finalization_streams_tasks_src
			taskgroup_replace_finalization_streams_tasks_src
				<< taskgroup_replace_finalization_streams_tasks_iss_src
				<< taskgroup_replace_finalization_streams_tasks_oss_src
				;
				
			// taskgroup_replace_finalization_streams_error_src
			taskgroup_replace_finalization_streams_error_src
				<< "stream_error_destroy"
				<< "( " << ERROR_NAME
				<< ");" 
				;
				
			// taskgroup_replace_finalization_streams_src
			taskgroup_replace_finalization_streams_src
				<< taskgroup_replace_finalization_streams_nested_src
				<< taskgroup_replace_finalization_streams_tasks_src
				<< taskgroup_replace_finalization_streams_error_src
				<< taskgroup_replace_finalization_streams_tasks_error_src
				;
			
			// taskgroup_replace_finalization_src
			taskgroup_replace_finalization_src
				<< taskgroup_replace_finalization_streams_src
				;
								
			// taskgroup_replace_src
			taskgroup_replace_src
				<< "{"
				<<   "acotrace_init(1, " << task_implicit_id << ");"
				<<   taskgroup_replace_initialization_src
				<<   taskgroup_replace_start_src
				<<   taskgroup_replace_body_src
				<<   taskgroup_replace_closes_src
				<<   taskgroup_replace_join_src
				<<   taskgroup_replace_finalization_src
				<<   "acotrace_stop();"
				<< "}"
				;			


			// - Code placement
			// Pragma code substitution
			AST_t taskgroup_replace_tree= taskgroup_replace_src.parse_statement
				( pragma_custom_construct.get_scope()
				, pragma_custom_construct.get_scope_link()
				);
			pragma_custom_construct.get_ast().replace(taskgroup_replace_tree);
		}
		
		

	};
	
}


EXPORT_PHASE(TL::AcotesTransform);
