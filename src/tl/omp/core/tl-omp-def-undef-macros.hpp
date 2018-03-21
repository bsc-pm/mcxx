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


// This header provides a useful set of macros to define some generic
// implementations of pragma handlers. It should be used in the following way:
//
//      #define CLASSNAME ABCD
//      #include "tl-omp-def-undef-macro.hpp"
//
//      /* use some of the macros defined in the header */
//
//      #include "tl-omp-def-undef-macros.hpp"
//
// IMPORTANT: before including this header, CLASSNAME must be defined to the
// name of the class that inherits from TL::PragmaCustomCompilerPhase


// INTERNAL IMPLEMENTATION
// How this header works: the first time this header is included, it adds some
// macros's definitions. Once we don't need those macros anymore, we should
// include this header again, removing the macros that were included before.

#ifndef OMP_DEF_UNDEF_MACROS_HPP

    #define OMP_DEF_UNDEF_MACROS_HPP

    #ifndef CLASSNAME
        #error "Macco 'CLASSNAME' must be defined before including the 'tl-omp-def-undef-macros.hpp' header"
    #endif

    /* INVALID STATEMENT HANDLER */
    #define INVALID_STATEMENT_HANDLER(_func_prefix, _name) \
        void CLASSNAME::_func_prefix##_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf_at(ctr.get_locus(), "invalid '#pragma %s %s'\n",  \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void CLASSNAME::_name##_handler_post(TL::PragmaCustomStatement) { }

    #define OMP_INVALID_STATEMENT_HANDLER(_name) INVALID_STATEMENT_HANDLER( /*empty_prefix*/ , _name)
    #define OSS_INVALID_STATEMENT_HANDLER(_name) INVALID_STATEMENT_HANDLER(oss_ , _name)


    /* INVALID DECLARATION HANDLER */
    #define INVALID_DECLARATION_HANDLER(_func_prefix, _name) \
        void CLASSNAME::_func_prefix##_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
            error_printf_at(ctr.get_locus(), "invalid '#pragma %s %s'\n",  \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void CLASSNAME::_func_prefix##_name##_handler_post(TL::PragmaCustomDeclaration) { }

    #define OMP_INVALID_DECLARATION_HANDLER(_name) INVALID_DECLARATION_HANDLER( /*empty_prefix*/ , _name)
    #define OSS_INVALID_DECLARATION_HANDLER(_name) INVALID_DECLARATION_HANDLER(oss_ , _name)


    /* EMPTY STATEMENT HANDLER */
    #define EMPTY_STATEMENT_HANDLER(_func_prefix,_name) \
        void CLASSNAME::_func_prefix##_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void CLASSNAME::_func_prefix##_name##_handler_post(TL::PragmaCustomStatement) { }

    #define OMP_EMPTY_STATEMENT_HANDLER(_name) EMPTY_STATEMENT_HANDLER(/*empty_prefix*/, _name)
    #define OSS_EMPTY_STATEMENT_HANDLER(_name) EMPTY_STATEMENT_HANDLER(oss_, _name)


    /* EMPTY DECLARATION HANDLER */
    #define EMPTY_DECLARATION_HANDLER(_func_prefix,_name) \
        void CLASSNAME::_func_prefix##_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void CLASSNAME::_func_prefix##_name##_handler_post(TL::PragmaCustomDeclaration) { }

    #define OMP_EMPTY_DECLARATION_HANDLER(_name) EMPTY_DECLARATION_HANDLER(/*empty_prefix*/, _name)
    #define OSS_EMPTY_DECLARATION_HANDLER(_name) EMPTY_DECLARATION_HANDLER(oss_, _name)


    /* EMPTY DIRECTIVE HANDLER */
    #define EMPTY_DIRECTIVE_HANDLER(_func_prefix,_name) \
        void CLASSNAME::_func_prefix##_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void CLASSNAME::_func_prefix##_name##_handler_post(TL::PragmaCustomDirective) { }

    #define OMP_EMPTY_DIRECTIVE_HANDLER(_name) EMPTY_DIRECTIVE_HANDLER(/*empty_prefix*/, _name)
    #define OSS_EMPTY_DIRECTIVE_HANDLER(_name) EMPTY_DIRECTIVE_HANDLER(oss_, _name)


    /* UNIMPLEMENTED HANDLER STATEMENT */
    #define UNIMPLEMENTED_STATEMENT_HANDLER(_func_prefix, _name) \
        void CLASSNAME::_func_prefix##_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf_at(ctr.get_locus(), "OpenMP construct not implemented\n");\
        } \
        void CLASSNAME::_func_prefix##_name##_handler_post(TL::PragmaCustomStatement) { }

    #define OMP_UNIMPLEMENTED_STATEMENT_HANDLER(_name) UNIMPLEMENTED_STATEMENT_HANDLER(/*empty_prefix*/, _name)
    #define OSS_UNIMPLEMENTED_STATEMENT_HANDLER(_name) UNIMPLEMENTED_STATEMENT_HANDLER(oss_, _name)


    /* OSS_TO_OMP_STATEMENT_HANDLER */
    #define OSS_TO_OMP_STATEMENT_HANDLER(_name) \
    void CLASSNAME::oss_##_name##_handler_pre(TL::PragmaCustomStatement construct) {\
        _name##_handler_pre(construct); \
    } \
    void CLASSNAME::oss_##_name##_handler_post(TL::PragmaCustomStatement construct) {\
        _name##_handler_post(construct); \
    }

    /* OSS_TO_OMP_DECLARATION_HANDLER */
    #define OSS_TO_OMP_DECLARATION_HANDLER(_name) \
        void CLASSNAME::oss_##_name##_handler_pre(TL::PragmaCustomDeclaration construct) {\
            _name##_handler_pre(construct); \
        } \
        void CLASSNAME::oss_##_name##_handler_post(TL::PragmaCustomDeclaration construct) {\
            _name##_handler_post(construct); \
        }

    /* OSS_TO_OMP_DIRECTIVE_HANDLER */
    #define OSS_TO_OMP_DIRECTIVE_HANDLER(_name) \
        void CLASSNAME::oss_##_name##_handler_pre(TL::PragmaCustomDirective construct) {\
            _name##_handler_pre(construct); \
        } \
        void CLASSNAME::oss_##_name##_handler_post(TL::PragmaCustomDirective construct) {\
            _name##_handler_post(construct); \
        }

#else

    #undef OSS_TO_OMP_DIRECTIVE_HANDLER
    #undef OSS_TO_OMP_DECLARATION_HANDLER
    #undef OSS_TO_OMP_STATEMENT_HANDLER

    #undef OSS_UNIMPLEMENTED_STATEMENT_HANDLER
    #undef OMP_UNIMPLEMENTED_STATEMENT_HANDLER
    #undef     UNIMPLEMENTED_STATEMENT_HANDLER

    #undef OSS_EMPTY_DIRECTIVE_HANDLER
    #undef OMP_EMPTY_DIRECTIVE_HANDLER
    #undef     EMPTY_DIRECTIVE_HANDLER

    #undef OSS_EMPTY_DECLARATION_HANDLER
    #undef OMP_EMPTY_DECLARATION_HANDLER
    #undef     EMPTY_DECLARATION_HANDLER

    #undef OSS_EMPTY_STATEMENT_HANDLER
    #undef OMP_EMPTY_STATEMENT_HANDLER
    #undef     EMPTY_STATEMENT_HANDLER

    #undef OSS_INVALID_DECLARATION_HANDLER
    #undef OMP_INVALID_DECLARATION_HANDLER
    #undef     INVALID_DECLARATION_HANDLER

    #undef OSS_INVALID_STATEMENT_HANDLER
    #undef OMP_INVALID_STATEMENT_HANDLER
    #undef     INVALID_STATEMENT_HANDLER

    #undef CLASSNAME

    #undef OMP_DEF_UNDEF_MACROS_HPP

#endif // OMP_DEF_UNDEF_MACROS_HPP
