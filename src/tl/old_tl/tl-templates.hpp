/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#ifndef TL_TEMPLATES_HPP
#define TL_TEMPLATES_HPP

#include "cxx-scope-decls.h"

#include "tl-type.hpp"
#include "tl-symbol.hpp"

namespace TL
{
    class Symbol;
    class Type;
    class TemplateArgument;

    //! This class wraps a template parameter
    /*!
      A template parameter is something that appears after a 'template' keyword
      in a template declaration.

      template\<typename _T, int _N, template\<typename\> class _W\>
      struct A { };

      _T, _N and _W are template parameters of kind type, nontype and template
      template parameters.

      Template parameters belong to the template type and not the
      specializations.  Note that template functions can be viewed as template
      types but this is not the case for specializations of template classes
      (which are more like plain types but tagged as specializations of a
      template type).
     */
    class TemplateParameter : public TL::Object
    {
        private:
            template_parameter_list_t* _tpl_param;
            int _index;
        public:
            enum Kind
            {
                INVALID = 0,
                //! This is for template parameters like _T in 'template \<typename _T\>'
                TYPE,
                //! This is for template parameters like _N in 'template \<int _N\>'
                NONTYPE,
                //! This is for template parameters like _W in 'template \<template \<typename\> class _W\>'
                TEMPLATE
            };
        public:
            TemplateParameter(template_parameter_list_t* tpl_param, int index);

            //! Gets the kind of this template parameter
            Kind get_kind() const;

            //! Related symbol of this template parameter
            /*!
              Template parameters information is stored in special symbols
              which live in special template scopes. Depending on the kind
              of template parameter, that symbol contains more or less
              information.

              All template parameters have a position and a nesting in the Symbol.

              TYPE template parameters have a Symbol that holds Symbol::is_type_template_parameter
              and nothing else.

              NONTYPE template parameters have a Symbol that holds Symbol::is_nontype_template_parameter
              and whose Type is the exact type of the nontype template parameter. For instance
              in 'template \<long _L\>', the symbol created to '_L' will have a Type of long.

              TEMPLATE template parameters have a Symbol whose type states that Type::is_template_type.
              You can, then, get its template parameters.
             */
            Symbol get_symbol() const;

            //! States whether the template parameter was declared with a default template argument
            bool has_default_argument() const;
            //! For those template parameters with template argument, returns the template argument
            TemplateArgument get_default_argument() const;

            //! Returns the name of the template parameter
            std::string get_name() const;

            //! Gets the position of this template parameter
            int get_position() const;

            //! Gets the nesting of this template parameter
            int get_nesting() const;
    };

    //! This class wraps a template argument
    /*!
      A template argument is what we put inside < and > when a template-id
      is named. For instance 'A\<int, 3\>'
      
      Every specialization of a template class or a template function has its
      set of template arguments. For template functions, sometimes these
      template arguments are deduced after overload. For classes they are
      always explicit, except if the class had some of the template parameters
      declared with default template arguments.
     */
    class TemplateArgument : public TL::Object
    {
        private:
            template_parameter_list_t* _tpl_param;
            int _index;
        public:
            enum Kind
            {
                INVALID = 0,
                TYPE,
                NONTYPE,
                TEMPLATE
            };
        public:
            TemplateArgument(template_parameter_list_t* tpl_arg, int index);

            //! Returns the kind of the template argument
            Kind get_kind() const;

            //! Returns the type of the template argument
            /*!
              This function is valid only for TYPE and TEMPLATE template
              arguments. It returns the type used as a parameter (which for
              TEMPLATE template arguments will be a template type)
             */
            Type get_type() const;

            //! Returns the expression of the template argument
            /*! 
              This function is valid only for NONTYPE template arguments. It
              returns the expression used as a parameter for a nontype template
              argument.

              You can wrap this tree into an Expression
              */
            DEPRECATED AST_t get_expression() const;

            //! States whether the template argument was defined implicitly
            /*! 
              A template argument can be defined implicitly because its related
              template parameter had a default template argument
             */
            bool is_implicit() const;

            //! Returns the position of the template argument
            /*! Since template arguments can be bound to things without name
              we need a way to identify them. The position is a, zero based, counter
              of the position of the template argument inside the template-id (or in
              deduced contexts, the equivalent deduced template-id)
              */
            int get_position() const;

            //! Returns the nesting of the template argument
            /*! Most of the time this value will be 1. Sometimes, for template members
              of template classes, this value will be higher
             */
            int get_nesting() const;
    };
}

#endif // TL_TEMPLATES_HPP
