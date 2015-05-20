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



#ifndef TL_MODULES_HPP
#define TL_MODULES_HPP

#include "tl-common.hpp"
#include "tl-modules-fwd.hpp"
#include "tl-symbol.hpp"
#include "tl-nodecl.hpp"
#include "fortran03-modules-data.h"

namespace TL
{
    // Writer

    class LIBTL_CLASS ModuleWriter
    {
        public:
            ModuleWriter(TL::Symbol module, const std::string domain);

            void builtin_write(unsigned int);
            void builtin_write(int);
            void builtin_write(bool);
            void builtin_write(const std::string& str);

            void builtin_write(TL::Symbol);
            void builtin_write(TL::Type);
            void builtin_write(Nodecl::NodeclBase);
            void builtin_write(TL::Scope);

            template <typename T>
                void write(const T& t)
                {
                    ModuleWriterTrait<T>::write(*this, const_cast<T&>(t));
                }

            template <typename T>
                void write(T& t)
                {
                    ModuleWriterTrait<T>::write(*this, t);
                }

            void commit();
        private:
            TL::Symbol _module;
            std::string _domain;
            TL::ObjectList<tl_type_t> _tl_values;
    };
    
    template <typename T>
        struct ModuleWriterTrait
        {
            static void write(ModuleWriter& mw, T& d)
            {
                d.module_write(mw);
            }
        };

    template <typename T>
        struct ModuleWriterTrait<TL::ObjectList<T> >
        {
            static void write(ModuleWriter& mw, TL::ObjectList<T>& d)
            {
                mw.write((int)d.size());
                for (typename TL::ObjectList<T>::iterator it = d.begin();
                        it != d.end();
                        it++)
                {
                    ModuleWriterTrait<T>::write(mw, *it);
                }
            }
        };

    template <typename T, typename P = T>
        struct BuiltinModuleWriterTrait
        {
            static void write(ModuleWriter& mw, P d)
            {
                mw.builtin_write(d);
            }
        };

    template <>
    struct ModuleWriterTrait<unsigned int> : public BuiltinModuleWriterTrait<unsigned int> { };
        
    template <>
    struct ModuleWriterTrait<int> : public BuiltinModuleWriterTrait<int> { };

    template <>
    struct ModuleWriterTrait<bool> : public BuiltinModuleWriterTrait<bool> { };

    template <>
    struct ModuleWriterTrait<std::string> : public BuiltinModuleWriterTrait<std::string, const std::string&> { };
    
    template <>
    struct ModuleWriterTrait<TL::Symbol> : public BuiltinModuleWriterTrait<TL::Symbol> { };

    template <>
    struct ModuleWriterTrait<TL::Type> : public BuiltinModuleWriterTrait<TL::Type> { };

    template <>
    struct ModuleWriterTrait<Nodecl::NodeclBase> : public BuiltinModuleWriterTrait<Nodecl::NodeclBase> { };

    template <>
    struct ModuleWriterTrait<TL::Scope> : public BuiltinModuleWriterTrait<TL::Scope> { };

    template <typename T, typename Q>
        struct ModuleWriterTrait<std::map<T, Q> >
        {
            typedef std::map<T, Q> Map;

            static void write(ModuleWriter& mw, Map& table)
            {
                mw.write((int)table.size());
                for (typename Map::iterator it = table.begin();
                        it != table.end();
                        it++)
                {
                    mw.write(it->first);
                    mw.write(it->second);
                }
            }
        };

    template <typename T, typename Q>
        struct ModuleWriterTrait<std::multimap<T, Q> >
        {
            typedef std::multimap<T, Q> MultiMap;

            static void write(ModuleWriter& mw, MultiMap& table)
            {
                mw.write((int)table.size());
                for (typename MultiMap::iterator it = table.begin();
                        it != table.end();
                        it++)
                {
                    mw.write(it->first);
                    mw.write(it->second);
                }
            }
        };

    template <typename E>
        struct EnumWriterTrait
        {
            static void write(ModuleWriter& mw, E& enum_val)
            {
                mw.write((int)enum_val);
            }
        };

    template <>
        struct ModuleWriterTrait<const locus_t* >
        {
            static void write(ModuleWriter& mw, const locus_t* &d)
            {
                std::string filename = locus_get_filename(d);
                unsigned int line = locus_get_line(d);
                unsigned int column = locus_get_column(d);

                mw.write(filename);
                mw.write(line);
                mw.write(column);
            }
        };

    // Reader
    class LIBTL_CLASS ModuleReader
    {
        public:
            ModuleReader(TL::Symbol module, const std::string& domain);

            void builtin_read(unsigned int&);
            void builtin_read(int&);
            void builtin_read(TL::Symbol&);
            void builtin_read(TL::Type&);
            void builtin_read(std::string& str);
            void builtin_read(bool&);
            void builtin_read(Nodecl::NodeclBase&);
            void builtin_read(TL::Scope&);

            template <typename T>
                void read(T& t)
                {
                    ModuleReaderTrait<T>::read(*this, t);
                }

            bool empty() const;
        private:
            fortran_modules_data_t *_data;
            int _cursor;

            tl_type_t& read_item_from_module();
    };

    template <typename T>
        struct ModuleReaderTrait
        {
            static void read(ModuleReader& mr, T& d)
            {
                d.module_read(mr);
            }
        };


    template <typename T>
        struct ModuleReaderTrait<TL::ObjectList<T> >
        {
            static void read(ModuleReader& mr, TL::ObjectList<T>& d)
            {
                int num_items = 0;
                mr.read(num_items);
                for (int i = 0; i < num_items; i++)
                {
                    T t;
                    ModuleReaderTrait<T>::read(mr, t);
                    d.append(t);
                }
            }
        };


    template <typename T, typename P = T>
        struct BuiltinModuleReaderTrait
        {
            static void read(ModuleReader& mr, P& d)
            {
                mr.builtin_read(d);
            }
        };

    template <typename E>
        struct EnumReaderTrait
        {
            static void read(ModuleReader& mr, E& enum_val)
            {
                int i;
                mr.read(i);
                enum_val = E(i);
            }
        };

    template <typename T, typename Q>
        struct ModuleReaderTrait<std::map<T, Q> >
        {
            typedef std::map<T, Q> Map;

            static void read(ModuleReader& mr, Map& table)
            {
                int n;
                mr.read(n);

                for (int i = 0; i < n; i++)
                {
                    T t; Q q;
                    mr.read(t);
                    mr.read(q);

                    table.insert(std::make_pair(t, q));
                }
            }
        };

    template <typename T, typename Q>
        struct ModuleReaderTrait<std::multimap<T, Q> >
        {
            typedef std::multimap<T, Q> MultiMap;

            static void read(ModuleReader& mr, MultiMap& table)
            {
                int n;
                mr.read(n);

                for (int i = 0; i < n; i++)
                {
                    T t; Q q;
                    mr.read(t);
                    mr.read(q);

                    table.insert(std::make_pair<T, Q>(t, q));
                }
            }
        };

    template <>
    struct ModuleReaderTrait<unsigned int> : public BuiltinModuleReaderTrait<unsigned int> { };
    
    template <>
    struct ModuleReaderTrait<int> : public BuiltinModuleReaderTrait<int> { };

    template <>
    struct ModuleReaderTrait<bool> : public BuiltinModuleReaderTrait<bool> { };

    template <>
    struct ModuleReaderTrait<std::string> : public BuiltinModuleReaderTrait<std::string> { };

    template <>
    struct ModuleReaderTrait<TL::Symbol> : public BuiltinModuleReaderTrait<TL::Symbol> { };

    template <>
    struct ModuleReaderTrait<TL::Type> : public BuiltinModuleReaderTrait<TL::Type> { };

    template <>
    struct ModuleReaderTrait<Nodecl::NodeclBase> : public BuiltinModuleReaderTrait<Nodecl::NodeclBase> { };

    template <>
    struct ModuleReaderTrait<TL::Scope> : public BuiltinModuleReaderTrait<TL::Scope> { };

    template <>
        struct ModuleReaderTrait<const locus_t* >
        {
            static void read(ModuleReader& mr, const locus_t* &d)
            {
                std::string filename;
                unsigned int line = 0;
                unsigned int column = 0;

                mr.read(filename);
                mr.read(line);
                mr.read(column);

                d = make_locus(filename.c_str(), line, column);
            }
        };
}

#endif // TL_MODULES_HPP
