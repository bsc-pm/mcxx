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
template <typename _CharT, typename _Traits = _CharT*, typename _Alloc = _Traits**>
struct basic_istream
{
};

template <typename _CharT, typename _Traits = _CharT*, typename _Alloc = _Traits**>
struct basic_string
{
};

template<typename _CharT, typename _Traits, typename _Alloc>
  basic_istream<_CharT, _Traits>&
  operator>>(basic_istream<_CharT, _Traits>& __is,
      basic_string<_CharT, _Traits, _Alloc>& __str);

template<>
  basic_istream<char>&
  operator>>(basic_istream<char>& __is, basic_string<char>& __str);
