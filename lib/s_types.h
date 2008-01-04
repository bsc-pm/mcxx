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
#ifndef S_TYPES
#define S_TYPES

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __cplusplus
typedef enum
{ false, true }
bool_type;
#else
typedef bool bool_type;
#endif

/*
 *
 *    Prototipo de funcion destructora de un objeto
 *    Object es el objeto a borrar.La función no debe hacer el free si no k 
 *    lo deja listo para que se pueda hacer el free del objeto.
 *     
 */

typedef void delete_func (const void *object);

#ifdef __cplusplus
}
#endif


#endif
