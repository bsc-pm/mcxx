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
#ifndef CXX_AST_H
#define CXX_AST_H

/*
 * Abstract syntax tree
 */

#include "cxx-macros.h"
#include "cxx-ast-decls.h"

MCXX_BEGIN_DECLS

extern extensible_schema_t ast_extensible_schema;

// Mandatory macros
#define ASTType(a) ((a)->type)
#define ASTParent(a) ((a)->parent)
// ASTLine hardened to avoid problems (not a lvalue)
#define ASTLine(a) (((a) != NULL) ? ((a)->line) : 0)
#define ASTLineLval(a) ((a)->line)
#define ASTText(a) ((a)->text)
#define ASTSon0(a) ASTChild(a, 0)
#define ASTSon1(a) ASTChild(a, 1)
#define ASTSon2(a) ASTChild(a, 2)
#define ASTSon3(a) ASTChild(a, 3)

#define ASTLeaf(node, line, text) ASTMake(node, 0,  NULL, NULL, NULL, NULL, line, text)
#define ASTMake1(node, son0, line, text) ASTMake(node, 1, son0, NULL, NULL, NULL, line, text)
#define ASTMake2(node, son0, son1, line, text) ASTMake(node, 2, son0, son1, NULL, NULL, line, text)
#define ASTMake3(node, son0, son1, son2, line, text) ASTMake(node, 3, son0, son1, son2, NULL, line, text)
#define ASTMake4(node, son0, son1, son2, son3, line, text) ASTMake(node, 4, son0, son1, son2, son3, line, text)

// Convenience macros
#define ASTChild0(a) ASTChild(a, 0)
#define ASTChild1(a) ASTChild(a, 1)
#define ASTChild2(a) ASTChild(a, 2)
#define ASTChild3(a) ASTChild(a, 3)
#define ASTChild(a, n) ((a)->children[n])

#define ASTNumChildren(a) ((a)->num_children)

#define ASTFileName(a) ((a)->filename)

#define ASTListLeaf(a) ASTMake2(AST_NODE_LIST, NULL, a, ASTLine(a), NULL)
#define ASTList(list,element) ASTMake2(AST_NODE_LIST, list, element, ASTLine(list), NULL)

#define ASTAmbiguous(a, b) ASTMake2(AST_AMBIGUITY, a, b, 0, NULL)

// Extensible structure function
#define ASTAttrValue(_a, _name) \
    ( \
      extensible_struct_get_field_pointer(&ast_extensible_schema, (_a)->extended_data, (_name)) \
    )

#define ASTAttrValueType(_a, _name, _type) \
    ( (*(_type*)(ASTAttrValue((_a), (_name)))))

#define ASTAttrSetValueType(_a, _name, _type, _value) \
    ( ASTAttrValueType((_a), (_name), _type) = _value )

// Routine to create a node
AST ASTMake(node_t type, int num_children, const AST son0, const AST son1, const AST son2, const AST son3, int line, const char *text);

char ASTCheck(AST node);
void ASTFree(AST node);

AST duplicate_ast(AST a);

int get_children_num(AST parent, AST children);

char* ast_print_node_type(node_t n);

char ast_equal (AST ast1, AST ast2);
char ast_equal_node (AST ast1, AST ast2);

char* node_information(AST a);

// Eases iterating forward in AST_NODE_LISTs
#define for_each_element(list, iter) \
    iter = (list); while (ASTSon0(iter) != NULL) iter = ASTSon0(iter); \
    for(; iter != NULL; iter = (iter != (list)) ? ASTParent(iter) : NULL)

MCXX_END_DECLS

#endif // CXX_AST_H
