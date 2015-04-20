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



#include "cxx-html.h"
#include "cxx-utils.h"
#include <string.h>

// Very minimal html stripping, DO NOT RELY ON IT FOR CRITICAL MISSIONS
static const char* mini_strip_html(const char* c)
{
    if (c == NULL)
        return "";

    int size = strlen(c);
    if (size == 0)
        return "";

    char output[size * 2];

    const char *p = c;
    char *q = output;

    while (*p != '\0')
    {
        if (q >= &output[size * 2])
            internal_error("string is too large for protection", 0);

        // We only remove < and > but for sure there are many others to be
        // removed
        if (*p == '<')
        {
            if ((q + 3) >= &output[size * 2])
                internal_error("string is too large for protection", 0);

            *q = '&'; q++;
            *q = 'l'; q++;
            *q = 't'; q++;
            *q = ';'; q++;
        }
        else if (*p == '>')
        {
            if ((q + 3) >= &output[size * 2])
                internal_error("string is too large for protection", 0);

            *q = '&'; q++;
            *q = 'g'; q++;
            *q = 't'; q++;
            *q = ';'; q++;
        }
        else
        {
            *q = *p; q++;
        }
        p++;
    }
    *q = '\0';

    return uniquestr(output);
}

static void ast_dump_html_rec(AST a, FILE* f, const char* root_id, int id_node)
{
    if (a == NULL)
        return;

    char current_id_node[64];
    memset(current_id_node, 0, sizeof(current_id_node));
    snprintf(current_id_node, 63, "%s.%d", root_id, id_node);
    current_id_node[63] = '\0';

    fprintf(f, "<div id=\"%s\" class=\"node\" style=\"display: none;\">\n"
            "<div class=\"node_info\">"
            "<span id=\"%s.handle\" class=\"handle\">+</span>"
            "<span id=\"%s.kind\" class=\"node_kind\">%s</span>"
            "<span id=\"%s.locus\" class=\"node_locus\">%s</span>",
            current_id_node, // div
            current_id_node, // handle
            current_id_node, ast_node_type_name(ASTKind(a)),
            current_id_node, mini_strip_html(ast_location(a))
           ); // kind

    if (ASTText(a) != NULL)
    {
        fprintf(f, 
            "<span id=\"%s.text\" class=\"node_text\">%s</span>",
            current_id_node,
            mini_strip_html(ASTText(a)));
    }

    fprintf(f, "</div>\n");

    if (ASTKind(a) == AST_NODE_LIST)
    {
        fprintf(f, "%s", "<div class=\"node_list\">");
        int k = 0;
        AST list = a, iter;
        for_each_element(list, iter)
        {
            AST element = ASTSon1(iter);
            ast_dump_html_rec(element, f, current_id_node, k);

            k++;
        }
        fprintf(f, "%s", "</div>");
    }
    else if (ASTKind(a) == AST_AMBIGUITY)
    {
        fprintf(f, "%s", "<div class=\"node_list\">");
        int num_ambig = ast_get_num_ambiguities(a);
        int i;
        for (i = 0; i < num_ambig; i++)
        {
            AST ambig = ast_get_ambiguity(a, i);

            ast_dump_html_rec(ambig, f, current_id_node, i);
        }
        fprintf(f, "%s", "</div>");
    }
    else
    {
        fprintf(f, "%s", "<div class=\"node_list\">");
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            ast_dump_html_rec(ASTChild(a, i), f, current_id_node, i);
        }
        fprintf(f, "%s", "</div>");
    }

    fprintf(f, "</div>\n");
}

void ast_dump_html(AST a, FILE* f)
{
    fprintf(f, "%s\n", "<html><title>Mercurium HTML tree dump</title>");
    // CSS code
    //
    fprintf(f, "%s\n", "<style type=\"text/css\">");
    fprintf(f, "%s\n", "div.node_list");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    margin-left: 1em;");
    fprintf(f, "%s\n", "    border-left-style: dotted;");
    fprintf(f, "%s\n", "    border-left-width: thin;");
    fprintf(f, "%s\n", "    border-left-color: black;");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "div.node_info");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "display: table;");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "span.handle, span.node_list, span.node_kind, span.node_text, span.node_locus");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "       text-align: left;");
    fprintf(f, "%s\n", "padding: 1em;");
    fprintf(f, "%s\n", "display: table-cell;");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "span.node_list, span.node_kind, span.node_text, span.node_locus");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "width: 20em;");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "span.node_text, span.node_locus");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    font-family: monospace;");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "</style>");

    // Body
    fprintf(f, "%s\n", "<body>");
    
    fprintf(f, "%s\n", "<p>");
    fprintf(f, "%s\n", "<a href='javascript:expandAll(document.getElementById(\"node.0\"));'>Expand all nodes</a>");
    fprintf(f, "%s\n", "<a href='javascript:collapseAll(document.getElementById(\"node.0\"));'>Collapse all nodes</a>");
    fprintf(f, "%s\n", "</p>");

    // Tree dump
    fprintf(f, "%s", "<div class=\"node_list\">");
    ast_dump_html_rec(a, f, "node", 0);
    fprintf(f, "%s", "</div>");

    // Required javascript code
    fprintf(f, "%s\n", "<script language=\"javascript\" type=\"text/javascript\">");
    fprintf(f, "%s\n", "function genericRecursion(e, functor, deep)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    var i;");
    fprintf(f, "%s\n", "    for (i = 0; i < e.childNodes.length; i++)");
    fprintf(f, "%s\n", "    {");
    fprintf(f, "%s\n", "        var n = e.childNodes[i];");
    fprintf(f, "%s\n", "        var match = false;");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "        var attr_map = n.attributes;");
    fprintf(f, "%s\n", "        if (attr_map != null)");
    fprintf(f, "%s\n", "        {");
    fprintf(f, "%s\n", "            var class_attr = attr_map.getNamedItem(\"class\");");
    fprintf(f, "%s\n", "            if (class_attr != null");
    fprintf(f, "%s\n", "                    && class_attr.value == \"node\")");
    fprintf(f, "%s\n", "            {");
    fprintf(f, "%s\n", "                match = true;");
    fprintf(f, "%s\n", "            }");
    fprintf(f, "%s\n", "        }");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "        if (match)");
    fprintf(f, "%s\n", "        {");
    fprintf(f, "%s\n", "            functor(n);");
    fprintf(f, "%s\n", "        }");
    fprintf(f, "%s\n", "            ");
    fprintf(f, "%s\n", "        if (!match || deep)");
    fprintf(f, "%s\n", "        {");
    fprintf(f, "%s\n", "            genericRecursion(n, functor, deep);");
    fprintf(f, "%s\n", "        }");
    fprintf(f, "%s\n", "    }");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function hideImmediateChildren(e)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    genericRecursion(e, function(n) { ");
    fprintf(f, "%s\n", "            n.style.display = \"none\"; ");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "            n.removeEventListener(\"click\", switchNodeEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "            n.removeEventListener(\"mouseout\", onMouseOutEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "            n.removeEventListener(\"mouseover\", onMouseOverEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "            }, ");
    fprintf(f, "%s\n", "            /* deep */ false);");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function collapseNode(e)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    var i;");
    fprintf(f, "%s\n", "    hideImmediateChildren(e);");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "    var hndl = document.getElementById(e.id + \".handle\");");
    fprintf(f, "%s\n", "    if (hndl != null)");
    fprintf(f, "%s\n", "    {");
    fprintf(f, "%s\n", "        hndl.textContent = \"+\";");
    fprintf(f, "%s\n", "    }");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function collapseAll(e)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    genericRecursion(e, collapseNode, /* deep */ true);");
    fprintf(f, "%s\n", "    collapseNode(e);");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function showImmediateChildren(e)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    genericRecursion(e, function(n) ");
    fprintf(f, "%s\n", "            { ");
    fprintf(f, "%s\n", "            n.style.display = \"\"; ");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "            n.addEventListener(\"click\", switchNodeEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "            n.addEventListener(\"mouseout\", onMouseOutEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "            n.addEventListener(\"mouseover\", onMouseOverEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "            }, ");
    fprintf(f, "%s\n", "            /* deep */ false);");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function expandNode(e)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    var i;");
    fprintf(f, "%s\n", "    showImmediateChildren(e);");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "    var hndl = document.getElementById(e.id + \".handle\");");
    fprintf(f, "%s\n", "    if (hndl != null)");
    fprintf(f, "%s\n", "    {");
    fprintf(f, "%s\n", "        hndl.textContent = \"-\";");
    fprintf(f, "%s\n", "    }");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function switchNodeEvent(ev)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    ev.stopPropagation();");
    fprintf(f, "%s\n", "    var e = this;");
    fprintf(f, "%s\n", "    var i;");
    fprintf(f, "%s\n", "    var hndl = document.getElementById(e.id + \".handle\");");
    fprintf(f, "%s\n", "    if (hndl.textContent == \"+\")");
    fprintf(f, "%s\n", "    {");
    fprintf(f, "%s\n", "        expandNode(e);");
    fprintf(f, "%s\n", "    }");
    fprintf(f, "%s\n", "    else");
    fprintf(f, "%s\n", "    {");
    fprintf(f, "%s\n", "        collapseNode(e);");
    fprintf(f, "%s\n", "    }");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function expandAll(e)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    genericRecursion(e, expandNode, /* deep */ true);");
    fprintf(f, "%s\n", "    expandNode(e);");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function doNothingEvent(ev)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    ev.stopPropagation();");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function onMouseOverEvent(ev)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    ev.stopPropagation();");
    fprintf(f, "%s\n", "    var e = this;");
    fprintf(f, "%s\n", "    e.style.background = \"#eee\";");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "function onMouseOutEvent(ev)");
    fprintf(f, "%s\n", "{");
    fprintf(f, "%s\n", "    ev.stopPropagation();");
    fprintf(f, "%s\n", "    var e = this;");
    fprintf(f, "%s\n", "    e.style.background = \"\";");
    fprintf(f, "%s\n", "}");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "var root = document.getElementById(\"node.0\");");
    fprintf(f, "%s\n", "root.style.display = \"\";");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "root.addEventListener(\"mouseout\", onMouseOutEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "root.addEventListener(\"mouseover\", onMouseOverEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "root.addEventListener(\"click\", switchNodeEvent, /* bubble */ false);");
    fprintf(f, "%s\n", "");
    fprintf(f, "%s\n", "</script>");


    // Finish html
    fprintf(f, "%s\n", "</body>");
    fprintf(f, "%s\n", "</html>");
}


