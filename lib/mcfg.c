/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

    // This file derives from Samba source but now it has been heavily modified
    // so just little bits of the original code remain
*/
#include "mcfg.h"
#include <string.h>

#define BUFR_INC    1024

#define MAX_FLAG_LENGTH (256)
#define MAX_FLAG_NUM (32)

#define SECTION_LENGTH (256)
#define OPTION_LENGTH (256)
#define VALUE_LENGTH (1024)

/*
** local function prototypes
*/
static FILE *openConfFile (char *filename);
static int Parse (FILE * fp, int (*sfunc) (char *),
          int (*pfunc) (char * option, char * value, int num_flags, char** flags));
static int Section (FILE * fp, int (*sfunc) (char *));
static int eatWhitespace (FILE * fp);
static int Parameter (FILE * fp, int (*pfunc) (char * option, char * value, int num_flags, char** flags), int c);

/*
**  Parameter()
**  scan a parameter name (or name and value pair) and pass the value (or
**  values) to function pfunc().
**
**  Parameters:
**      fp      - open FILE pointer
**      pfunc   - a pointer to the function that will be called to process
**                the parameter, once it has been scanned
**      c       - the first character of the parameter name, which would
**                have been read by Parse(). unlike comment line or a section
**                header, there's no lead-in character can be discarded.
**
**  Return Values:
**      0       on success
**      -1      on failure
**
**  Limitations and Comments:
**
**
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-08-1998    first cut
*/

static int
Parameter (FILE * fp, int (*pfunc) (char * option, char * value, int num_flags, char **flags), int c)
{
    int option_length = 0;
    char option[OPTION_LENGTH];


    int num_flags = 0;
    char _flags[MAX_FLAG_NUM][MAX_FLAG_LENGTH];

    char *flags[MAX_FLAG_NUM];
    {
        // Initialize the array of pointers with the static storage
        int i;
        for (i = 0; i < MAX_FLAG_NUM; i++)
        {
            flags[i] = _flags[i];
        }
    }


    // First parse flags if any
    int p = c;

    // This is the beginning of a flag
    if (p == '{')
    {
        char flags_finished = 0;

        int current_flag_length = 0;
        char current_flag[MAX_FLAG_LENGTH];

        // After '{' there may be blanks
        p = eatWhitespace(fp);
        while (!flags_finished)
        {
            switch (p)
            {
                case EOF : 
                    {
                        fprintf(stderr, "Unexpected end of file while parsing parameter of configuration file\n");
                        return (-1);
                    }
                case '}' :
                    {
                        // No more parameters
                        flags_finished = 1;
                        /* Fall-through! */
                    }
                case ',' :
                    {
                        if (current_flag_length == MAX_FLAG_LENGTH)
                        {
                            fprintf(stderr, "Flag too long\n");
                            return -1;
                        }
                        if (current_flag_length == 0
                                || (current_flag_length == 1 
                                    && current_flag[0] == '!'))
                        {
                            fprintf(stderr, "Empty flag\n");
                            return -1;
                        }

                        // Terminate the string
                        current_flag[current_flag_length] = '\0';
                        current_flag_length++;

                        if (num_flags == MAX_FLAG_NUM)
                        {
                            fprintf(stderr, "Too many flags\n");
                            return (-1);
                        }

                        strncpy(flags[num_flags], current_flag, current_flag_length);
                        num_flags++;

                        // Restart the flag
                        current_flag_length = 0;

                        // After a ',' or '}' we allow blanks
                        p = eatWhitespace(fp);
                        break;
                    }
                case '\n':
                    {
                        fprintf(stderr, "Invalid newline in flags\n");
                        return -1;
                        break;
                    }
                case ' ':
                case '\t':
                    {
                        fprintf(stderr, "Invalid whitespace in flags\n");
                        return -1;
                        break;
                    }
                default:
                    {
                        if (p == '!') // Negation
                        {
                            if (current_flag_length == MAX_FLAG_LENGTH)
                            {
                                fprintf(stderr, "Flag too long\n");
                                return -1;
                            }

                            current_flag[current_flag_length] = p;
                            current_flag_length++;

                            p = eatWhitespace(fp);
                        }

                        // Advance as many as possible
                        while (p != ' ' // Blank
                                && p != '\t' // Blank
                                && p != '\n' // newline
                                && p != ',' // Comma
                                && p != '}' // End of flags
                                && p != '!' // Negation
                                && p != EOF
                              )
                        {
                            if (current_flag_length == MAX_FLAG_LENGTH)
                            {
                                fprintf(stderr, "Flag too long\n");
                                return -1;
                            }

                            current_flag[current_flag_length] = p;
                            current_flag_length++;

                            p = getc(fp);
                        }

                        // Maybe this is the end of a flag
                        if (p == ' ' || p == '\t')
                        {
                            p = eatWhitespace(fp);
                        }
                        break;
                    }
            }
        }
    }
    
    // We allow this because it is more flexible when autogenerating the
    // configure
    if (p == '\n')
    {
        // Do not do anything but returning success
        return 0;
    }

    char option_finished = 0;
    while (!option_finished)
    {
        switch (p)
        {
            case EOF :
                {
                    fprintf(stderr, "Unexpected end of file when parsing option\n");
                    return -1;
                    break;
                }
            case '=' :
                {
                    if (option_length == 0)
                    {
                        fprintf(stderr, "Empty option\n");
                        return -1;
                    }

                    if (option_length == OPTION_LENGTH)
                    {
                        fprintf(stderr, "Option too long\n");
                        return -1;
                    }

                    // Terminate the string
                    option[option_length] = '\0';
                    option_length++;

                    option_finished = 1;

                    // Allow whitespaces after the finished option
                    p = eatWhitespace(fp);
                    break;
                }
            case '\n':
                {
                    fprintf(stderr, "Unexpected end of line when parsing option\n");
                    return -1;
                    break;
                }
            case ' ' :
            case '\t' :
                {
                    fprintf(stderr, "Invalid whitespace when parsing option\n");
                    return -1;
                    break;
                }
            default:
                {
                    while (p != EOF
                            && p != '='
                            && p != '\n'
                            && p != ' '
                            && p != '\t')
                    {
                        if (option_length == OPTION_LENGTH)
                        {
                            fprintf(stderr, "Option too long\n");
                            return -1;
                        }

                        option[option_length] = p;
                        option_length++;

                        p = getc(fp);
                    }

                    // Maybe this is the end of the option
                    if (p == ' ' || p == '\t')
                    {
                        p = eatWhitespace(fp);
                    }
                    break;
                }
        }
    }

    int value_length = 0;
    char value[VALUE_LENGTH];

    // Get the rest of the line and trim at the same time
    // This is -1 because in empty strings there is no last non blank
    // but there will be '\0' so when incremented it will be 0
    int last_nonblank = -1;
    while (p != '\n'
            && p != EOF)
    {
        if (value_length == VALUE_LENGTH)
        {
            fprintf(stderr, "Value for option too long\n");
            return -1;
        }

        // Store if this is nonblank
        if (p != ' '
                && p != '\t')
        {
            last_nonblank = value_length;
        }

        value[value_length] = p;
        value_length++;

        p = getc(fp);
    }

    // Terminate the string
    value[last_nonblank + 1] = '\0';
    value_length = last_nonblank + 1;

    /*
    fprintf(stderr, "[MCFG] Option -> '%s'\n", option);
    fprintf(stderr, "[MCFG] Value -> '%s'\n", value);
    fprintf(stderr, "[MCFG] num_flags -> '%d'\n", num_flags);
    {
        int i;
        for (i = 0; i < num_flags; i++)
        {
            fprintf(stderr, "[MCFG] flags[%d] -> '%s'\n", i, flags[i]);
        }
    }
    fprintf(stderr, "[MCFG] \n");
    */

    return pfunc(option, value, num_flags, flags);
}

/*
**  openConfFile()
**  open the configuration file
**
**  Parameters:
**      filename    - the pathname of the configuration fle.
**
**  Return Values:
**      a pointer to type (FILE *) to the opened file, or
**      NULL if the file could not be opened.
**
**  Limitations and Comments:
**      taken from samba source.
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-08-1998    first cut
*/


static FILE *
openConfFile (char *filename)
{
    FILE *fp = (FILE *) NULL;

    if ((filename == NULL) || (*filename == '\0'))
    {
        (void) fprintf (stderr, "No config file specified.\n");
        return ((FILE *) NULL);
    }

    fp = fopen (filename, "r");
    if (fp == (FILE *) NULL)
    {
        (void) fprintf (stderr, "Unable to open config file '%s'\n", filename);
        return ((FILE *) NULL);
    }

    return (fp);

}


/*
**  param_process()
**  process the named parameter file
**
**  Parameters:
**      filename    - the pathname of the file to be opened
**
**      sfunc       - a pointer to a function that will be called when a 
**                    section name is discovered.
**      pfunc       - a pointer to a function that will be called when a
**                    parameter name/value are discovered
**
**  Return Values:
**      0   if successfully parsed
**      -1  if failed to open the file for reading
**      -2  malloc failed
**      -3  parse error
**
**  Limitations and Comments:
**      adapted from samba source code
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-08-1998    first cut
**                           Mar-02-1999    more error codes
*/

int
param_process (char *filename,
          int (*sfunc) (char *), int (*pfunc) (char * option, char * value, int num_flags, char** flags))
{
    char *func = "params.c:param_process() -";

    int result;

    FILE *fp;

    /* open the conf file */
    fp = openConfFile (filename);
    if (fp == (FILE *) NULL)
        return (PPR_OPEN_FILE_ERROR);

    result = Parse (fp, sfunc, pfunc);

    fclose (fp);
    if (result < 0)
    {
        fprintf (stderr, "%s failed. error returned from Parse()\n",
                func);
        return (PPR_PARSE_ERROR);
    }

    return (PPR_SUCCESS);
}

/*
** scan to the end of a comment
*/
static int
eatComment (FILE * fp)
{
    int c;
    for (c = getc (fp); ('\n' != c) && (EOF != c) && (c > 0); c = getc (fp))
        ;

    return (c);
}


static int
eatWhitespace (FILE * fp)
{
    int c;

    for (c = getc (fp); isspace (c) && ('\n' != c); c = getc (fp))
        ;
    return (c);
}

/*
**  
**
**  Parameters:
**      fp      - open FILE pointer
**      sfunc   - function to be called when a section name is scanned.
**      pfunc   - function to be called when a parameter is scanned.
**
**  Return Values:
**      0  on success
**      -1 on failure
**
**  Limitations and Comments:
**      from samba source code
**
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-09-1998    first cut
*/

static int
Parse (FILE * fp, int (*sfunc) (char *), int (*pfunc) (char * option, char * value, int num_flags, char** flags))
{
    int c;
    c = eatWhitespace (fp);

    while ((c != EOF) && (c > 0))
    {

        switch (c)
        {
            case '\n':      /* blank line */
                {
                    c = eatWhitespace (fp);
                    break;
                }

            case ';':       /* comment line */
            case '#':
                {
                    c = eatComment (fp);
                    break;
                }

            case '[':       /* section header */
                {
                    if (Section (fp, sfunc) < 0)
                    {
                        return (-1);
                    }
                    c = eatWhitespace (fp);
                    break;
                }
            default:        /* parameter line */
                {
                    if (Parameter (fp, pfunc, c) < 0)
                        return (-1);
                    c = eatWhitespace (fp);
                }
        }
    }

    return 0;
}

/*
**  Section()
**  scan a section name and pass the name to the function sfunc()
**
**  Parameters:
**      fp      - open FILE pointer
**      sfunc   - pointer to the function to be called if the section name
**                is successfully read.
**
**  Return Values:
**      0   if the section name was read and 0 was returned from <sfunc>
**      -1  if <sfuc> failed or if a lexical error was encountered.
**
**  Limitations and Comments:
**      from samba source code
**
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-10-1998    first cut
*/

static int Section (FILE * fp, int (*sfunc) (char *))
{
    int p;
    p = eatWhitespace (fp);   /* 
                               ** we've already got the '['. scan past initial
                               ** white space
                               */

    char section[SECTION_LENGTH];
    int section_length = 0;

    char section_finished = 0;
    while (!section_finished)
    {
        switch (p)
        {
            case ']' :
                {
                    if (section_length == 0)
                    {
                        fprintf(stderr, "Empty section length\n");
                        return -1;
                        break;
                    }

                    if (section_length == SECTION_LENGTH)
                    {
                        fprintf(stderr, "Section name too long\n");
                        return -1;
                    }

                    // Terminate the string
                    section[section_length] = '\0';
                    section_length++;

                    section_finished = 1;
                    break;
                }
            case ' ' :
            case '\t' :
                {
                    fprintf(stderr, "Unexpected whitespace in section name\n");
                    return -1;
                    break;
                }
            case EOF :
                {
                    fprintf(stderr, "Unexpected end of file\n");
                    return -1;
                    break;
                }
            case '\n':
                {
                    fprintf(stderr, "Unexpected end of file\n");
                    return -1;
                    break;
                }
            default:
                {
                    while (p != ']'
                            && p != '\n'
                            && p != ' '
                            && p != '\t'
                            && p != EOF)
                    {
                        if (section_length == SECTION_LENGTH)
                        {
                            fprintf(stderr, "Section name too long\n");
                        }

                        section[section_length] = p;
                        section_length++;

                        p = getc(fp);
                    }

                    // Maybe this is the end of name
                    if (p == ' '
                            || p == '\t')
                    {
                        p = eatWhitespace(fp);
                    }
                }
        }
    }

    /*
    fprintf(stderr, "[MCFG] Section -> '%s'\n", section);
    fprintf(stderr, "[MCFG] \n");
    */

    return sfunc(section);
}
