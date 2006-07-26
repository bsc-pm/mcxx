#include "mcfg.h"

#define BUFR_INC    1024

static char *bufr = (char *) NULL;
static int bsize = 0;
static char *cursec = (char *) NULL;

/*
** local function prototypes
*/
static FILE *openConfFile (char *filename);
static int Parse (FILE * fp, int style, int (*sfunc) (char *),
		  int (*pfunc) (char *, char *));
static int Section (FILE * fp, int (*sfunc) (char *));
static int Continuation (char *line, int pos);
static char *loc_GC_REALLOC (char *p, int size);
static int eatWhitespace (FILE * fp);
static int Parameter (FILE * fp, int style,
		      int (*pfunc) (char *, char *), int c);
// static void regSection (char **s);

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
**      style   - the style of the config file. it can be MS_STYLE, that is
**                parameter must follows by = and the value. If it is
**                NOT_MS_STYLE, then parameter does not follows by = or the
**                value.
**                  Example of MS_STYLE config file:
**                      [section]
**                          version = 2.4
**                      [foo]
**                          bar=hello
**                          foobar=world
**
**                  Example of NOT_MS_STYLE config file:
**                      [section]
**                           2.4
**                      [foo]
**                          hello
**                          world
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
Parameter (FILE * fp, int style, int (*pfunc) (char *, char *), int c)
{
  int i = 0;			/* position withing bufr */
  int end = 0;			/* bufr[end] is current end-of-string */
  int vstart = 0;		/* starting position of the parameter */

  char *func = "params.c:Parameter() -";

  if (style == MS_STYLE)
    {
      /*
       ** loop until we found the start of the value 
       */
      while (vstart == 0)
	{
	  /*
	   ** ensure there's space for next char 
	   */
	  if (i > (bsize - 2))
	    {
	      bsize += BUFR_INC;
	      bufr = loc_GC_REALLOC (bufr, bsize);
	      if (bufr == NULL)
		{
		  (void) fprintf (stderr, "%s GC_MALLOC failed\n", func);
		  return (-1);
		}
	    }

	  switch (c)
	    {
	    case '=':
	      {
		if (end == 0)
		  {
		    (void) fprintf (stderr, "%s invalid parameter name\n",
				    func);
		    return (-1);
		  }
		bufr[end++] = '\0';
		i = end;
		vstart = end;
		bufr[i] = '\0';
		break;
	      }

	    case '\n':
	      {
		i = Continuation (bufr, i);
		if (i < 0)
		  {
		    bufr[end] = '\0';
		    (void) fprintf (stderr,
				    "%s ignoring badly formed line in config file\n",
				    func);
		    return (0);
		  }

		end = ((i > 0) && (bufr[i - 1] == ' ')) ? (i - 1) : (i);
		c = getc (fp);
		break;
	      }

	    case '\0':
	    case EOF:
	      {
		bufr[i] = '\0';
		(void) fprintf (stderr,
				"%s unexpected end-of-file at %s: func\n",
				func, bufr);
		return (0);
		break;
	      }

	    default:
	      {
		if (isspace (c))
		  {
		    bufr[end] = ' ';
		    i = end + 1;
		    c = eatWhitespace (fp);
		  }
		else
		  {
		    bufr[i++] = c;
		    end = i;
		    c = getc (fp);
		  }
		break;
	      }
	    }
	}

      /*
       ** now parse the value
       */
      c = eatWhitespace (fp);
    }				/* MS_STYLE */

  while ((c != EOF) && (c > 0))
    {
      if (i > (bsize - 2))
	{
	  bsize += BUFR_INC;
	  bufr = loc_GC_REALLOC (bufr, bsize);
	  if (bufr == NULL)
	    {
	      (void) fprintf (stderr, "%s GC_MALLOC failed\n", func);
	      return (-1);
	    }
	}

      switch (c)
	{
	case '\r':
	  {
	    c = getc (fp);
	    break;
	  }

	case '\n':
	  {
	    i = Continuation (bufr, i);
	    if (i < 0)
	      c = 0;
	    else
	      {
		for (end = i; (end >= 0) && isspace (bufr[end]); end--)
		  ;
		c = getc (fp);
	      }
	    break;
	  }

	default:
	  {
	    bufr[i++] = c;
	    if (!isspace (c))
	      end = i;
	    c = getc (fp);
	    break;
	  }

	}
    }

  bufr[end] = '\0';
  return (pfunc (bufr, &bufr[vstart]));
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

  char *func = "params.c:openConfFile() -";

  if ((filename == NULL) || (*filename == '\0'))
    {
      (void) fprintf (stderr, "%s no config file specified.\n", func);
      return ((FILE *) NULL);
    }

  fp = fopen (filename, "r");
  if (fp == (FILE *) NULL)
    {
      (void) fprintf (stderr, "%s unable to open config file %s\n",
		      func, filename);
      return ((FILE *) NULL);
    }

  return (fp);

}


/*
**  paramProcess()
**  process the named parameter file
**
**  Parameters:
**      filename    - the pathname of the file to be opened
**      style   - the style of the config file. it can be MS_STYLE, that is
**                parameter must follows by = and the value. If it is
**                NOT_MS_STYLE, then parameter does not follows by = or the
**                value.
**                  Example of MS_STYLE config file:
**                      [section]
**                          version = 2.4
**                      [foo]
**                          bar=hello
**                          foobar=world
**
**                  Example of NOT_MS_STYLE config file:
**                      [section]
**                           2.4
**                      [foo]
**                          hello=foo
**                          world
**
**      sfunc       - a pointer to a function that will be called when a 
**                    section name is discovered.
**      pfunc       - a pointer to a function that will be called when a
**                    parameter name/value are discovered
**
**  Return Values:
**      0   if successfully parsed
**      -1  if failed to open the file for reading
**      -2  GC_MALLOC failed
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
paramProcess (char *filename,
	      int style, int (*sfunc) (char *), int (*pfunc) (char *, char *))
{
  char *func = "params.c:paramProcess() -";

  int result;

  FILE *fp;

  /* open the conf file */
  fp = openConfFile (filename);
  if (fp == (FILE *) NULL)
    return (-1);

  if (bufr != NULL)
    result = Parse (fp, style, sfunc, pfunc);
  else
    {
      bsize = BUFR_INC;
      bufr = (char *) GC_MALLOC (bsize);
      if (bufr == NULL)
	{
	  (void) fprintf (stderr, "%s GC_MALLOC failed\n", func);
	  (void) fclose (fp);
	  return (-2);
	}
      result = Parse (fp, style, sfunc, pfunc);
      // free(bufr);
      bufr = NULL;
      bsize = 0;
    }
  (void) fclose (fp);
  if (result < 0)
    {
      (void) fprintf (stderr, "%s failed. error returned from Parse()\n",
		      func);
      return (-3);
    }

  return (0);
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

static int
Continuation (char *line, int pos)
{
  pos--;
  while ((pos >= 0) && isspace (line[pos]))
    pos--;

  return (((pos >= 0) && (line[pos] == '\\')) ? pos : -1);
}


/*
**  
**
**  Parameters:
**      fp      - open FILE pointer
**      style   - the style of the config file. it can be MS_STYLE, that is
**                parameter must follows by = and the value. If it is
**                NOT_MS_STYLE, then parameter does not follows by = or the
**                value.
**                  Example of MS_STYLE config file:
**                      [section]
**                          version = 2.4
**                      [foo]
**                          bar=hello
**                          foobar=world
**
**                  Example of NOT_MS_STYLE config file:
**                      [section]
**                           2.4
**                      [foo]
**                          hello
**                          world
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
Parse (FILE * fp,
       int style, int (*sfunc) (char *), int (*pfunc) (char *, char *))
{
  int c;

  c = eatWhitespace (fp);

  while ((c != EOF) && (c > 0))
    {

      switch (c)
	{
	case '\n':		/* blank line */
	  {
	    c = eatWhitespace (fp);
	    break;
	  }

	case ';':		/* comment line */
	case '#':
	  {
	    c = eatComment (fp);
	    break;
	  }

	case '[':		/* section header */
	  {
	    if (Section (fp, sfunc) < 0)
	      {
		return (-1);
	      }
	    c = eatWhitespace (fp);
	    break;
	  }

	case '\\':		/* bogus backslash */
	  {
	    c = eatWhitespace (fp);
	    break;
	  }

	default:		/* parameter line */
	  {
	    if (Parameter (fp, style, pfunc, c) < 0)
	      return (-1);
	    c = eatWhitespace (fp);
	  }
	}
    }

  return (0);
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
  int c, i, end;

  char *func = "params.c:Section() -";

  i = 0;
  end = 0;

  c = eatWhitespace (fp);	/* 
				 ** we've already got the '['. scan past initial
				 ** white space
				 */

  while ((c != EOF) && (c > 0))
    {
      if (i > (bsize - 2))
	{
	  bsize += BUFR_INC;
	  bufr = loc_GC_REALLOC (bufr, bsize);
	  if (bufr == NULL)
	    {
	      (void) fprintf (stderr, "%s GC_MALLOC failed\n", func);
	      return (-1);
	    }
	}

      switch (c)
	{
	case ']':		/* found the closing bracked */
	  {
	    bufr[end] = '\0';
	    if (end == 0)
	      {
		(void) fprintf (stderr, "%s empty section name\n", func);
		return (-1);
	      }

	    /*
	     ** register
	     ** regSection(&bufr);
	     */

	    if (sfunc (bufr) < 0)
	      {
		return (-1);
	      }
	    (void) eatComment (fp);
	    return (0);
	    break;
	  }

	case '\n':
	  {
	    i = Continuation (bufr, i);
	    if (i < 0)
	      {
		bufr[end] = '\0';
		(void) fprintf (stderr, "%s badly formed line in cfg file\n",
				func);

		return (-1);
	      }
	    end = ((i > 0) && (bufr[i - 1] == ' ')) ? (i - 1) : (i);
	    c = getc (fp);
	    break;
	  }
	default:
	  {
	    if (isspace (c))
	      {
		bufr[end] = ' ';
		i = end + 1;
		c = eatWhitespace (fp);
	      }
	    else
	      {
		bufr[i++] = c;
		end = i;
		c = getc (fp);
	      }
	    break;
	  }
	}
    }

  return (0);
}

/*
** expand a pointer to be a particular size
*/
static char *
loc_GC_REALLOC (char *p, int size)
{
  char *ret = NULL;

  if (size == 0)
    {
      if (p)
	{
	  // (void) free(p);
	  (void) fprintf (stderr, "loc_GC_REALLOC() asked for 0 bytes\n");
	  return (NULL);
	}
    }

  if (!p)
    ret = (char *) GC_MALLOC (size);
  else
    ret = (char *) GC_REALLOC (p, size);

  if (!ret)
    (void) fprintf (stderr,
		    "GC_MALLOC problem, failed to expand to %d bytes\n",
		    size);
  return (ret);
}


/*
**  register a section, it's really a convinient function for users. 
**  supposed to be called from section handling routine written by the
**  user
**
**  Parameters:
**  char **s
**
**  Return Values:
**  none
**
**  Limitations and Comments:
**  uses the staic variable cursec.
**
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-10-1998    first cut
*/


// static void
// regSection (char **s)
// {
//   if (cursec != (char *) NULL)
//     {
//       (void) fprintf (stderr, "Freeing cursec\n");
//       // (void) free(cursec);
//       cursec = (char *) NULL;
//     }
//   if (*s)
//     cursec = strdup (*s);
// }

/*
**  
**
**  Parameters:
**
**
**  Return Values:
**
**
**  Limitations and Comments:
**
**
**
**  Development History:
**      who                  when           why
**      ma_muquit@fccc.edu   Apr-10-1998    
*/


char *
getCurrentSection (void)
{
  return (cursec);
}
