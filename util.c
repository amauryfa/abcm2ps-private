/*  
 *  This file is part of abc2ps, Copyright (C) 1996,1997 Michael Methfessel
 *  Modified for abcm2ps, Copyright (C) 1998-2000 Jean-François Moine
 *  See file abc2ps.c for details.
 */

#include <stdio.h>
#include <stdlib.h>
#ifdef unix
#include <unistd.h>
#endif
#include <math.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>

#include "abcparse.h" 
#include "abc2ps.h" 

/*  low-level utilities  */

/* -- error warning -- */
void wng(char msg[],
	 char str[])
{
	printf("++++ %s%s\n", msg, str);
}

/* -- error exit -- */
void rx(char msg[],
	char str[])
{
	printf("\n++++ %s%s\n", msg, str);
	exit(1);
}

/* -- print message for internal error and maybe stop -- */
void bug(char msg[],
	 int fatal)
{
	printf("\n\nThis cannot happen!");
	if (msg[0] != '\0')
		printf("\nInternal error: %s.\n", msg);
	if (fatal) {
		printf("Emergency stop.\n\n");
		exit(1);
	}
	printf("Trying to continue...\n\n");
}

/* -- return random float between x1 and x2 -- */
float ranf(float x1,
	   float x2)
{
static int m = 259200;		/* generator constants */
static int a = 421;
static int c = 54773;
static int j = 1;		/* seed */

	j = (j * a + c) % m;
	return x1 + (x2 - x1) * (double) j / (double) m;
}

/* -- check for valid abbreviation -- */
int abbrev(char str[],
	   char ab[],
	   int nchar)
{
	int nc;

	nc = strlen(str);
	if (nc > strlen(ab))
		return 0;
	if (nc < nchar)
		nc = nchar;
	if (strncmp(str, ab, nc) != 0)
		return 0;
	return 1;
}

/* -- read a number with a unit -- */
float scan_u(char *str)
{
	float a;
	int nch;

	if (sscanf(str, "%f%n", &a, &nch) == 1) {
		if (str[nch] == '\0')
			return a * PT;
		if (!strncmp(str + nch, "cm", 2))
			return a * CM;
		if (!strncmp(str + nch , "in", 2))
			return a * IN;
		if (!strncmp(str + nch, "pt", 2))
			return a * PT;
	}
	printf("\n++++ Unknown unit \"%s\"\n", str);
	exit(3);
}

/* -- capitalize a string -- */
void cap_str(char *p)
{
	while (*p != '\0') {
		*p = toupper(*p);
		p++;
	}
}
